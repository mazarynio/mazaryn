-module(post_ipfs).
-author("Zaryn Technologies").
-export([
    remote_pin_post/1,remote_pin_post/2,remote_pin_post/3,unpin_post/1,unpin_post/2,check_pin_status/1,check_pin_status/2,list_post_pins/0,list_post_pins/1,
    get_post_pin_info/1,migrate_pins/2,set_pin_expiration/2,renew_pin/1,add_pin_tags/2,get_cluster_stats/0, pin_to_cluster/1, pin_to_cluster/2, get_pin_info/1,
    format_pin_result/3, format_pin_result/4, update_post_pin_info/2
]).
-include("../records.hrl").

remote_pin_post(PostID) ->
    remote_pin_post(PostID, #{}).

remote_pin_post(PostID, Options) when is_map(Options) ->
    remote_pin_post(PostID, undefined, Options);
remote_pin_post(PostID, Service) when is_list(Service) orelse is_binary(Service) ->
    remote_pin_post(PostID, Service, #{}).

%% @doc Pins a post's content and media to a specific IPFS cluster service with options
%% Options can include:
%% - name: Custom pin name (default: generated from PostID and Author)
%% - replication: Number of replicas (default: based on user verification status)
%% - tags: List of tags to associate with the pin
%% - expires_after: Time in seconds after which the pin will expire
%% - region: Preferred geographic region for pinning
%% - metadata: Additional metadata to store with the pin
remote_pin_post(PostID, Service, Options) when (is_list(PostID) orelse is_binary(PostID)),
                                              is_map(Options) ->
    Fun = fun() ->
        case mnesia:read({post, PostID}) of
            [Post] ->
                Author = Post#post.author,
                UserID = Post#post.user_id,
                BusinessID = Post#post.business_id,
                IPNS = Post#post.content,
                MediaCID = Post#post.media,
                
                case storage_quota:check_storage_quota(UserID, BusinessID) of
                    {ok, Quota} ->
                        PinName = maps:get(name, Options, generate_pin_name(PostID, Author)),
                        Tags = maps:get(tags, Options, []),
                        Region = maps:get(region, Options, undefined),
                        ExpiresAfter = maps:get(expires_after, Options, undefined),
                        Metadata = maps:get(metadata, Options, #{}),
                        StartTime = erlang:system_time(millisecond),

                        logger:info("Starting cluster pin operation for post ~p by ~p", [PostID, Author]),

                        PinOpts = build_pin_options(Options),
                        ContentPinResult = case resolve_and_pin_content(IPNS, Service, PinOpts) of
                            {ok, ContentResult} ->
                                {ok, ContentResult};
                            Error ->
                                logger:error("Failed to pin content: ~p", [Error]),
                                Error
                        end,

                        MediaPinResult = pin_media_if_exists(MediaCID, Service, PinOpts),

                        ContentCID = case ContentPinResult of
                            {ok, #{cid := CID}} -> CID;
                            _ -> undefined
                        end,

                        MediaCIDs = case MediaPinResult of
                            {ok, #{cid := MCID}} -> [MCID];
                            {ok, _} -> [];
                            _ -> []
                        end,

                        EndTime = erlang:system_time(millisecond),
                        ElapsedTime = EndTime - StartTime,
                        logger:info("Cluster pin operation completed in ~pms", [ElapsedTime]),

                        ContentSize = storage_quota:calculate_content_size(ContentCID),
                        MediaSize = storage_quota:calculate_media_size(MediaCIDs),
                        TotalSize = ContentSize + MediaSize,

                        spawn(fun() ->
                            update_pinning_metrics(PostID, Author, ElapsedTime)
                        end),

                        PinID = generate_pin_id(PostID, os:timestamp()),

                        PinInfo = #pin_info{
                            post_id = PostID,
                            pin_id = PinID,
                            pin_type = cluster,
                            pin_name = PinName,
                            content_cid = ContentCID,
                            media_cids = MediaCIDs,
                            replication = determine_replication_strategy(Post, Options),
                            service = Service,
                            pin_time = calendar:universal_time(),
                            tags = Tags,
                            region = Region,
                            expires_at = calculate_expiration(ExpiresAfter),
                            status = case ContentPinResult of
                                {ok, _} -> active;
                                _ -> failed
                            end,
                            last_checked = calendar:universal_time(),
                            verification_count = 1,
                            size_bytes = TotalSize,
                            metadata = Metadata#{
                                elapsed_time_ms => ElapsedTime,
                                content_details => case ContentPinResult of 
                                    {ok, #{details := Details}} -> Details;
                                    _ -> #{}
                                end,
                                media_details => case MediaPinResult of
                                    {ok, #{details := MDetails}} -> MDetails;
                                    _ -> #{}
                                end,
                                quota_tier => Quota#storage_quota.tier_level
                            }
                        },

                        spawn(fun() ->
                            store_pin_history(PinInfo)
                        end),

                        UpdateResult = storage_quota:update_storage_quota(Quota, TotalSize),
                        case UpdateResult of
                            {ok, _UpdatedQuota} ->
                                UpdatedPost = Post#post{pin_info = PinInfo},
                                mnesia:write(UpdatedPost),
                                format_pin_result(ContentPinResult, MediaPinResult, ElapsedTime, PinID);
                            _ ->
                                spawn(fun() ->
                                    rollback_pin_operation(ContentCID, MediaCIDs, Service)
                                end),
                                {error, {quota_update_failed, UpdateResult}}
                        end;
                    {error, quota_exceeded} ->
                        {error, {quota_exceeded, <<"Storage quota exceeded. Please upgrade your plan to continue pinning content.">>}};
                    {error, pin_limit_reached} ->
                        {error, {pin_limit_reached, <<"You have reached your pin limit. Please upgrade your plan to pin more content.">>}};
                    {error, Reason} ->
                        {error, {quota_check_failed, Reason}}
                end;
            [] ->
                logger:warning("Post not found for pinning: ~p", [PostID]),
                {error, post_not_found};
            Error ->
                logger:error("Database error: ~p", [Error]),
                Error
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} ->
            logger:error("Transaction failed: ~p", [Reason]),
            {error, {transaction_failed, Reason}}
    end.

%% @doc Rollback a pin operation if quota update fails
rollback_pin_operation(ContentCID, MediaCIDs, Service) ->
    % Unpin content
    case ContentCID of
        undefined -> ok;
        _ -> ipfs_cluster:unpin_from_cluster(ContentCID, Service)
    end,
    
    lists:foreach(
        fun(CID) ->
            ipfs_cluster:unpin_from_cluster(CID, Service)
        end,
        MediaCIDs
    ).

%% Helper function to generate a unique pin ID
generate_pin_id(PostID, Timestamp) ->
    Unique = integer_to_list(erlang:phash2({PostID, Timestamp})),
    list_to_binary("pin_" ++ Unique).

%% Helper function to store pin history for auditing
store_pin_history(PinInfo) ->
    logger:info("Storing pin history: ~p", [PinInfo]),
    ok.

%% Updated format_pin_result to include pin_id
format_pin_result(ContentPinResult, MediaPinResult, ElapsedTime, PinID) ->
    {ok, #{
        content => format_single_pin_result(ContentPinResult),
        media => format_single_pin_result(MediaPinResult),
        elapsed_time_ms => ElapsedTime,
        pin_id => PinID
    }}.

%% Function to format an individual pin result
format_single_pin_result({ok, Result}) ->
    Result;
format_single_pin_result(Error) ->
    #{
        status => failed,
        error => Error
    }.

unpin_post(PostID) ->
    unpin_post(PostID, undefined).

unpin_post(PostID, Service) ->
    Fun = fun() ->
        case mnesia:read({post, PostID}) of
            [Post] ->
                IPNS = Post#post.content,
                MediaCID = Post#post.media,
                StartTime = erlang:system_time(millisecond),

                logger:info("Starting cluster unpin operation for post ~p", [PostID]),

                ContentResult = case postdb:get_post_ipfs_by_ipns(IPNS) of
                    {ok, ContentCID} ->
                        ipfs_cluster:unpin_from_cluster(ContentCID);
                    ContentCID when is_list(ContentCID) orelse is_binary(ContentCID) ->
                        ipfs_cluster:unpin_from_cluster(ContentCID);
                    Error ->
                        logger:error("Failed to resolve IPNS for unpinning ~p: ~p", [IPNS, Error]),
                        {error, {ipns_resolution_failed, Error}}
                end,

                MediaResult = unpin_media_if_exists(MediaCID, Service),

                EndTime = erlang:system_time(millisecond),
                ElapsedTime = EndTime - StartTime,

                UpdatedPost = mark_pins_inactive(Post),
                mnesia:write(UpdatedPost),

                format_unpin_result(ContentResult, MediaResult, ElapsedTime);
            [] ->
                {error, post_not_found}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

%% @doc Checks the status of a post's pins in the IPFS cluster
check_pin_status(PostID) ->
    check_pin_status(PostID, undefined).

check_pin_status(PostID, Service) ->
    Fun = fun() ->
        case mnesia:read({post, PostID}) of
            [Post] ->
                IPNS = Post#post.content,
                MediaCID = Post#post.media,

                ContentStatus = case postdb:get_post_ipfs_by_ipns(IPNS) of
                    {ok, ContentCID} ->
                        get_pin_status(ContentCID, Service);
                    ContentCID when is_list(ContentCID) orelse is_binary(ContentCID) ->
                        get_pin_status(ContentCID, Service);
                    Error ->
                        {error, {ipns_resolution_failed, Error}}
                end,

                MediaStatus = case MediaCID of
                    undefined -> {ok, not_applicable};
                    null -> {ok, not_applicable};
                    "" -> {ok, not_applicable};
                    _ -> get_pin_status(MediaCID, Service)
                end,

                {ok, #{
                    post_id => PostID,
                    content_status => ContentStatus,
                    media_status => MediaStatus
                }};
            [] ->
                {error, post_not_found}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

%% @doc Lists all pinned posts, optionally filtered by user
list_post_pins() ->
    Fun = fun() ->
        MatchHead = #post{_ = '_', pin_info = '$1'},
        Guard = {'=/=', '$1', undefined},
        Result = ['$_'],
        MatchSpec = [{MatchHead, [Guard], Result}],

        Posts = mnesia:select(post, MatchSpec),
        format_pinned_posts(Posts)
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> {ok, Result};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

list_post_pins(UserID) ->
    Fun = fun() ->
        MatchHead = #post{user_id = UserID, pin_info = '$1', _ = '_'},
        Guard = {'=/=', '$1', undefined},
        Result = ['$_'],
        MatchSpec = [{MatchHead, [Guard], Result}],

        Posts = mnesia:select(post, MatchSpec),
        format_pinned_posts(Posts)
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> {ok, Result};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

%% @doc Get detailed pin information for a specific post
get_post_pin_info(PostID) ->
    Fun = fun() ->
        case mnesia:read({post, PostID}) of
            [Post] ->
                case Post#post.pin_info of
                    undefined -> {error, no_pin_info};
                    null -> {error, no_pin_info};
                    [] -> {error, no_pin_info};
                    PinInfo -> {ok, PinInfo}
                end;
            [] ->
                {error, post_not_found}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

%% @doc Migrate pins from one service to another
migrate_pins(FromService, ToService) ->
    Fun = fun() ->
        MatchHead = #post{pin_info = '$1', _ = '_'},
        Guard = {'=/=', '$1', undefined},
        Result = ['$_'],
        MatchSpec = [{MatchHead, [Guard], Result}],

        Posts = mnesia:select(post, MatchSpec),

        SourcePosts = lists:filter(
            fun(Post) ->
                PinInfo = Post#post.pin_info,
                lists:any(
                    fun(Info) ->
                        case is_map(Info) of
                            true ->
                                maps:get(service, Info, undefined) =:= FromService andalso
                                maps:get(status, Info, inactive) =:= active;
                            false -> false
                        end
                    end,
                    PinInfo
                )
            end,
            Posts
        ),

        Results = lists:map(
            fun(Post) ->
                PostID = Post#post.id,
                try
                    ExistingOptions = get_existing_pin_options(Post, FromService),
                    {PostID, remote_pin_post(PostID, ToService, ExistingOptions)}
                catch
                    _:Error -> {PostID, {error, Error}}
                end
            end,
            SourcePosts
        ),

        {ok, #{
            total_posts => length(SourcePosts),
            results => Results
        }}
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

%% @doc Set expiration for a post's pins
set_pin_expiration(PostID, ExpiresAfter) when is_integer(ExpiresAfter), ExpiresAfter > 0 ->
    Fun = fun() ->
        case mnesia:read({post, PostID}) of
            [Post] ->
                ExpiresAt = calculate_expiration(ExpiresAfter),
                UpdatedPinInfo = update_pin_expiration(Post#post.pin_info, ExpiresAt),
                UpdatedPost = Post#post{pin_info = UpdatedPinInfo},
                mnesia:write(UpdatedPost),
                {ok, #{post_id => PostID, expires_at => ExpiresAt}};
            [] ->
                {error, post_not_found}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

%% @doc Renew an expired or about-to-expire pin
renew_pin(PostID) ->
    Fun = fun() ->
        case mnesia:read({post, PostID}) of
            [Post] ->
                case has_active_pins(Post#post.pin_info) of
                    true ->
                        UpdatedPinInfo = reset_pin_expiration(Post#post.pin_info),
                        UpdatedPost = Post#post{pin_info = UpdatedPinInfo},
                        mnesia:write(UpdatedPost),
                        {ok, #{post_id => PostID, status => renewed}};
                    false ->
                        remote_pin_post(PostID)
                end;
            [] ->
                {error, post_not_found}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

%% @doc Add tags to a post's pins
add_pin_tags(PostID, NewTags) when is_list(NewTags) ->
    Fun = fun() ->
        case mnesia:read({post, PostID}) of
            [Post] ->
                UpdatedPinInfo = update_pin_tags(Post#post.pin_info, NewTags),
                UpdatedPost = Post#post{pin_info = UpdatedPinInfo},
                mnesia:write(UpdatedPost),
                {ok, #{post_id => PostID, tags => NewTags}};
            [] ->
                {error, post_not_found}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

%% @doc Get cluster stats including storage usage and pin counts
get_cluster_stats() ->
    case ipfs_cluster:list_allocations() of
        {ok, Allocations} ->
            analyze_cluster_allocations(Allocations);
        Error ->
            Error
    end.

%% @doc Resolves IPNS to CID and pins the content to the cluster
resolve_and_pin_content(IPNS, Service, Options) ->
    case postdb:get_post_ipfs_by_ipns(IPNS) of
        {ok, ContentCID} ->
            pin_to_cluster_with_options(ContentCID, Service, Options);
        ContentCID when is_list(ContentCID) orelse is_binary(ContentCID) ->
            pin_to_cluster_with_options(ContentCID, Service, Options);
        Error ->
            logger:error("Failed to resolve IPNS ~p: ~p", [IPNS, Error]),
            {error, {ipns_resolution_failed, Error}}
    end.

pin_to_cluster_with_options(CID, undefined, Options) ->
    case ipfs_cluster:pin_to_cluster(CID) of
        {ok, Result} ->
            {ok, #{cid => CID, status => pinned, details => Result, options => Options}};
        Error ->
            {error, {cluster_pin_failed, Error}}
    end;
pin_to_cluster_with_options(CID, _Service, Options) ->
    case ipfs_cluster:pin_to_cluster(CID) of
        {ok, Result} ->
            {ok, #{cid => CID, status => pinned, details => Result, options => Options}};
        Error ->
            {error, {cluster_pin_failed, Error}}
    end.

%% @doc Pins content to the default cluster
pin_to_cluster(CID) ->
    case ipfs_cluster:pin_to_cluster(CID) of
        {ok, Result} ->
            {ok, #{cid => CID, status => pinned, details => Result}};
        Error ->
            {error, {cluster_pin_failed, Error}}
    end.

pin_to_cluster(CID, undefined) ->
    pin_to_cluster(CID);
pin_to_cluster(CID, _Service) ->
    case ipfs_cluster:pin_to_cluster(CID) of
        {ok, Result} ->
            {ok, #{cid => CID, status => pinned, details => Result}};
        Error ->
            {error, {cluster_pin_failed, Error}}
    end.

%% @doc Unpins content from the cluster
unpin_from_cluster(CID, undefined) ->
    unpin_from_cluster(CID);
unpin_from_cluster(CID, _Service) ->
    case ipfs_cluster:unpin_from_cluster(CID) of
        {ok, Result} ->
            {ok, #{cid => CID, status => unpinned, details => Result}};
        Error ->
            {error, {cluster_unpin_failed, Error}}
    end.

unpin_from_cluster(CID) ->
    case ipfs_cluster:unpin_from_cluster(CID) of
        {ok, Result} ->
            {ok, #{cid => CID, status => unpinned, details => Result}};
        Error ->
            {error, {cluster_unpin_failed, Error}}
    end.

%% @doc Pins media CID if it exists and is valid
pin_media_if_exists(undefined, _, _) -> {ok, skip_media};
pin_media_if_exists(null, _, _) -> {ok, skip_media};
pin_media_if_exists("", _, _) -> {ok, skip_media};
pin_media_if_exists(MediaCID, Service, Options) when is_list(MediaCID) orelse is_binary(MediaCID) ->
    case pin_to_cluster_with_options(MediaCID, Service, Options) of
        {ok, Result} -> {ok, Result};
        Error ->
            logger:error("Failed to pin media ~p: ~p", [MediaCID, Error]),
            Error
    end;
pin_media_if_exists(Invalid, _, _) ->
    logger:error("Invalid media CID format: ~p", [Invalid]),
    {error, invalid_media_cid}.

unpin_media_if_exists(undefined, _) -> {ok, skip_media};
unpin_media_if_exists(null, _) -> {ok, skip_media};
unpin_media_if_exists("", _) -> {ok, skip_media};
unpin_media_if_exists(MediaCID, Service) when is_list(MediaCID) orelse is_binary(MediaCID) ->
    case unpin_from_cluster(MediaCID, Service) of
        {ok, Result} -> {ok, Result};
        Error ->
            logger:error("Failed to unpin media ~p: ~p", [MediaCID, Error]),
            Error
    end;
unpin_media_if_exists(Invalid, _) ->
    logger:error("Invalid media CID format: ~p", [Invalid]),
    {error, invalid_media_cid}.

get_pin_status(CID, undefined) ->
    get_pin_status(CID);
get_pin_status(CID, _Service) ->
    case ipfs_cluster:get_pin_status(CID) of
        {ok, Result} -> {ok, Result};
        Error -> Error
    end.

get_pin_status(CID) ->
    case ipfs_cluster:get_pin_status(CID) of
        {ok, Result} -> {ok, Result};
        Error -> Error
    end.

format_pin_result({ok, ContentResult}, {ok, MediaResult}, ElapsedTime) ->
    {ok, #{
        content => ContentResult,
        media => MediaResult,
        elapsed_time_ms => ElapsedTime
    }};
format_pin_result({error, ContentError}, _, _) ->
    {error, {content_pin_failed, ContentError}};
format_pin_result(_, {error, MediaError}, _) when MediaError =/= skip_media ->
    {error, {media_pin_failed, MediaError}};
format_pin_result(ContentResult, MediaResult, ElapsedTime) ->
    {ok, #{
        content => ContentResult,
        media => MediaResult,
        elapsed_time_ms => ElapsedTime
    }}.

format_unpin_result({ok, ContentResult}, {ok, MediaResult}, ElapsedTime) ->
    {ok, #{
        content => ContentResult,
        media => MediaResult,
        elapsed_time_ms => ElapsedTime
    }};
format_unpin_result({error, ContentError}, _, _) ->
    {error, {content_unpin_failed, ContentError}};
format_unpin_result(_, {error, MediaError}, _) when MediaError =/= skip_media ->
    {error, {media_unpin_failed, MediaError}};
format_unpin_result(ContentResult, MediaResult, ElapsedTime) ->
    {ok, #{
        content => ContentResult,
        media => MediaResult,
        elapsed_time_ms => ElapsedTime
    }}.

%% @doc Format pinned posts for listing
format_pinned_posts(Posts) ->
    lists:map(
        fun(Post) ->
            #{
                post_id => Post#post.id,
                author => Post#post.author,
                created_at => Post#post.date_created,
                pins => Post#post.pin_info
            }
        end,
        Posts
    ).

%% @doc Generate a pin name for the IPFS cluster based on post details
generate_pin_name(PostID, Author) ->
    lists:flatten(io_lib:format("post_~s_by_~s_~s",
                              [PostID,
                               Author,
                               format_date(calendar:universal_time())])).

format_date({{Year, Month, Day}, {Hour, Min, _}}) ->
    lists:flatten(io_lib:format("~4..0w~2..0w~2..0w_~2..0w~2..0w",
                              [Year, Month, Day, Hour, Min])).

determine_replication_strategy(Post, Options) ->
    case maps:get(replication, Options, undefined) of
        undefined ->
            case is_verified_user(Post#post.author) of
                true -> 3;
                false -> 1
            end;
        Strategy -> Strategy
    end.

is_verified_user(UserID) ->
    case mnesia:dirty_read({user, UserID}) of
        [User] -> User#user.verified =:= true;
        _ -> false
    end.

%% @doc Update metrics for pinning operations
update_pinning_metrics(PostID, Author, ElapsedTime) ->
    try
        case code:ensure_loaded(prometheus_histogram) of
            {module, _} ->
                prometheus_histogram:observe(pinning_operation_duration,
                                           #{operation => cluster_pin,
                                             author => Author},
                                           ElapsedTime),
                prometheus_counter:inc(pins_created, #{type => cluster});
            {error, _} ->
                case erlang:function_exported(mazaryn_metrics, record_pin_operation, 3) of
                    true ->
                        mazaryn_metrics:record_pin_operation(PostID, Author, ElapsedTime);
                    false ->
                        logger:info("Pin metrics: post=~p author=~p time=~pms",
                                   [PostID, Author, ElapsedTime])
                end
        end
    catch
        Class:Reason:Stacktrace ->
            logger:warning("Failed to update metrics: ~p:~p~n~p",
                          [Class, Reason, Stacktrace]),
            ok
    end.

update_post_pin_info(Post, PinInfo) ->
    CurrentPins = case Post#post.pin_info of
        undefined -> [];
        null -> [];
        Pins when is_list(Pins) -> Pins
    end,
    Post#post{pin_info = [PinInfo | CurrentPins]}.

%% @doc Mark all pins as inactive (after unpinning)
mark_pins_inactive(Post) ->
    PinInfo = case Post#post.pin_info of
        undefined -> [];
        null -> [];
        Pins when is_list(Pins) ->
            lists:map(
                fun(Pin) when is_map(Pin) ->
                    maps:put(status, inactive, Pin);
                   (Pin) -> Pin
                end,
                Pins
            )
    end,
    Post#post{pin_info = PinInfo}.

%% @doc Calculate expiration timestamp from duration in seconds
calculate_expiration(undefined) ->
    undefined;
calculate_expiration(SecondsFromNow) when is_integer(SecondsFromNow) ->
    Now = calendar:universal_time(),
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Now) + SecondsFromNow
    ).

%% @doc Update pin expiration for all active pins
update_pin_expiration(PinInfo, ExpiresAt) when is_list(PinInfo) ->
    lists:map(
        fun(Pin) when is_map(Pin) ->
            case maps:get(status, Pin, active) of
                active -> maps:put(expires_at, ExpiresAt, Pin);
                _ -> Pin
            end;
           (Pin) -> Pin
        end,
        PinInfo
    );
update_pin_expiration(undefined, _) -> [];
update_pin_expiration(null, _) -> [].

reset_pin_expiration(PinInfo) when is_list(PinInfo) ->
    lists:map(
        fun(Pin) when is_map(Pin) ->
            case maps:get(status, Pin, active) of
                active -> maps:remove(expires_at, Pin);
                _ -> Pin
            end;
           (Pin) -> Pin
        end,
        PinInfo
    );
reset_pin_expiration(undefined) -> [];
reset_pin_expiration(null) -> [].

%% @doc Update pin tags by adding new tags
update_pin_tags(PinInfo, NewTags) when is_list(PinInfo) ->
    lists:map(
        fun(Pin) when is_map(Pin) ->
            case maps:get(status, Pin, active) of
                active ->
                    CurrentTags = maps:get(tags, Pin, []),
                    UpdatedTags = lists:usort(CurrentTags ++ NewTags),
                    maps:put(tags, UpdatedTags, Pin);
                _ -> Pin
            end;
           (Pin) -> Pin
        end,
        PinInfo
    );
update_pin_tags(undefined, _) -> [];
update_pin_tags(null, _) -> [].

has_active_pins(PinInfo) when is_list(PinInfo) ->
    lists:any(
        fun(Pin) when is_map(Pin) ->
            maps:get(status, Pin, inactive) =:= active;
           (_) -> false
        end,
        PinInfo
    );
has_active_pins(_) -> false.

%% @doc Get existing pin options from post to preserve settings during migration
get_existing_pin_options(Post, SourceService) ->
    PinInfo = Post#post.pin_info,
    SourcePinInfo = lists:filter(
        fun(Info) ->
            is_map(Info) andalso
            maps:get(service, Info, undefined) =:= SourceService andalso
            maps:get(status, Info, inactive) =:= active
        end,
        PinInfo
    ),

    case SourcePinInfo of
        [FirstPin|_] ->
            #{
                name => maps:get(name, FirstPin, undefined),
                replication => maps:get(replication, FirstPin, undefined),
                tags => maps:get(tags, FirstPin, []),
                expires_after => case maps:get(expires_at, FirstPin, undefined) of
                    undefined -> undefined;
                    ExpiresAt ->
                        Now = calendar:universal_time(),
                        NowSecs = calendar:datetime_to_gregorian_seconds(Now),
                        ExpireSecs = calendar:datetime_to_gregorian_seconds(ExpiresAt),
                        max(0, ExpireSecs - NowSecs)
                end
            };
        [] -> #{}
    end.

%% @doc Build pin options for cluster API
build_pin_options(Options) ->
    maps:fold(
        fun(replication, Value, Acc) ->
                maps:put(replication_factor, Value, Acc);
           (region, Value, Acc) ->
                maps:put(pinning_region, Value, Acc);
           (name, Value, Acc) ->
                maps:put(name, Value, Acc);
           (tags, Value, Acc) ->
                maps:put(tags, Value, Acc);
           (_, _, Acc) -> Acc
        end,
        #{},
        Options
    ).

%% @doc Analyze cluster allocations to get statistics
analyze_cluster_allocations(Allocations) when is_map(Allocations) ->
    Pins = maps:get(pins, Allocations, []),

    TotalPins = length(Pins),

    {TotalSize, SizeByType} = calculate_storage_usage(Pins),

    PinsByStatus = count_pins_by_status(Pins),

    {ok, #{
        total_pins => TotalPins,
        total_storage_bytes => TotalSize,
        storage_by_type => SizeByType,
        pins_by_status => PinsByStatus,
        allocation_by_peer => count_allocations_by_peer(Pins),
        healthy_pins_percent => calculate_health_percentage(Pins)
    }};
analyze_cluster_allocations(_) ->
    {error, invalid_allocation_data}.

%% @doc Calculate storage used by pins
calculate_storage_usage(Pins) ->
    {TotalSize, TypeSizes} = lists:foldl(
        fun(Pin, {AccSize, AccTypes}) ->
            Size = maps:get(size, Pin, 0),
            Type = determine_content_type(Pin),
            TypeSize = maps:get(Type, AccTypes, 0),
            {AccSize + Size, maps:put(Type, TypeSize + Size, AccTypes)}
        end,
        {0, #{}},
        Pins
    ),
    {TotalSize, TypeSizes}.

%% @doc Determine content type based on pin metadata
determine_content_type(Pin) ->
    case maps:get(name, Pin, "") of
        "post_" ++ _ -> post;
        "media_" ++ _ -> media;
        "profile_" ++ _ -> profile;
        "comment_" ++ _ -> comment;
        _ ->
            Tags = maps:get(tags, Pin, []),
            determine_type_from_tags(Tags)
    end.

determine_type_from_tags(Tags) ->
    case lists:member("post", Tags) of
        true -> post;
        false ->
            case lists:member("media", Tags) of
                true -> media;
                false ->
                    case lists:member("profile", Tags) of
                        true -> profile;
                        false ->
                            case lists:member("comment", Tags) of
                                true -> comment;
                                false -> other
                            end
                    end
            end
    end.

count_pins_by_status(Pins) ->
    lists:foldl(
        fun(Pin, Acc) ->
            Status = maps:get(status, Pin, "unknown"),
            Count = maps:get(Status, Acc, 0),
            maps:put(Status, Count + 1, Acc)
        end,
        #{},
        Pins
    ).

%% @doc Count allocations by peer
count_allocations_by_peer(Pins) ->
    lists:foldl(
        fun(Pin, PeerAcc) ->
            Allocations = maps:get(allocations, Pin, []),
            lists:foldl(
                fun(Allocation, Acc) ->
                    PeerID = maps:get(peer, Allocation, "unknown"),
                    Count = maps:get(PeerID, Acc, 0),
                    maps:put(PeerID, Count + 1, Acc)
                end,
                PeerAcc,
                Allocations
            )
        end,
        #{},
        Pins
    ).

%% @doc Calculate percentage of pins that are healthy (fully allocated)
calculate_health_percentage(Pins) ->
    {HealthyCount, Total} = lists:foldl(
        fun(Pin, {Healthy, Count}) ->
            ReplicationFactor = maps:get(replication_factor, Pin, 0),
            Allocations = maps:get(allocations, Pin, []),

            PinnedCount = length([A || A <- Allocations, maps:get(status, A, "") =:= "pinned"]),

            case PinnedCount >= ReplicationFactor of
                true -> {Healthy + 1, Count + 1};
                false -> {Healthy, Count + 1}
            end
        end,
        {0, 0},
        Pins
    ),

    case Total of
        0 -> 100.0;  
        _ -> (HealthyCount / Total) * 100.0
    end.

get_pin_info(PostID) when is_list(PostID) orelse is_binary(PostID) ->
    Fun = fun() ->
        case mnesia:read({post, PostID}) of
            [Post] ->
                case Post#post.pin_info of
                    undefined ->
                        {error, no_pin_info};
                    PinInfo ->
                        {ok, PinInfo}
                end;
            [] ->
                {error, post_not_found};
            Error ->
                {error, {db_error, Error}}
        end
    end,
    
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.
