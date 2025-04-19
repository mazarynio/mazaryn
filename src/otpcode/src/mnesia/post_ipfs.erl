-module(post_ipfs).
-author("Zaryn Technologies").
-export([
    check_pin_status/1,check_pin_status/2,list_post_pins/0,list_post_pins/1,
    get_post_pin_info/1,migrate_pins/2,set_pin_expiration/2,renew_pin/1,add_pin_tags/2,get_cluster_stats/0, pin_to_cluster/1, pin_to_cluster/2, get_pin_info/1,
    format_pin_result/3, update_post_pin_info/2
]).
-include("../records.hrl").

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
                    {PostID, pin_post:remote_pin_post(PostID, ToService, ExistingOptions)}
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
                ExpiresAt = pin_post:calculate_expiration(ExpiresAfter),
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
                        pin_post:remote_pin_post(PostID)
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

update_post_pin_info(Post, PinInfo) ->
    CurrentPins = case Post#post.pin_info of
        undefined -> [];
        null -> [];
        Pins when is_list(Pins) -> Pins
    end,
    Post#post{pin_info = [PinInfo | CurrentPins]}.


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
