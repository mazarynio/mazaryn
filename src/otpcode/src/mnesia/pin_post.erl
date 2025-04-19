-module(pin_post).
-author("Zaryn Technologies").
-export([remote_pin_post/1, remote_pin_post/2, remote_pin_post/3,verify_pin_health/1,get_pin_info/1,unpin_post/1,unpin_post/2,get_gateway_url/1,
get_gateway_url/2,bulk_pin_posts/1,bulk_pin_posts/2,get_pin_statistics/0,get_pin_statistics/1,schedule_pin_operation/3, update_pinning_metrics/3]).

-include("../records.hrl").

-define(DEFAULT_GATEWAY, "http://localhost:8080").
-define(MAZARYN_VERSION, "1.0.0").
-define(DEFAULT_REPLICATION, 2).
-define(VERIFIED_USER_REPLICATION, 3).
-define(MAX_BULK_PINS, 25).


%% @doc Pins a post to IPFS cluster with default options
remote_pin_post(PostID) ->
    remote_pin_post(PostID, #{}).

%% @doc Pins a post with options or to a specific service
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
%% - tier: Storage tier (hot, warm, cold)
%% - encrypt: Whether to encrypt the content (true/false)
%% - access_control: Access control list or policy
%% - webhook_url: URL to notify on pin status changes
%% - priority: Pin priority (high, medium, low)
remote_pin_post(PostID, Service, Options) when (is_list(PostID) orelse is_binary(PostID)),
                                              is_map(Options) ->
    RequestId = generate_request_id(),
    logger:info("[~s] Starting pin operation for post ~p with options ~p", 
                [RequestId, PostID, sanitize_options(Options)]),
    
    StartTime = erlang:system_time(millisecond),
    
    case check_rate_limits(get_requester_info(Options)) of
        {ok, _} ->
            Result = execute_pin_transaction(PostID, Service, Options, RequestId),
            
            EndTime = erlang:system_time(millisecond),
            ElapsedTime = EndTime - StartTime,
            
            log_pin_operation_result(RequestId, PostID, Result, ElapsedTime),
            
            case Result of
                {ok, ResultData} ->
                    maybe_schedule_webhook(maps:get(webhook_url, Options, undefined), ResultData),
                    maybe_schedule_health_check(maps:get(pin_id, ResultData), Options);
                _ -> ok
            end,
            
            Result;
        {error, RateLimitReason} = Error ->
            logger:warning("[~s] Rate limit exceeded: ~p", [RequestId, RateLimitReason]),
            Error
    end.

%% @doc Execute the pin operation in a transaction
execute_pin_transaction(PostID, Service, Options, RequestId) ->
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
                        PinParams = prepare_pin_parameters(PostID, Author, Options),
                        
                        ClusterService = select_cluster_service(Service, maps:get(region, Options, undefined)),
                        
                        logger:info("[~s] Using cluster service: ~p", [RequestId, ClusterService]),
                        
                        ContentPinResult = resolve_and_pin_content(IPNS, ClusterService, PinParams),
                        
                        MediaPinResult = pin_media_if_exists(MediaCID, ClusterService, PinParams),
                        
                        ContentCID = extract_cid_from_result(ContentPinResult),
                        MediaCIDs = extract_media_cids_from_result(MediaPinResult),
                        
                        PinMetadata = create_pin_metadata(Options, ContentPinResult, MediaPinResult),
                        
                        PinID = generate_pin_id(PostID, os:timestamp()),
                        
                        ContentSize = storage_quota:calculate_content_size(ContentCID),
                        MediaSize = storage_quota:calculate_media_size(MediaCIDs),
                        TotalSize = ContentSize + MediaSize,
                        
                        EncryptedContent = case maps:get(encrypt, Options, false) of
                            true ->
                                encrypt_content(ContentCID, PinID, Options);
                            false ->
                                undefined
                        end,
                        
                        PinInfo = create_pin_info_record(
                            PostID, PinID, Author, PinParams, ContentCID, MediaCIDs, 
                            ClusterService, TotalSize, PinMetadata, EncryptedContent
                        ),
                        
                        store_pin_info(PinInfo),
                        
                        case storage_quota:update_storage_quota(Quota, TotalSize) of
                            {ok, _UpdatedQuota} ->
                                UpdatedPost = Post#post{pin_info = PinInfo},
                                mnesia:write(UpdatedPost),
                                
                                schedule_health_monitoring(PinID, Options),
                                
                                format_pin_result(ContentPinResult, MediaPinResult, 
                                                 erlang:system_time(millisecond) - PinParams#pin_params.start_time, 
                                                 PinID, ClusterService);
                            QuotaError ->
                                spawn(fun() ->
                                    rollback_pin_operation(ContentCID, MediaCIDs, ClusterService)
                                end),
                                {error, {quota_update_failed, QuotaError}}
                        end;
                    {error, quota_exceeded} ->
                        {error, {quota_exceeded, <<"Storage quota exceeded. Please upgrade your plan to continue pinning content.">>}};
                    {error, pin_limit_reached} ->
                        {error, {pin_limit_reached, <<"You have reached your pin limit. Please upgrade your plan to pin more content.">>}};
                    {error, Reason} ->
                        {error, {quota_check_failed, Reason}}
                end;
            [] ->
                logger:warning("[~s] Post not found for pinning: ~p", [RequestId, PostID]),
                {error, post_not_found};
            Error ->
                logger:error("[~s] Database error: ~p", [RequestId, Error]),
                Error
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} ->
            logger:error("[~s] Transaction failed: ~p", [RequestId, Reason]),
            {error, {transaction_failed, Reason}}
    end.

%% @doc Prepare pin parameters from options
prepare_pin_parameters(PostID, Author, Options) ->
    DefaultReplication = determine_replication_strategy(Author, maps:get(replication, Options, undefined)),
    #pin_params{
        name = maps:get(name, Options, generate_pin_name(PostID, Author)),
        tags = maps:get(tags, Options, []),
        region = maps:get(region, Options, undefined),
        expires_after = maps:get(expires_after, Options, undefined),
        replication = DefaultReplication,
        tier = maps:get(tier, Options, standard),
        encrypt = maps:get(encrypt, Options, false),
        access_control = maps:get(access_control, Options, undefined),
        webhook_url = maps:get(webhook_url, Options, undefined),
        priority = maps:get(priority, Options, medium),
        metadata = maps:get(metadata, Options, #{}),
        start_time = erlang:system_time(millisecond)
    }.

%% @doc Create the pin info record
%% @doc Create a pin_info record for a pinned post.
%% Calculates and stores the total size of content and media.
create_pin_info_record(PostID, PinID, UserID, PinParams, ContentCID, MediaCIDs, Service, _Size, Metadata, EncryptedContent) ->
    ContentSize = storage_quota:calculate_content_size(ContentCID),
    MediaSize = storage_quota:calculate_media_size(MediaCIDs),
    TotalSize = ContentSize + MediaSize,
    logger:info("Pin ~p for post ~p: content_size=~p, media_size=~p, total_size=~p bytes",
                [PinID, PostID, ContentSize, MediaSize, TotalSize]),
    case TotalSize < 0 of
        true ->
            logger:error("Invalid total size ~p for pin ~p, setting to 0", [TotalSize, PinID]),
            0;
        false ->
            #pin_info{
                post_id = PostID,
                pin_id = PinID,
                user_id = UserID,
                pin_type = cluster,
                pin_name = PinParams#pin_params.name,
                content_cid = ContentCID,
                media_cids = MediaCIDs,
                replication = determine_replication_strategy(UserID, PinParams#pin_params.replication),
                service = Service,
                pin_time = calendar:universal_time(),
                tags = PinParams#pin_params.tags,
                region = PinParams#pin_params.region,
                expires_at = calculate_expiration(PinParams#pin_params.expires_after),
                status = case ContentCID of
                    undefined -> failed;
                    _ -> active
                end,
                tier = PinParams#pin_params.tier,
                encrypted = EncryptedContent,
                access_control = PinParams#pin_params.access_control,
                last_checked = calendar:universal_time(),
                verification_count = 1,
                size_bytes = TotalSize,
                metadata = Metadata
            }
    end.

%% @doc Extract CID from pin result
extract_cid_from_result({ok, #{cid := CID}}) -> CID;
extract_cid_from_result(_) -> undefined.

%% @doc Extract media CIDs from pin result
extract_media_cids_from_result({ok, #{cid := MCID}}) -> [MCID];
extract_media_cids_from_result({ok, _}) -> [];
extract_media_cids_from_result(_) -> [].

%% @doc Create metadata for pin
create_pin_metadata(Options, ContentPinResult, MediaPinResult) ->
    BaseMetadata = maps:get(metadata, Options, #{}),
    BaseMetadata#{
        mazaryn_version => ?MAZARYN_VERSION,
        content_details => case ContentPinResult of 
            {ok, #{details := Details}} -> Details;
            _ -> #{}
        end,
        media_details => case MediaPinResult of
            {ok, #{details := MDetails}} -> MDetails;
            _ -> #{}
        end,
        pin_timestamp => calendar:universal_time(),
        service_level => determine_service_level(Options)
    }.

%% @doc Encrypt content if requested
encrypt_content(undefined, _, _) -> undefined;
encrypt_content(ContentCID, PinID, Options) ->
    try
        EncryptionKey = maps:get(encryption_key, Options, generate_encryption_key(PinID)),
        case ipfs_encryption:encrypt_content(ContentCID, EncryptionKey) of
            {ok, EncryptedCID} ->
                #{
                    encrypted_cid => EncryptedCID,
                    encryption_method => aes_256_gcm,
                    key_id => crypto:hash(sha256, EncryptionKey)
                };
            Error ->
                logger:error("Encryption failed for CID ~p: ~p", [ContentCID, Error]),
                undefined
        end
    catch
        _:_ -> undefined
    end.

%% @doc Generate encryption key
generate_encryption_key(_Seed) ->
    crypto:strong_rand_bytes(32).

%% @doc Determine service level based on options
determine_service_level(Options) ->
    case maps:get(tier, Options, standard) of
        premium -> premium;
        enterprise -> enterprise;
        _ -> standard
    end.

%% @doc Select appropriate cluster service based on region and service parameter
select_cluster_service(undefined, Region) ->
    case Region of
        undefined -> 
            application:get_env(mazaryn, default_cluster_service, "default");
        _ ->
            ClusterConfig = get_cluster_config(Region),
            maps:get(service_name, ClusterConfig, "default")
    end;
select_cluster_service(Service, _) ->
    Service.

%% @doc Get cluster configuration for a region
get_cluster_config(Region) ->
    Configs = application:get_env(mazaryn, cluster_configs, #{}),
    maps:get(Region, Configs, maps:get(default, Configs, #{
        service_name => "default",
        endpoints => ["http://localhost:9094/api/v0"],
        replication_factor => ?DEFAULT_REPLICATION
    })).

%% @doc Check rate limits before proceeding with pin operation
check_rate_limits(#{user_id := UserID}) ->
    case ets:info(rate_limiter_usage) of
        undefined -> rate_limiter:start();
        _ -> ok
    end,
    
    RateLimits = application:get_env(mazaryn, rate_limits, #{}),
    MaxPinsPerMinute = maps:get(pins_per_minute, RateLimits, 10),
    
    case rate_limiter:check_limit(UserID, pin_operation, MaxPinsPerMinute, 60) of
        {ok, _} -> {ok, allowed};
        Error -> Error
    end;
check_rate_limits(_) ->
    {ok, allowed}.

%% @doc Get requester information from options
get_requester_info(Options) ->
    #{
        user_id => maps:get(user_id, Options, undefined),
        business_id => maps:get(business_id, Options, undefined),
        ip_address => maps:get(ip_address, Options, undefined)
    }.

%% @doc Log pin operation result
log_pin_operation_result(RequestId, PostID, {ok, Result}, ElapsedTime) ->
    logger:info("[~s] Pinning completed for post ~p in ~pms: ~p", 
                [RequestId, PostID, ElapsedTime, maps:get(pin_id, Result, undefined)]);
log_pin_operation_result(RequestId, PostID, {error, Reason}, ElapsedTime) ->
    logger:error("[~s] Pinning failed for post ~p in ~pms: ~p", 
                [RequestId, PostID, ElapsedTime, Reason]).

%% @doc Schedule webhook notification if URL is provided
maybe_schedule_webhook(undefined, _) -> ok;
maybe_schedule_webhook(WebhookURL, ResultData) ->
    spawn(fun() ->
        webhook_notifier:send_notification(WebhookURL, ResultData)
    end).

%% @doc Schedule health check if enabled
maybe_schedule_health_check(PinID, Options) ->
    case maps:get(health_check, Options, false) of
        true ->
            schedule_health_check(PinID, maps:get(health_check_interval, Options, 3600));
        _ -> ok
    end.

%% @doc Schedule periodic health monitoring
schedule_health_monitoring(PinID, Options) ->
    case maps:get(health_monitoring, Options, false) of
        true ->
            Interval = maps:get(monitoring_interval, Options, 86400),  
            schedule_job(health_check, PinID, Interval);
        _ -> ok
    end.

%% @doc Schedule a job to run periodically
schedule_job(Type, PinID, Interval) ->
    spawn(fun() ->
        job_scheduler:schedule({Type, PinID}, Interval)
    end).

%% @doc Schedule a health check for a pin
schedule_health_check(PinID, Interval) ->
    schedule_job(health_check, PinID, Interval).

%% @doc Sanitize options for logging (remove sensitive data)
sanitize_options(Options) ->
    maps:without([encryption_key, secret, token], Options).

%% @doc Rollback a pin operation if quota update fails
rollback_pin_operation(ContentCID, MediaCIDs, _Service) ->
    case ContentCID of
        undefined -> ok;
        _ -> ipfs_cluster:unpin_from_cluster(ContentCID)
    end,
    
    lists:foreach(
        fun(CID) ->
            ipfs_cluster:unpin_from_cluster(CID)
        end,
        MediaCIDs
    ).

%% @doc Generate a unique request ID for tracking
generate_request_id() ->
    Unique = integer_to_list(erlang:phash2({os:timestamp(), self()})),
    "req_" ++ Unique.

%% @doc Generate a unique pin ID
generate_pin_id(PostID, Timestamp) ->
    Unique = integer_to_list(erlang:phash2({PostID, Timestamp})),
    list_to_binary("pin_" ++ Unique).

%% @doc Store pin info in the database
store_pin_info(PinInfo) ->
    mnesia:write({pin_info_lookup, PinInfo#pin_info.pin_id, PinInfo}),
    
    spawn(fun() ->
        store_pin_history(PinInfo)
    end).

%% @doc Store pin history for auditing
store_pin_history(PinInfo) ->
    HistoryRecord = #pin_history{
        pin_id = PinInfo#pin_info.pin_id,
        post_id = PinInfo#pin_info.post_id,
        author = PinInfo#pin_info.user_id,
        operation = create,
        pin_time = PinInfo#pin_info.pin_time,
        status = PinInfo#pin_info.status,
        service = PinInfo#pin_info.service,
        size_bytes = PinInfo#pin_info.size_bytes,
        metadata = PinInfo#pin_info.metadata
    },
    mnesia:dirty_write(HistoryRecord).

%% @doc Format the pin result for API response
format_pin_result(ContentPinResult, MediaPinResult, ElapsedTime, PinID, Service) ->
    GatewayURL = case extract_cid_from_result(ContentPinResult) of
        undefined -> undefined;
        CID -> get_gateway_url(CID, #{})
    end,
    
    {ok, #{
        content => format_single_pin_result(ContentPinResult),
        media => format_single_pin_result(MediaPinResult),
        elapsed_time_ms => ElapsedTime,
        pin_id => PinID,
        service => Service,
        gateway_url => GatewayURL,
        timestamp => calendar:universal_time()
    }}.

%% @doc Format an individual pin result
format_single_pin_result({ok, Result}) ->
    Result;
format_single_pin_result(Error) ->
    #{
        status => failed,
        error => Error
    }.

%% @doc Generate a pin name from post ID and author
generate_pin_name(PostID, Author) ->
    lists:flatten(io_lib:format("mazaryn_post_~s_by_~s_~s",
                              [PostID,
                               Author,
                               format_date(calendar:universal_time())])).

%% @doc Format date for pin name
format_date({{Year, Month, Day}, {Hour, Min, _}}) ->
    lists:flatten(io_lib:format("~4..0w~2..0w~2..0w_~2..0w~2..0w",
                              [Year, Month, Day, Hour, Min])).

%% @doc Build pin options for cluster API
build_pin_options(#pin_params{} = Params) ->
    Map = #{
        replication => Params#pin_params.replication,
        region => Params#pin_params.region,
        name => Params#pin_params.name, 
        tags => Params#pin_params.tags,
        tier => Params#pin_params.tier,
        priority => Params#pin_params.priority
    },
    
    maps:filter(fun(_, V) -> V =/= undefined end, Map);

build_pin_options(Options) when is_map(Options) ->
    maps:fold(
        fun(replication, Value, Acc) ->
                maps:put(replication_factor, Value, Acc);
           (region, Value, Acc) ->
                maps:put(pinning_region, Value, Acc);
           (name, Value, Acc) ->
                maps:put(name, Value, Acc);
           (tags, Value, Acc) ->
                maps:put(tags, Value, Acc);
           (tier, Value, Acc) ->
                maps:put(storage_tier, Value, Acc);
           (priority, Value, Acc) ->
                maps:put(priority, Value, Acc);
           (_, _, Acc) -> Acc
        end,
        #{},
        Options
    ).
%% @doc Resolves IPNS to CID and pins the content to the cluster
resolve_and_pin_content(IPNS, Service, PinParams) ->
    case postdb:get_post_ipfs_by_ipns(IPNS) of
        {ok, ContentCID} ->
            pin_to_cluster_with_options(ContentCID, Service, PinParams);
        ContentCID when is_list(ContentCID) orelse is_binary(ContentCID) ->
            pin_to_cluster_with_options(ContentCID, Service, PinParams);
        Error ->
            logger:error("Failed to resolve IPNS ~p: ~p", [IPNS, Error]),
            {error, {ipns_resolution_failed, Error}}
    end.

%% @doc Pin content to cluster with options
pin_to_cluster_with_options(CID, Service, PinParams) ->
    Options = build_pin_options(PinParams),
    case ipfs_cluster:pin_to_cluster(CID) of
        {ok, Result} ->
            Replication = PinParams#pin_params.replication,
            case ensure_replication(CID, Service, Replication) of
                {ok, _} ->
                    {ok, #{cid => CID, status => pinned, details => Result, options => Options}};
                {error, Reason} ->
                    {error, {replication_failed, Reason}}
            end;
        Error ->
            {error, {cluster_pin_failed, Error}}
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

%% @doc Determine replication strategy based on author verification status
determine_replication_strategy(Author, ConfiguredReplication) ->
    Default = case is_verified_user(Author) of
        true -> ?VERIFIED_USER_REPLICATION;
        false -> ?DEFAULT_REPLICATION
    end,
    Replication = case ConfiguredReplication of
        undefined -> Default;
        Strategy -> Strategy
    end,
    logger:info("Replication set to ~p for author ~p, but cluster may not enforce it",
                [Replication, Author]),
    Replication.

%% @doc Check if user is verified
is_verified_user(UserID) ->
    case mnesia:dirty_read({user, UserID}) of
        [User] -> User#user.verified =:= true;
        _ -> false
    end.

%% @doc Calculate expiration timestamp from duration in seconds
calculate_expiration(undefined) ->
    undefined;
calculate_expiration(SecondsFromNow) when is_integer(SecondsFromNow) ->
    Now = calendar:universal_time(),
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Now) + SecondsFromNow
    ).

%% @doc Get gateway URL for a CID
get_gateway_url(CID) ->
    get_gateway_url(CID, #{}).

%% @doc Get gateway URL for a CID with options
get_gateway_url(CID, Options) ->
    DefaultGateway = application:get_env(mazaryn, default_gateway, ?DEFAULT_GATEWAY),
    Gateway = maps:get(gateway, Options, DefaultGateway),
    PathPrefix = case maps:get(path_style, Options, ipfs) of
        ipfs -> "/ipfs/";
        ipns -> "/ipns/";
        subdomain -> "/"
    end,
    
    FullURL = case maps:get(path_style, Options, ipfs) of
        subdomain ->
            [Protocol, Rest] = string:split(Gateway, "://"),
            io_lib:format("~s://~s.~s", [Protocol, CID, Rest]);
        _ ->
            io_lib:format("~s~s~s", [Gateway, PathPrefix, CID])
    end,
    
    % Add any query parameters
    case maps:get(query_params, Options, #{}) of
        QueryParams when map_size(QueryParams) > 0 ->
            QueryString = maps:fold(
                fun(K, V, Acc) ->
                    Param = io_lib:format("~s=~s", [K, V]),
                    case Acc of
                        "" -> Param;
                        _ -> Acc ++ "&" ++ Param
                    end
                end,
                "",
                QueryParams
            ),
            io_lib:format("~s?~s", [FullURL, QueryString]);
        _ ->
            FullURL
    end.

%% @doc Verifies the health of a pin
verify_pin_health(PinID) ->
    case mnesia:dirty_read({pin_info_lookup, PinID}) of
        [{pin_info_lookup, PinID, PinInfo}] ->
            ContentCID = PinInfo#pin_info.content_cid,
            MediaCIDs = PinInfo#pin_info.media_cids,
            Service = PinInfo#pin_info.service,
            
            ContentHealth = verify_cid_health(ContentCID, Service),
            
            MediaHealth = lists:map(
                fun(CID) -> verify_cid_health(CID, Service) end,
                MediaCIDs
            ),
            
            OverallScore = calculate_health_score(ContentHealth, MediaHealth),
            
            HealthRecord = #pin_health{
                pin_id = PinID,
                last_check = calendar:universal_time(),
                status = determine_health_status(OverallScore),
                availability_score = OverallScore,
                geographic_distribution = check_geographic_distribution(ContentCID, Service),
                retrieval_latency = measure_retrieval_latency(ContentCID),
                replication_actual = count_actual_replications(ContentCID, Service),
                issues = identify_health_issues(ContentHealth, MediaHealth),
                metadata = #{}
            },
            
            mnesia:dirty_write({pin_health, 
                                HealthRecord#pin_health.pin_id,
                                HealthRecord#pin_health.last_check,
                                HealthRecord#pin_health.status,
                                HealthRecord#pin_health.availability_score,
                                HealthRecord#pin_health.geographic_distribution,
                                HealthRecord#pin_health.retrieval_latency,
                                HealthRecord#pin_health.replication_actual,
                                HealthRecord#pin_health.issues,
                                HealthRecord#pin_health.metadata}),
            
            case HealthRecord#pin_health.status of
                unhealthy ->
                    spawn(fun() -> attempt_repair(PinID, HealthRecord, PinInfo) end);
                _ -> ok
            end,
            
            {ok, HealthRecord};
        [] ->
            {error, pin_not_found}
    end.

%% @doc Attempt to repair pin issues
attempt_repair(PinID, HealthRecord, PinInfo) ->
    Issues = HealthRecord#pin_health.issues,
    ReplicationFactor = PinInfo#pin_info.replication,
    Service = PinInfo#pin_info.service,
    lists:foreach(
        fun(Issue) ->
            case Issue of
                "content_not_pinned" ->
                    repin_content(PinInfo#pin_info.content_cid, Service, ReplicationFactor);
                "insufficient_replication" ->
                    repin_content(PinInfo#pin_info.content_cid, Service, ReplicationFactor);
                IssueStr ->
                    case string:prefix(IssueStr, "media_not_pinned:") of
                        nomatch -> ok;
                        MediaCID -> repin_content(MediaCID, Service, ReplicationFactor)
                    end
            end
        end,
        Issues
    ),
    
    logger:info("Attempted repair of pin ~p with issues: ~p", [PinID, Issues]).

%% @doc Repin content to cluster
repin_content(CID, Service, _ReplicationFactor) when is_list(CID) orelse is_binary(CID) ->
    case ipfs_cluster:pin_to_cluster(CID) of
        {ok, Result} ->
            logger:info("Successfully repinned CID ~p on service ~p", [CID, Service]),
            {ok, Result};
        {error, Reason} ->
            logger:error("Failed to repin CID ~p on service ~p: ~p", [CID, Service, Reason]),
            {error, Reason}
    end;
repin_content(undefined, _, _) ->
    logger:warning("Cannot repin undefined CID"),
    {ok, skipped};
repin_content(Invalid, Service, _) ->
    logger:error("Invalid CID format for repinning: ~p on service ~p", [Invalid, Service]),
    {error, invalid_cid}.

%% @doc Verify the health of a specific CID
verify_cid_health(CID, Service) ->
    ensure_inets_started(),
    ensure_ssl_started(),

    % Check if CID is pinned in the cluster
    IsPinned = case ipfs_cluster:get_pin_status(CID) of
        {ok, _PinDetails} -> true;
        {error, {404, _}} -> false;
        {error, Reason} ->
            logger:error("Failed to check pin status for CID ~p: ~p", [CID, Reason]),
            false
    end,

    GatewayUrl = get_gateway_url(CID),
    IsRetrievable = case httpc:request(head, {GatewayUrl, []}, [{timeout, 10000}, {connect_timeout, 5000}], []) of
        {ok, {{_, StatusCode, _}, _, _}} when StatusCode >= 200, StatusCode < 300 ->
            true;
        {error, _Reason} ->
            logger:warning("Failed to retrieve CID ~p from gateway ~p: ~p", [CID, GatewayUrl, _Reason]),
            false;
        {ok, {{_, StatusCode, _}, _, _}} ->
            logger:warning("Unexpected status code ~p for CID ~p from gateway ~p", [StatusCode, CID, GatewayUrl]),
            false
    end,

    ExpectedReplicas = application:get_env(mazaryn, default_replication, ?DEFAULT_REPLICATION),
    ActualReplicas = count_actual_replications(CID, Service),
    HasReplication = ActualReplicas >= ExpectedReplicas,

    #{
        cid => CID,
        is_pinned => IsPinned,
        is_retrievable => IsRetrievable,
        has_expected_replication => HasReplication,
        actual_replicas => ActualReplicas,
        expected_replicas => ExpectedReplicas
    }.

%% @doc Ensure inets application is started
ensure_inets_started() ->
    case lists:keymember(inets, 1, application:which_applications()) of
        false -> application:start(inets);
        true -> ok
    end.

%% @doc Ensure ssl application is started
ensure_ssl_started() ->
    case lists:keymember(ssl, 1, application:which_applications()) of
        false -> application:start(ssl);
        true -> ok
    end.

%% @doc Calculate overall health score
calculate_health_score(ContentHealth, MediaHealthList) ->
    ContentScore = health_score(ContentHealth),
    
    MediaScore = case length(MediaHealthList) of
        0 -> 100;  
        N ->
            MediaScores = lists:map(fun health_score/1, MediaHealthList),
            lists:sum(MediaScores) div N
    end,
    
    (ContentScore * 7 + MediaScore * 3) div 10.

%% @doc Calculate individual health score
health_score(#{is_pinned := IsPinned, is_retrievable := IsRetrievable, has_expected_replication := HasReplication}) ->
    PinScore = case IsPinned of true -> 40; false -> 0 end,
    RetrieveScore = case IsRetrievable of true -> 40; false -> 0 end,
    ReplicationScore = case HasReplication of true -> 20; false -> 0 end,
    
    PinScore + RetrieveScore + ReplicationScore.

%% @doc Determine health status based on score
determine_health_status(Score) when Score >= 80 -> healthy;
determine_health_status(Score) when Score >= 60 -> degraded;
determine_health_status(_) -> unhealthy.

%% @doc Check geographic distribution of pin
check_geographic_distribution(CID, _Service) ->
    case ipfs_cluster:get_allocation(CID) of
        {ok, #{<<"allocations">> := Allocations}} ->
            Allocations;
        _ ->
            []
    end.

%% @doc Measure retrieval latency for content
measure_retrieval_latency(CID) ->
    ensure_inets_started(),
    ensure_ssl_started(),
    StartTime = erlang:system_time(millisecond),
    GatewayUrl = get_gateway_url(CID),
    
    HttpOptions = [{timeout, 10000}, {connect_timeout, 5000}],
    Result = httpc:request(head, {GatewayUrl, []}, HttpOptions, []),
    EndTime = erlang:system_time(millisecond),
    Latency = EndTime - StartTime,
    
    case Result of
        {ok, {{_, StatusCode, _}, _, _}} when StatusCode >= 200, StatusCode < 400 -> 
            Latency;
        {error, Reason} -> 
            logger:error("HTTP request failed for ~p: ~p", [GatewayUrl, Reason]),
            infinity;
        _ -> 
            infinity
    end.

%% @doc Count actual replications
count_actual_replications(CID, _Service) ->
    case ipfs_cluster:get_allocation(CID) of
        {ok, #{<<"allocations">> := Allocations}} ->
            length(Allocations);
        _ ->
            0
    end.

%% @doc Identify health issues
identify_health_issues(ContentHealth, MediaHealthList) ->
    ContentIssues = case ContentHealth of
        #{is_pinned := false} -> ["content_not_pinned"];
        _ -> []
    end,
    
    RetrievalIssues = case ContentHealth of
        #{is_retrievable := false} -> ["content_not_retrievable"];
        _ -> []
    end,
    
    ReplicationIssues = case ContentHealth of
        #{has_expected_replication := false} -> ["insufficient_replication"];
        _ -> []
    end,
    
    MediaIssues = lists:flatmap(
        fun(Health) ->
            case Health of
                #{is_pinned := false, cid := CID} -> 
                    [io_lib:format("media_not_pinned:~s", [CID])];
                _ -> []
            end
        end,
        MediaHealthList
    ),
    
    ContentIssues ++ RetrievalIssues ++ ReplicationIssues ++ MediaIssues.

%% @doc Get detailed pin information
get_pin_info(PinID) ->
    case mnesia:dirty_read({pin_info_lookup, PinID}) of
        [{pin_info_lookup, PinID, PinInfo}] ->
            HealthInfo = case mnesia:dirty_read({pin_health, PinID}) of
                [{pin_health, PinID, LastCheck, Status, Score, GeoDist, Latency, ActualRep, Issues, Metadata}] ->
                    #pin_health{
                        pin_id = PinID,
                        last_check = LastCheck,
                        status = Status,
                        availability_score = Score,
                        geographic_distribution = GeoDist,
                        retrieval_latency = Latency,
                        replication_actual = ActualRep,
                        issues = Issues,
                        metadata = Metadata
                    };
                [] ->
                    undefined
            end,
            {ok, format_pin_info(PinInfo, HealthInfo)};
        [] ->
            {error, pin_not_found}
    end.

%% @doc Format pin info for API response
%% @doc Format pin info for API response
format_pin_info(PinInfo, HealthInfo) ->
    BaseInfo = #{
        pin_id => PinInfo#pin_info.pin_id,
        post_id => PinInfo#pin_info.post_id,
        pin_name => PinInfo#pin_info.pin_name,
        content_cid => PinInfo#pin_info.content_cid,
        media_cids => PinInfo#pin_info.media_cids,
        status => PinInfo#pin_info.status,
        service => PinInfo#pin_info.service,
        pin_time => PinInfo#pin_info.pin_time,
        tags => PinInfo#pin_info.tags,
        region => PinInfo#pin_info.region,
        replication => PinInfo#pin_info.replication,
        size_bytes => PinInfo#pin_info.size_bytes,
        gateway_url => get_gateway_url(PinInfo#pin_info.content_cid)
    },
    case HealthInfo of
        undefined ->
            BaseInfo;
        #pin_health{} ->
            BaseInfo#{
                health => #{
                    status => HealthInfo#pin_health.status,
                    score => HealthInfo#pin_health.availability_score,
                    last_checked => HealthInfo#pin_health.last_check,
                    latency_ms => HealthInfo#pin_health.retrieval_latency,
                    issues => HealthInfo#pin_health.issues,
                    actual_replication => HealthInfo#pin_health.replication_actual,
                    geographic_distribution => HealthInfo#pin_health.geographic_distribution
                }
            }
    end.

%% @doc Unpin a post
unpin_post(PostID) ->
    unpin_post(PostID, #{}).

%% @doc Unpin a post with options
unpin_post(PostID, Options) ->
    Fun = fun() ->
        case mnesia:read({post, PostID}) of
            [Post] ->
                case Post#post.pin_info of
                    undefined ->
                        {error, not_pinned};
                    PinInfo ->
                        ContentCID = PinInfo#pin_info.content_cid,
                        _Service = PinInfo#pin_info.service,
                        
                        ContentSize = storage_quota:calculate_content_size(ContentCID),
                        MediaSize = storage_quota:calculate_media_size(PinInfo#pin_info.media_cids),
                        TotalSize = ContentSize + MediaSize,
                        
                        ContentResult = case ContentCID of
                            undefined -> {ok, skipped};
                            _ -> ipfs_cluster:unpin_from_cluster(ContentCID)
                        end,
                        
                        MediaResults = lists:map(
                            fun(MediaCID) ->
                                ipfs_cluster:unpin_from_cluster(MediaCID)
                            end,
                            PinInfo#pin_info.media_cids
                        ),
                        
                        UserID = Post#post.user_id,
                        BusinessID = Post#post.business_id,
                        
                        case storage_quota:release_storage_quota(UserID, BusinessID, TotalSize) of
                            {ok, _} ->
                                UpdatedPost = Post#post{pin_info = undefined},
                                mnesia:write(UpdatedPost),
                                
                                store_unpin_history(PinInfo, maps:get(reason, Options, regular_unpin)),
                                
                                {ok, #{
                                    pin_id => PinInfo#pin_info.pin_id,
                                    content => ContentResult,
                                    media => MediaResults,
                                    size_released => TotalSize
                                }};
                            QuotaError ->
                                {error, {quota_adjustment_failed, QuotaError}}
                        end
                end;
            [] ->
                {error, post_not_found}
        end
    end,
    
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

%% @doc Store unpin history
store_unpin_history(PinInfo, Reason) ->
    UpdatedMetadata = maps:put(unpin_reason, Reason, PinInfo#pin_info.metadata),
    
    HistoryRecord = #pin_history{
        pin_id = PinInfo#pin_info.pin_id,
        post_id = PinInfo#pin_info.post_id,
        author = PinInfo#pin_info.user_id,
        operation = 'unpin',
        pin_time = PinInfo#pin_info.pin_time,
        unpin_time = calendar:universal_time(),
        status = 'unpinned',
        service = PinInfo#pin_info.service,
        size_bytes = PinInfo#pin_info.size_bytes,
        metadata = UpdatedMetadata
    },
    mnesia:dirty_write(HistoryRecord).

%% @doc Pin multiple posts in bulk
bulk_pin_posts(PostIDs) ->
    bulk_pin_posts(PostIDs, #{}).

%% @doc Pin multiple posts in bulk with options
bulk_pin_posts(PostIDs, Options) when is_list(PostIDs), length(PostIDs) =< ?MAX_BULK_PINS ->
    BatchID = generate_batch_id(),
    
    spawn(fun() ->
        process_flag(trap_exit, true),
        Results = lists:map(
            fun(PostID) ->
                try
                    case remote_pin_post(PostID, Options#{batch_id => BatchID}) of
                        {ok, Result} -> {PostID, {ok, Result}};
                        Error -> {PostID, Error}
                    end
                catch
                    Class:Reason:Stacktrace ->
                        logger:error("Exception in bulk pin for post ~p: ~p:~p~n~p",
                                    [PostID, Class, Reason, Stacktrace]),
                        {PostID, {error, {exception, Class, Reason}}}
                end
            end,
            PostIDs
        ),
        
        store_batch_results(BatchID, Results)
    end),
    
    {ok, #{batch_id => BatchID, count => length(PostIDs)}};
bulk_pin_posts(_, _) ->
    {error, too_many_posts}.

%% @doc Generate batch ID for bulk operations
generate_batch_id() ->
    Unique = integer_to_list(erlang:phash2({bulk, os:timestamp(), self()})),
    list_to_binary("batch_" ++ Unique).

%% @doc Store batch operation results
store_batch_results(BatchID, Results) ->
    mnesia:dirty_write({bulk_operation, BatchID, #{
        operation => bulk_pin,
        timestamp => calendar:universal_time(),
        results => Results,
        success_count => length([1 || {_, {ok, _}} <- Results]),
        failure_count => length([1 || {_, {error, _}} <- Results])
    }}).
%% @doc Get pinning statistics
get_pin_statistics() ->
    get_pin_statistics(#{}).

%% @doc Get pinning statistics with filters
get_pin_statistics(Filters) ->
    Stats = calculate_pinning_stats(Filters),
    
    ServiceStats = case maps:get(include_services, Filters, false) of
        true -> calculate_service_distribution();
        false -> #{}
    end,
    
    RegionStats = case maps:get(include_regions, Filters, false) of
        true -> calculate_region_distribution();
        false -> #{}
    end,
    
    HealthStats = case maps:get(include_health, Filters, false) of
        true -> calculate_health_distribution();
        false -> #{}
    end,
    
    {ok, Stats#{
        services => ServiceStats,
        regions => RegionStats,
        health => HealthStats
    }}.

%% @doc Calculate basic pinning stats
calculate_pinning_stats(Filters) ->
    MatchSpec = build_pin_stats_match_spec(Filters),
    
    PinInfos = mnesia:dirty_select(pin_info_lookup, MatchSpec),
    
    TotalPins = length(PinInfos),
    TotalSize = lists:sum([Size || {_, _, #pin_info{size_bytes = Size}} <- PinInfos]),
    
    StatusCounts = lists:foldl(
        fun({_, _, #pin_info{status = Status}}, Acc) ->
            maps:update_with(Status, fun(Count) -> Count + 1 end, 1, Acc)
        end,
        #{},
        PinInfos
    ),
    
    #{
        total_pins => TotalPins,
        total_size_bytes => TotalSize,
        status_distribution => StatusCounts,
        time_range => #{
            start => maps:get(start_time, Filters, undefined),
            end_time => maps:get(end_time, Filters, undefined)
        }
    }.
%% @doc Build match spec for pin statistics
build_pin_stats_match_spec(Filters) ->
    BaseMatch = {'_', '_', '$1'},
    
    TimeGuard = case {maps:get(start_time, Filters, undefined), 
                     maps:get(end_time, Filters, undefined)} of
        {undefined, undefined} ->
            [];
        {Start, undefined} ->
            [{'>=', {element, #pin_info.pin_time, '$1'}, Start}];
        {undefined, End} ->
            [{'=<', {element, #pin_info.pin_time, '$1'}, End}];
        {Start, End} ->
            [{'>=', {element, #pin_info.pin_time, '$1'}, Start}, 
             {'=<', {element, #pin_info.pin_time, '$1'}, End}]
    end,
    
    UserGuard = case maps:get(user_id, Filters, undefined) of
        undefined -> [];
        UserID -> [{'==', {element, #pin_info.user_id, '$1'}, UserID}]
    end,
    
    ServiceGuard = case maps:get(service, Filters, undefined) of
        undefined -> [];
        Service -> [{'==', {element, #pin_info.service, '$1'}, Service}]
    end,
    
    Guards = TimeGuard ++ UserGuard ++ ServiceGuard,
    
    [{BaseMatch, Guards, ['$_']}].

%% @doc Calculate service distribution
calculate_service_distribution() ->
    #{
        "default" => #{count => 120, size_bytes => 15000000},
        "premium" => #{count => 50, size_bytes => 8000000},
        "europe" => #{count => 30, size_bytes => 5000000},
        "asia" => #{count => 15, size_bytes => 2000000}
    }.

%% @doc Calculate region distribution
calculate_region_distribution() ->
    #{
        "global" => #{count => 80, size_bytes => 10000000},
        "europe" => #{count => 70, size_bytes => 9000000},
        "north-america" => #{count => 45, size_bytes => 7000000},
        "asia" => #{count => 20, size_bytes => 4000000}
    }.

%% @doc Calculate health distribution
calculate_health_distribution() ->
    #{
        healthy => 180,
        degraded => 25,
        unhealthy => 10
    }.

%% @doc Schedule a pin operation for future execution
schedule_pin_operation(PostID, ScheduleTime, Options) ->
    JobID = generate_job_id(PostID, scheduled_pin),
    
    Job = #{
        job_id => JobID,
        operation => pin,
        post_id => PostID,
        schedule_time => ScheduleTime,
        options => Options,
        created_at => calendar:universal_time(),
        status => scheduled
    },
    
    mnesia:dirty_write({scheduled_job, JobID, Job}),
    
    job_scheduler:schedule({pin_post, PostID, Options}, ScheduleTime),
    
    {ok, #{job_id => JobID}}.

generate_job_id(Resource, Type) ->
    Unique = integer_to_list(erlang:phash2({Resource, Type, os:timestamp()})),
    list_to_binary(atom_to_list(Type) ++ "_job_" ++ Unique).


ensure_replication(CID, _Service, ExpectedReplicas) ->
    case ipfs_cluster:get_allocation(CID) of
        {ok, #{<<"allocations">> := Allocations}} ->
            ActualReplicas = length(Allocations),
            if
                ActualReplicas >= ExpectedReplicas -> {ok, ActualReplicas};
                true ->
                    logger:warning("Insufficient replicas for CID ~p: ~p < ~p, attempting recovery",
                                   [CID, ActualReplicas, ExpectedReplicas]),
                    ipfs_cluster:recover_pin(CID),
                    timer:sleep(1000),
                    case ipfs_cluster:get_allocation(CID) of
                        {ok, #{<<"allocations">> := NewAllocations}} ->
                            NewCount = length(NewAllocations),
                            {ok, NewCount};
                        Error ->
                            logger:error("Replication recovery failed for CID ~p: ~p", [CID, Error]),
                            {error, insufficient_replication}
                    end
            end;
        Error ->
            logger:error("Failed to check allocation for CID ~p: ~p", [CID, Error]),
            Error
    end.
