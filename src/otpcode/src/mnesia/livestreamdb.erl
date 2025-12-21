-module(livestreamdb).
-export([
    create_livestream/8,
    start_livestream/5,
    end_livestream/2,
    get_livestream/1,
    get_user_livestreams/1,
    get_all_livestreams/0,
    get_live_streams/0,
    get_live_streams_by_category/1,
    increment_viewer/2,
    decrement_viewer/2,
    react_to_livestream/3,
    get_reaction_counts/1,
    share_livestream/2,
    save_livestream/2,
    add_chat_message/4,
    get_chat_messages/1,
    get_chat_messages/2,
    update_stream_quality/3,
    add_moderator/3,
    ban_user/4,
    enable_slow_mode/3,
    delete_livestream/2,
    get_stream_analytics/1,
    generate_stream_id/0,
    generate_stream_key/0,
    enable_auto_record/2,
    disable_auto_record/2,
    save_recording/3,
    get_recorded_streams/1,
    get_vod_streams/0,
    update_recording_status/3
]).

-include("../records.hrl").
-include("../media_records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").

create_livestream(UserId, Title, Description, Visibility, Tags, Category, Language, ScheduledFor) ->
    StreamId = generate_stream_id(),
    StreamKey = generate_stream_key(),
    RtmpUrl = generate_rtmp_url(StreamKey),
    BackupRtmpUrl = generate_backup_rtmp_url(StreamKey),
    Now = calendar:universal_time(),
    Stream = #livestream{
        id = StreamId,
        user_id = UserId,
        title = Title,
        description = Description,
        status = scheduled,
        visibility = Visibility,
        tags = Tags,
        category = Category,
        language = Language,
        stream_key = StreamKey,
        rtmp_url = RtmpUrl,
        rtmp_backup_url = BackupRtmpUrl,
        scheduled_for = ScheduledFor,
        date_created = Now
    },
    case mnesia:transaction(fun() ->
        mnesia:write(Stream)
    end) of
        {atomic, ok} ->
            {ok, StreamId, StreamKey, RtmpUrl, BackupRtmpUrl};
        {aborted, Reason} ->
            {error, Reason}
    end.

start_livestream(StreamId, UserId, RustStreamId, PlaybackUrl, HlsUrl) ->
    case mnesia:transaction(fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                if Stream#livestream.user_id =:= UserId ->
                    UpdatedStream = Stream#livestream{
                        status = live,
                        rust_stream_id = RustStreamId,
                        playback_url = PlaybackUrl,
                        hls_url = HlsUrl,
                        started_at = calendar:universal_time(),
                        notification_sent = true
                    },
                    mnesia:write(UpdatedStream),
                    {ok, updated};
                true ->
                    {error, unauthorized}
                end;
            [] ->
                {error, not_found}
        end
    end) of
        {atomic, {ok, updated}} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, Reason}
    end.

end_livestream(StreamId, UserId0) when is_binary(StreamId) orelse is_list(StreamId) ->
    UserId = normalize_user_id(UserId0),
    StreamIdBin = if is_binary(StreamId) -> StreamId; true -> list_to_binary(StreamId) end,
    case mnesia:transaction(fun() ->
        case mnesia:read({livestream, StreamIdBin}) of
            [Stream] ->
                DbUserId = Stream#livestream.user_id,
                DbUserIdNorm = normalize_user_id(DbUserId),
                case DbUserIdNorm =:= UserId of
                    true ->
                        Now = calendar:universal_time(),
                        Duration = case Stream#livestream.started_at of
                            undefined -> 0;
                            Start ->
                                calendar:datetime_to_gregorian_seconds(Now) -
                                calendar:datetime_to_gregorian_seconds(Start)
                        end,
                        Updated = Stream#livestream{
                            status = ended,
                            ended_at = Now,
                            duration_seconds = Duration
                        },
                        mnesia:write(Updated),
                        {ok, Duration};
                    false ->
                        {error, unauthorized}
                end;
            [] ->
                {error, not_found}
        end
    end) of
        {atomic, {ok, Duration}} ->
            {ok, Duration};
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {aborted, Reason}}
    end.

get_livestream(StreamId) ->
    case mnesia:transaction(fun() ->
        mnesia:read({livestream, StreamId})
    end) of
        {atomic, [Stream]} ->
            {ok, Stream};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_user_livestreams(UserId) ->
    case mnesia:transaction(fun() ->
        mnesia:match_object(#livestream{user_id = UserId, _ = '_'})
    end) of
        {atomic, Streams} ->
            lists:reverse(lists:keysort(#livestream.started_at, Streams));
        {aborted, _Reason} ->
            []
    end.

get_all_livestreams() ->
    case mnesia:transaction(fun() ->
        mnesia:match_object(#livestream{_ = '_'})
    end) of
        {atomic, Streams} ->
            {ok, Streams};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_live_streams() ->
    case mnesia:transaction(fun() ->
        mnesia:match_object(#livestream{status = live, visibility = public, _ = '_'})
    end) of
        {atomic, Streams} ->
            Sorted = lists:reverse(lists:keysort(#livestream.viewers_count, Streams)),
            Sorted;
        {aborted, _Reason} ->
            []
    end.

get_live_streams_by_category(Category) ->
    case mnesia:transaction(fun() ->
        mnesia:match_object(#livestream{status = live, visibility = public, category = Category, _ = '_'})
    end) of
        {atomic, Streams} ->
            lists:reverse(lists:keysort(#livestream.viewers_count, Streams));
        {aborted, _Reason} ->
            []
    end.

increment_viewer(StreamId, ViewerId) ->
    mnesia:transaction(fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                UniqueViewers = Stream#livestream.unique_viewers,
                IsNewViewer = not lists:member(ViewerId, UniqueViewers),
                NewUniqueViewers = case IsNewViewer of
                    true -> [ViewerId | UniqueViewers];
                    false -> UniqueViewers
                end,
                NewTotalUnique = case IsNewViewer of
                    true -> Stream#livestream.total_unique_viewers + 1;
                    false -> Stream#livestream.total_unique_viewers
                end,
                NewViewerCount = Stream#livestream.viewers_count + 1,
                NewPeakViewers = max(NewViewerCount, Stream#livestream.peak_viewers),
                Now = calendar:universal_time(),
                ViewerEntry = {ViewerId, joined, Now},
                UpdatedTimeline = [ViewerEntry | Stream#livestream.viewer_timeline],
                UpdatedStream = Stream#livestream{
                    viewers_count = NewViewerCount,
                    peak_viewers = NewPeakViewers,
                    unique_viewers = NewUniqueViewers,
                    total_unique_viewers = NewTotalUnique,
                    viewer_timeline = UpdatedTimeline
                },
                mnesia:write(UpdatedStream),
                {ok, NewViewerCount};
            [] ->
                {error, not_found}
        end
    end).

decrement_viewer(StreamId, ViewerId) ->
    mnesia:transaction(fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                NewCount = max(0, Stream#livestream.viewers_count - 1),
                Now = calendar:universal_time(),
                ViewerEntry = {ViewerId, left, Now},
                UpdatedTimeline = [ViewerEntry | Stream#livestream.viewer_timeline],
                UpdatedStream = Stream#livestream{
                    viewers_count = NewCount,
                    viewer_timeline = UpdatedTimeline
                },
                mnesia:write(UpdatedStream),
                {ok, NewCount};
            [] ->
                {error, not_found}
        end
    end).

react_to_livestream(UserId, StreamId, ReactionType) ->
    ValidReactions = [like, love, wow, haha, fire],
    case lists:member(ReactionType, ValidReactions) of
        false -> {error, invalid_reaction_type};
        true ->
            Fun = fun() ->
                case mnesia:read({livestream, StreamId}) of
                    [Stream] ->
                        ExistingReaction = find_user_reaction_in_livestream(Stream, UserId),
                        case ExistingReaction of
                            {found, OldType, OldLikeID} when OldType =:= ReactionType ->
                                Reactions = Stream#livestream.reactions,
                                ReactionList = maps:get(OldType, Reactions, []),
                                UpdatedList = lists:delete(OldLikeID, ReactionList),
                                UpdatedReactions = maps:put(OldType, UpdatedList, Reactions),
                                ReactionCounts = Stream#livestream.reaction_counts,
                                CurrentCount = maps:get(OldType, ReactionCounts, 0),
                                UpdatedCount = max(0, CurrentCount - 1),
                                UpdatedReactionCounts = maps:put(OldType, UpdatedCount, ReactionCounts),
                                mnesia:delete({like, OldLikeID}),
                                mnesia:write(Stream#livestream{
                                    reactions = UpdatedReactions,
                                    reaction_counts = UpdatedReactionCounts
                                }),
                                {removed, OldType};
                            {found, OldType, OldLikeID} ->
                                Reactions = Stream#livestream.reactions,
                                OldReactionList = maps:get(OldType, Reactions, []),
                                UpdatedOldList = lists:delete(OldLikeID, OldReactionList),
                                IntermediateReactions = maps:put(OldType, UpdatedOldList, Reactions),
                                ReactionCounts = Stream#livestream.reaction_counts,
                                OldCount = maps:get(OldType, ReactionCounts, 0),
                                UpdatedOldCount = max(0, OldCount - 1),
                                IntermediateReactionCounts = maps:put(OldType, UpdatedOldCount, ReactionCounts),
                                mnesia:delete({like, OldLikeID}),
                                ID = nanoid:gen(),
                                mnesia:write(#like{
                                    id = ID,
                                    userID = UserId,
                                    reaction_type = ReactionType,
                                    date_created = calendar:universal_time()
                                }),
                                NewReactionList = maps:get(ReactionType, IntermediateReactions, []),
                                FinalReactions = maps:put(ReactionType, [ID | NewReactionList], IntermediateReactions),
                                NewCount = maps:get(ReactionType, IntermediateReactionCounts, 0),
                                FinalReactionCounts = maps:put(ReactionType, NewCount + 1, IntermediateReactionCounts),
                                mnesia:write(Stream#livestream{
                                    reactions = FinalReactions,
                                    reaction_counts = FinalReactionCounts
                                }),
                                ID;
                            not_found ->
                                ID = nanoid:gen(),
                                mnesia:write(#like{
                                    id = ID,
                                    userID = UserId,
                                    reaction_type = ReactionType,
                                    date_created = calendar:universal_time()
                                }),
                                Reactions = Stream#livestream.reactions,
                                ReactionList = maps:get(ReactionType, Reactions, []),
                                UpdatedReactions = maps:put(ReactionType, [ID | ReactionList], Reactions),
                                ReactionCounts = Stream#livestream.reaction_counts,
                                CurrentCount = maps:get(ReactionType, ReactionCounts, 0),
                                UpdatedReactionCounts = maps:put(ReactionType, CurrentCount + 1, ReactionCounts),
                                mnesia:write(Stream#livestream{
                                    reactions = UpdatedReactions,
                                    reaction_counts = UpdatedReactionCounts
                                }),
                                ID
                        end;
                    [] ->
                        {error, stream_not_found}
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end
    end.

find_user_reaction_in_livestream(Stream, UserId) ->
    Reactions = Stream#livestream.reactions,
    ReactionTypes = [like, love, wow, haha, fire],
    find_user_reaction_in_types(UserId, ReactionTypes, Reactions).

find_user_reaction_in_types(_UserId, [], _Reactions) ->
    not_found;
find_user_reaction_in_types(UserId, [Type | Rest], Reactions) ->
    ReactionList = maps:get(Type, Reactions, []),
    case lists:any(fun(ID) ->
        case mnesia:read({like, ID}) of
            [Like] -> Like#like.userID =:= UserId;
            [] -> false
        end
    end, ReactionList) of
        true ->
            LikeID = lists:foldl(fun(ID, Acc) ->
                case mnesia:read({like, ID}) of
                    [Like] when Like#like.userID =:= UserId -> ID;
                    _ -> Acc
                end
            end, undefined, ReactionList),
            {found, Type, LikeID};
        false ->
            find_user_reaction_in_types(UserId, Rest, Reactions)
    end.

get_reaction_counts(StreamId) ->
    Fun = fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] -> Stream#livestream.reaction_counts;
            [] -> #{}
        end
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

share_livestream(StreamId, UserId) ->
    Fun = fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                UpdatedShares = Stream#livestream.shares + 1,
                UpdatedStream = Stream#livestream{shares = UpdatedShares},
                mnesia:write(UpdatedStream),
                case mnesia:read({user, UserId}) of
                    [User] ->
                        SharedStreams = case User#user.data of
                            #{shared_streams := Shared} -> Shared;
                            _ -> []
                        end,
                        UpdatedData = maps:put(shared_streams, [StreamId | SharedStreams], User#user.data),
                        mnesia:write(User#user{data = UpdatedData});
                    [] -> ok
                end,
                {ok, UpdatedShares};
            [] ->
                {error, stream_not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

save_livestream(StreamId, UserId) ->
    Fun = fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                UpdatedSaves = Stream#livestream.saves + 1,
                UpdatedStream = Stream#livestream{saves = UpdatedSaves},
                mnesia:write(UpdatedStream),
                case mnesia:read({user, UserId}) of
                    [User] ->
                        SavedStreams = case User#user.data of
                            #{saved_livestreams := Saved} -> Saved;
                            _ -> []
                        end,
                        UpdatedData = maps:put(saved_livestreams, [StreamId | SavedStreams], User#user.data),
                        mnesia:write(User#user{data = UpdatedData});
                    [] -> ok
                end,
                ok;
            [] ->
                {error, stream_not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_chat_message(StreamId, UserId, Username, Message) ->
    Fun = fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                case Stream#livestream.chat_enabled of
                    false -> {error, chat_disabled};
                    true ->
                        MessageId = nanoid:gen(),
                        Now = calendar:universal_time(),
                        ChatMessage = {MessageId, UserId, Username, Message, Now},
                        UpdatedMessages = [ChatMessage | Stream#livestream.chat_messages],
                        mnesia:write(Stream#livestream{
                            chat_messages = UpdatedMessages
                        }),
                        {ok, MessageId}
                end;
            [] ->
                {error, stream_not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_chat_messages(StreamId) ->
    get_chat_messages(StreamId, 100).

get_chat_messages(StreamId, Limit) ->
    Fun = fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                Messages = Stream#livestream.chat_messages,
                lists:sublist(lists:reverse(Messages), Limit);
            [] ->
                []
        end
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

update_stream_quality(StreamId, Bitrate, Resolution) ->
    Fun = fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                UpdatedStream = Stream#livestream{
                    current_bitrate = Bitrate,
                    current_resolution = Resolution
                },
                mnesia:write(UpdatedStream),
                ok;
            [] ->
                {error, stream_not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_moderator(StreamId, UserId, ModeratorId) ->
    Fun = fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                if Stream#livestream.user_id =:= UserId ->
                    Moderators = Stream#livestream.moderators,
                    UpdatedModerators = case lists:member(ModeratorId, Moderators) of
                        true -> Moderators;
                        false -> [ModeratorId | Moderators]
                    end,
                    mnesia:write(Stream#livestream{
                        moderators = UpdatedModerators
                    }),
                    ok;
                true ->
                    {error, unauthorized}
                end;
            [] ->
                {error, stream_not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

ban_user(StreamId, ModeratorId, BannedUserId, Reason) ->
    Fun = fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                IsModerator = lists:member(ModeratorId, Stream#livestream.moderators) orelse
                              Stream#livestream.user_id =:= ModeratorId,
                case IsModerator of
                    true ->
                        BannedUsers = Stream#livestream.banned_users,
                        BanEntry = {BannedUserId, Reason, calendar:universal_time()},
                        UpdatedBannedUsers = [BanEntry | BannedUsers],
                        mnesia:write(Stream#livestream{
                            banned_users = UpdatedBannedUsers
                        }),
                        ok;
                    false ->
                        {error, unauthorized}
                end;
            [] ->
                {error, stream_not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

enable_slow_mode(StreamId, UserId, DurationSeconds) ->
    Fun = fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                if Stream#livestream.user_id =:= UserId ->
                    mnesia:write(Stream#livestream{
                        slow_mode = true,
                        slow_mode_duration = DurationSeconds
                    }),
                    ok;
                true ->
                    {error, unauthorized}
                end;
            [] ->
                {error, stream_not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

delete_livestream(StreamId, UserId) when is_binary(StreamId), is_binary(UserId) ->
    case mnesia:transaction(fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                UserIdInDb = Stream#livestream.user_id,
                case UserIdInDb of
                    UserIdBin when is_binary(UserIdBin), UserIdBin =:= UserId ->
                        mnesia:delete({livestream, StreamId}),
                        {ok, deleted};
                    UserIdList when is_list(UserIdList) ->
                        case list_to_binary(UserIdList) of
                            UserId ->
                                mnesia:delete({livestream, StreamId}),
                                {ok, deleted};
                            _ ->
                                {error, unauthorized}
                        end;
                    _ ->
                        {error, unauthorized}
                end;
            [] ->
                {error, not_found}
        end
    end) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            logger:error("Mnesia delete aborted: ~p", [Reason]),
            {error, {aborted, Reason}}
    end.

get_stream_analytics(StreamId) ->
    Fun = fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                {ok, #{
                    viewers_count => Stream#livestream.viewers_count,
                    peak_viewers => Stream#livestream.peak_viewers,
                    total_unique_viewers => Stream#livestream.total_unique_viewers,
                    duration_seconds => Stream#livestream.duration_seconds,
                    chat_messages => length(Stream#livestream.chat_messages),
                    reactions => Stream#livestream.reaction_counts,
                    shares => Stream#livestream.shares,
                    saves => Stream#livestream.saves,
                    current_bitrate => Stream#livestream.current_bitrate,
                    current_resolution => Stream#livestream.current_resolution,
                    dropped_frames => Stream#livestream.dropped_frames,
                    stream_health_score => Stream#livestream.stream_health_score
                }};
            [] ->
                {error, stream_not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

enable_auto_record(StreamId, UserId) ->
    Fun = fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                if Stream#livestream.user_id =:= UserId ->
                    mnesia:write(Stream#livestream{
                        auto_record = true
                    }),
                    ok;
                true ->
                    {error, unauthorized}
                end;
            [] ->
                {error, stream_not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

disable_auto_record(StreamId, UserId) ->
    Fun = fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                if Stream#livestream.user_id =:= UserId ->
                    mnesia:write(Stream#livestream{
                        auto_record = false
                    }),
                    ok;
                true ->
                    {error, unauthorized}
                end;
            [] ->
                {error, stream_not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

save_recording(StreamId, UserId, RecordingCid) ->
    Fun = fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                if Stream#livestream.user_id =:= UserId ->
                    mnesia:write(Stream#livestream{
                        recording_cid = RecordingCid,
                        recording_status = available,
                        vod_enabled = true,
                        vod_cid = RecordingCid
                    }),
                    ok;
                true ->
                    {error, unauthorized}
                end;
            [] ->
                {error, stream_not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_recorded_streams(UserId) ->
    case mnesia:transaction(fun() ->
        mnesia:match_object(#livestream{
            user_id = UserId,
            recording_status = available,
            _ = '_'
        })
    end) of
        {atomic, Streams} ->
            {ok, lists:reverse(lists:keysort(#livestream.started_at, Streams))};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_vod_streams() ->
    case mnesia:transaction(fun() ->
        mnesia:match_object(#livestream{
            vod_enabled = true,
            status = ended,
            visibility = public,
            _ = '_'
        })
    end) of
        {atomic, Streams} ->
            {ok, lists:reverse(lists:keysort(#livestream.started_at, Streams))};
        {aborted, Reason} ->
            {error, Reason}
    end.

update_recording_status(StreamId, UserId, Status) ->
    Fun = fun() ->
        case mnesia:read({livestream, StreamId}) of
            [Stream] ->
                if Stream#livestream.user_id =:= UserId ->
                    mnesia:write(Stream#livestream{
                        recording_status = Status
                    }),
                    ok;
                true ->
                    {error, unauthorized}
                end;
            [] ->
                {error, stream_not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

generate_stream_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(999999),
    list_to_binary(io_lib:format("stream_~p_~p", [Timestamp, Random])).

generate_stream_key() ->
    Bytes = crypto:strong_rand_bytes(32),
    base64:encode(Bytes).

generate_rtmp_url(StreamKey) ->
    ServerUrl = application:get_env(mazaryn, rtmp_server, "rtmp://localhost:1935/live"),
    list_to_binary(io_lib:format("~s/~s", [ServerUrl, StreamKey])).

generate_backup_rtmp_url(StreamKey) ->
    BackupUrl = application:get_env(mazaryn, rtmp_backup_server, "rtmp://backup.localhost:1935/live"),
    list_to_binary(io_lib:format("~s/~s", [BackupUrl, StreamKey])).

normalize_user_id(UserId) when is_binary(UserId) -> UserId;
normalize_user_id(UserId) when is_list(UserId) -> list_to_binary(UserId);
normalize_user_id(UserId) -> list_to_binary(io_lib:format("~p", [UserId])).
