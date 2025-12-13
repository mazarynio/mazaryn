-module(videodb).
-author("Zaryn Technologies").

-export([
    create_video/10,
    create_video_concurrent/10,
    create_video_from_file/9,
    upload_video_file/2,
    update_video/11,
    delete_video/2,

    get_video_by_id/1,
    get_videos_by_creator/1,
    get_videos_by_business/1,
    get_public_videos/0,
    get_videos_by_tag/1,
    get_video_content/1,
    get_video_metadata/1,
    get_video_qualities/1,

    create_video_version/4,
    get_video_versions/1,
    add_thumbnail/3,
    add_preview/2,
    add_chapter/4,
    add_subtitle/4,
    add_audio_track/4,
    get_unique_view_count/1,

    react_to_video/3,
    remove_reaction_from_video/2,
    get_reactions_by_type/2,
    get_all_reactions/1,
    get_reaction_counts/1,
    has_user_reacted_with_type/3,
    get_user_reaction_type/2,

    add_video_comment/3,
    get_video_comments/1,
    get_video_comment_content/1,
    update_video_comment/2,
    delete_video_comment/2,
    react_to_video_comment/3,
    get_comment_reactions/1,
    get_user_comment_reaction/2,
    get_comment_count/1,

    increment_view_count/2,
    increment_unique_view/2,
    track_watch_time/3,
    get_view_stats/1,

    share_video/2,
    share_video/3,
    save_video/2,
    unsave_video/2,

    add_to_playlist/2,
    remove_from_playlist/2,
    link_to_series/3,

    add_interactive_hotspot/5,
    create_poll/4,
    vote_on_poll/3,

    set_monetization/3,
    add_ad_break/3,
    set_sponsor_info/2,

    track_viewer_retention/3,
    track_geographic_view/3,
    track_device_view/2,
    track_referral/2,

    start_live_stream/2,
    end_live_stream/1,
    update_live_viewers/2,
    get_live_stream_info/1,

    create_video_clip/5,
    enable_duet/1,
    disable_duet/1,
    add_duet_video/2,
    enable_stitch/1,
    disable_stitch/1,
    add_stitch_video/2,

    add_effect/2,
    add_filter/2,
    set_sound/2,

    create_challenge/3,
    submit_to_challenge/2,

    mint_video_nft/6,
    get_video_nft/1,

    add_collaboration/2,
    set_remix_permissions/2,
    set_revenue_split/2,

    pin_video/1,
    unpin_video/1,
    update_pin_status/2,

    report_video/4,
    set_age_restriction/2,
    set_content_warnings/2,

    search_videos/1,
    search_videos_advanced/1,
    get_trending_videos/1,
    get_featured_videos/0,

    increment_download_count/1,

    start_raid/3,
    host_channel/2,

    add_moderator/2,
    remove_moderator/2,
    add_vip/2,
    remove_vip/2,
    ban_user/3,

    enable_channel_points/1,
    create_channel_reward/5,
    redeem_reward/3,

    enable_clip_creation/1,
    disable_clip_creation/1,

    add_simulcast_destination/2,
    remove_simulcast_destination/2,

    calculate_trending_score/1,
    update_analytics/1,

    create_video_with_rust/10,
    upload_video_to_ipfs_simple/2,
    start_live_stream_with_rust/4,
    track_video_playback/3,
    update_playback_position/2,
    get_video_analytics/1,
    export_video_analytics/1,
    detect_video_format/1,
    get_video_info_from_rust/1,
    get_video_cid/1,
    get_transcoding_status/1,
    get_available_qualities/1,
    update_video_ipns/2,
    get_video/1,
    generate_stream_key/0
]).

-include("../records.hrl").
-include("../media_records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("kernel/include/file.hrl").

-define(DEFAULT_CONCURRENCY, 5).
-define(DEFAULT_TIMEOUT, 30000).
-define(MAX_RETRIES, 10).
-define(BACKOFF_TIME, 20).
-define(MAX_FILE_SIZE, 107374182400).
-define(CHUNK_SIZE, 10485760).
-define(SUPPORTED_FORMATS, [
    ".mp4", ".mov", ".avi", ".mkv", ".webm",
    ".flv", ".wmv", ".m4v", ".mpg", ".mpeg"
]).

create_video(CreatorId, Title, Description, VideoFile, Duration, Privacy, _Tags, AllowComments, AllowDownloads, Monetized) ->
    Fun = fun() ->
        Id = nanoid:gen(),
        Now = calendar:universal_time(),
        AI_Video_ID = ai_videodb:insert_in_transaction(Id),

        ok = content_cache:set({video_file, Id}, VideoFile),
        SizeBytes = calculate_content_size(VideoFile),

        Video = #video{
            id = Id,
            ai_video_id = AI_Video_ID,
            user_id = CreatorId,
            file_url = {pending, Id},
            ipfs_cid = {pending, Id},
            status = processing,
            title = Title,
            description = Description,
            duration_seconds = Duration,
            privacy = Privacy,
            hashtags = _Tags,
            allow_comments = AllowComments,
            allow_downloads = AllowDownloads,
            monetized = Monetized,
            file_size_bytes = SizeBytes,
            date_created = Now,
            date_updated = Now,
            views = 0,
            unique_views = 0,
            unique_viewers = [],
            likes = [],
            reactions = #{
                like => [],
                love => [],
                wow => [],
                haha => [],
                fire => []
            },
            reaction_counts = #{
                like => 0,
                love => 0,
                wow => 0,
                haha => 0,
                fire => 0
            },
            comments = [],
            shares = 0,
            saves = 0
        },

        mnesia:write(Video),

        case mnesia:read({user, CreatorId}) of
            [User] ->
                mnesia:write(User#user{media = [Id | User#user.media]});
            [] ->
                error_logger:warning_msg("User ~p not found when creating video", [CreatorId])
        end,

        {ok, Id}
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, Id}} ->
            spawn(fun() ->
                upload_video_to_ipfs(Id, VideoFile)
            end),
            Id;
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, {transaction_failed, Reason}}
    end.

create_video_concurrent(CreatorId, Title, Description, VideoFile, Duration, Privacy, _Tags, AllowComments, AllowDownloads, Monetized) ->
    IdFuture = spawn_monitor(fun() -> exit({result, nanoid:gen()}) end),
    Id = receive_result(IdFuture),

    Now = calendar:universal_time(),

    AIVideoIdFuture = spawn_monitor(fun() ->
        exit({result, ai_videodb:insert(Id)})
    end),

    CacheFuture = spawn_monitor(fun() ->
        ok = content_cache:set({video_file, Id}, VideoFile),
        exit({result, ok})
    end),

    SizeFuture = spawn_monitor(fun() ->
        exit({result, calculate_content_size(VideoFile)})
    end),

    AI_Video_ID = receive_result(AIVideoIdFuture),
    receive_result(CacheFuture),
    SizeBytes = receive_result(SizeFuture),

    Video = #video{
        id = Id,
        ai_video_id = AI_Video_ID,
        user_id = CreatorId,
        file_url = {pending, Id},
        ipfs_cid = {pending, Id},
        status = processing,
        title = Title,
        description = Description,
        duration_seconds = Duration,
        privacy = Privacy,
        hashtags = _Tags,
        allow_comments = AllowComments,
        allow_downloads = AllowDownloads,
        monetized = Monetized,
        file_size_bytes = SizeBytes,
        date_created = Now,
        date_updated = Now,
        views = 0,
        unique_views = 0,
        likes = [],
        reactions = #{
            like => [],
            love => [],
            wow => [],
            haha => [],
            fire => []
        },
        reaction_counts = #{
            like => 0,
            love => 0,
            wow => 0,
            haha => 0,
            fire => 0
        },
        comments = [],
        shares = 0,
        saves = 0
    },

    case write_video_with_retry(Video, CreatorId, ?MAX_RETRIES) of
        ok ->
            spawn(fun() ->
                upload_video_to_ipfs(Id, VideoFile)
            end),
            Id;
        {error, Reason} ->
            {error, Reason}
    end.

create_video_from_file(CreatorId, Title, Description, FilePath, Duration, Privacy, Tags, AllowComments, Monetized) ->
    case validate_video_file(FilePath) of
        {error, Reason} ->
            {error, Reason};
        {ok, _FileInfo} ->
            case file:read_file(FilePath) of
                {ok, FileContent} ->
                    create_video(CreatorId, Title, Description, FileContent, Duration, Privacy, Tags, AllowComments, true, Monetized);
                {error, Reason} ->
                    {error, {file_read_error, Reason}}
            end
    end.

upload_video_file(VideoId, FilePath) ->
    case validate_video_file(FilePath) of
        {error, Reason} ->
            {error, Reason};
        {ok, _FileInfo} ->
            case file:read_file(FilePath) of
                {ok, FileContent} ->
                    Fun = fun() ->
                        case mnesia:read({video, VideoId}) of
                            [] ->
                                {error, video_not_found};
                            [Video] ->
                                Now = calendar:universal_time(),
                                SizeBytes = byte_size(FileContent),

                                ok = content_cache:set({video_file_update, VideoId}, FileContent),

                                UpdatedVideo = Video#video{
                                    file_url = {pending_update, VideoId},
                                    ipfs_cid = {pending_update, VideoId},
                                    status = processing,
                                    file_size_bytes = SizeBytes,
                                    date_updated = Now
                                },

                                mnesia:write(UpdatedVideo),

                                spawn(fun() ->
                                    upload_video_update(VideoId, FileContent)
                                end),

                                ok
                        end
                    end,

                    case mnesia:transaction(Fun) of
                        {atomic, Result} -> Result;
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end;
                {error, Reason} ->
                    {error, {file_read_error, Reason}}
            end
    end.

update_video(VideoId, CreatorId, NewTitle, NewDescription, NewDuration, NewPrivacy, _NewTags, NewAllowComments, NewAllowDownloads, NewAllowRemixes, NewMonetized) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] ->
                {error, video_not_found};
            [Video] ->
                case Video#video.user_id of
                    CreatorId ->
                        Now = calendar:universal_time(),

                        UpdatedVideo = Video#video{
                            title = NewTitle,
                            description = NewDescription,
                            duration_seconds = NewDuration,
                            privacy = NewPrivacy,
                            hashtags = _NewTags,
                            allow_comments = NewAllowComments,
                            allow_downloads = NewAllowDownloads,
                            allow_remixes = NewAllowRemixes,
                            monetized = NewMonetized,
                            date_updated = Now
                        },

                        mnesia:write(UpdatedVideo),
                        ok;
                    _ ->
                        {error, unauthorized}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

delete_video(VideoId, UserId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] ->
                {error, video_not_found};
            [Video] ->
                case Video#video.user_id of
                    UserId ->
                        mnesia:delete({video, VideoId}),

                        case mnesia:read({user, UserId}) of
                            [User] ->
                                UpdatedMedia = lists:delete(VideoId, User#user.media),
                                mnesia:write(User#user{media = UpdatedMedia});
                            [] -> ok
                        end,

                        ok;
                    _ ->
                        {error, unauthorized}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_video_by_id(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] -> Video
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_videos_by_creator(CreatorId) ->
    Fun = fun() ->
        mnesia:match_object(#video{user_id = CreatorId, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_videos_by_business(BusinessId) ->
    Fun = fun() ->
        mnesia:match_object(#video{business_id = BusinessId, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_public_videos() ->
    Fun = fun() ->
        mnesia:match_object(#video{privacy = public, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_videos_by_tag(Tag) ->
    Fun = fun() ->
        AllVideos = mnesia:match_object(#video{_ = '_'}),
        lists:filter(fun(Video) ->
            lists:member(Tag, Video#video.hashtags)
        end, AllVideos)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

    get_video_content(VideoId) ->
        Fun = fun() ->
            case mnesia:read({video, VideoId}) of
                [] -> {error, video_not_found};
                [Video] ->
                    IPFSCID = Video#video.ipfs_cid,
                    case IPFSCID of
                        {pending, Id} when Id =:= VideoId ->
                            case content_cache:get({video_file, Id}) of
                                undefined -> {error, content_not_ready};
                                CachedContent -> {ok, CachedContent}
                            end;
                        _ ->
                            try
                                ActualContent = ipfs_video:get_video_binary(IPFSCID),
                                {ok, ActualContent}
                            catch
                                _:Error -> {error, Error}
                            end
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, {ok, Content}} -> Content;
            {atomic, {error, Reason}} -> {error, Reason};
            Error -> Error
        end.

get_video_metadata(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] -> {ok, Video#video.data}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_video_qualities(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] -> {ok, Video#video.available_qualities}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

create_video_version(VideoId, UserId, NewVideoFile, ChangeDescription) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] ->
                {error, video_not_found};
            [Video] ->
                case Video#video.user_id of
                    UserId ->
                        Now = calendar:universal_time(),

                        content_cache:set({video_version, VideoId}, NewVideoFile),

                        VersionNum = length(maps:get(version_history, Video#video.data, [])) + 1,
                        NewVersionEntry = {VersionNum, {pending_version, VideoId}, Now, ChangeDescription},

                        CurrentData = Video#video.data,
                        VersionHistory = maps:get(version_history, CurrentData, []),
                        UpdatedVersionHistory = [NewVersionEntry | VersionHistory],
                        UpdatedData = maps:put(version_history, UpdatedVersionHistory, CurrentData),

                        UpdatedVideo = Video#video{
                            ipfs_cid = {pending_version, VideoId},
                            status = processing,
                            data = UpdatedData,
                            date_updated = Now
                        },

                        mnesia:write(UpdatedVideo),

                        spawn(fun() ->
                            upload_video_version(VideoId, NewVideoFile)
                        end),

                        {ok, VersionNum};
                    _ ->
                        {error, unauthorized}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_video_versions(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                VersionHistory = maps:get(version_history, Video#video.data, []),
                {ok, VersionHistory}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_thumbnail(VideoId, ThumbnailData, Timestamp) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                ThumbnailCID = ipfs_media:upload_media(ThumbnailData),

                ThumbnailCIDs = Video#video.thumbnail_cids,
                UpdatedThumbnailCIDs = [ThumbnailCID | ThumbnailCIDs],

                mnesia:write(Video#video{
                    thumbnail_cids = UpdatedThumbnailCIDs,
                    thumbnail_url = ThumbnailCID,
                    thumbnail_timestamp = Timestamp
                }),

                {ok, ThumbnailCID}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_preview(VideoId, PreviewData) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                PreviewCID = ipfs_media:upload_media(PreviewData),

                mnesia:write(Video#video{preview_cid = PreviewCID}),

                {ok, PreviewCID}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_chapter(VideoId, Title, StartTime, EndTime) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                ChapterId = nanoid:gen(),
                Chapter = {ChapterId, Title, StartTime, EndTime},

                Chapters = Video#video.chapters,
                UpdatedChapters = [Chapter | Chapters],

                mnesia:write(Video#video{chapters = UpdatedChapters}),

                {ok, ChapterId}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_subtitle(VideoId, Language, SubtitleData, Format) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                SubtitleCID = ipfs_content:upload_text(SubtitleData),

                Subtitle = {Language, SubtitleCID, Format},
                Subtitles = Video#video.subtitles,
                UpdatedSubtitles = [Subtitle | Subtitles],

                mnesia:write(Video#video{subtitles = UpdatedSubtitles}),

                {ok, SubtitleCID}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_audio_track(VideoId, Language, AudioData, TrackType) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                AudioCID = ipfs_media:upload_media(AudioData),

                AudioTrack = {Language, AudioCID, TrackType},
                AudioTracks = Video#video.audio_tracks,
                UpdatedAudioTracks = [AudioTrack | AudioTracks],

                mnesia:write(Video#video{audio_tracks = UpdatedAudioTracks}),

                {ok, AudioCID}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

react_to_video(UserID, VideoId, ReactionType) ->
    ValidReactions = [like, love, wow, haha, fire],
    case lists:member(ReactionType, ValidReactions) of
        false -> {error, invalid_reaction_type};
        true ->
            Fun = fun() ->
                case mnesia:read({video, VideoId}) of
                    [] -> {error, video_not_found};
                    [Video] ->
                        ExistingReaction = find_user_reaction_in_video(Video, UserID),

                        case ExistingReaction of
                            {found, OldType, OldLikeID} when OldType =:= ReactionType ->
                                Reactions = Video#video.reactions,
                                ReactionList = maps:get(OldType, Reactions, []),
                                UpdatedList = lists:delete(OldLikeID, ReactionList),
                                UpdatedReactions = maps:put(OldType, UpdatedList, Reactions),

                                ReactionCounts = Video#video.reaction_counts,
                                CurrentCount = maps:get(OldType, ReactionCounts, 0),
                                UpdatedCount = max(0, CurrentCount - 1),
                                UpdatedReactionCounts = maps:put(OldType, UpdatedCount, ReactionCounts),

                                mnesia:delete({like, OldLikeID}),

                                mnesia:write(Video#video{
                                    reactions = UpdatedReactions,
                                    reaction_counts = UpdatedReactionCounts
                                }),
                                {removed, OldType};
                            {found, OldType, OldLikeID} ->
                                Reactions = Video#video.reactions,
                                OldReactionList = maps:get(OldType, Reactions, []),
                                UpdatedOldList = lists:delete(OldLikeID, OldReactionList),
                                IntermediateReactions = maps:put(OldType, UpdatedOldList, Reactions),

                                ReactionCounts = Video#video.reaction_counts,
                                OldCount = maps:get(OldType, ReactionCounts, 0),
                                UpdatedOldCount = max(0, OldCount - 1),
                                IntermediateReactionCounts = maps:put(OldType, UpdatedOldCount, ReactionCounts),

                                mnesia:delete({like, OldLikeID}),

                                ID = nanoid:gen(),
                                mnesia:write(#like{
                                    id = ID,
                                    userID = UserID,
                                    reaction_type = ReactionType,
                                    date_created = calendar:universal_time()
                                }),

                                NewReactionList = maps:get(ReactionType, IntermediateReactions, []),
                                FinalReactions = maps:put(ReactionType, [ID | NewReactionList], IntermediateReactions),

                                NewCount = maps:get(ReactionType, IntermediateReactionCounts, 0),
                                FinalReactionCounts = maps:put(ReactionType, NewCount + 1, IntermediateReactionCounts),

                                mnesia:write(Video#video{
                                    reactions = FinalReactions,
                                    reaction_counts = FinalReactionCounts
                                }),
                                ID;
                            not_found ->
                                ID = nanoid:gen(),
                                mnesia:write(#like{
                                    id = ID,
                                    userID = UserID,
                                    reaction_type = ReactionType,
                                    date_created = calendar:universal_time()
                                }),

                                Reactions = Video#video.reactions,
                                ReactionList = maps:get(ReactionType, Reactions, []),
                                UpdatedReactions = maps:put(ReactionType, [ID | ReactionList], Reactions),

                                ReactionCounts = Video#video.reaction_counts,
                                CurrentCount = maps:get(ReactionType, ReactionCounts, 0),
                                UpdatedReactionCounts = maps:put(ReactionType, CurrentCount + 1, ReactionCounts),

                                mnesia:write(Video#video{
                                    reactions = UpdatedReactions,
                                    reaction_counts = UpdatedReactionCounts
                                }),
                                ID
                        end
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end
    end.

find_user_reaction_in_video(Video, UserID) ->
    Reactions = Video#video.reactions,
    ReactionTypes = [like, love, wow, haha, fire],
    find_user_reaction_in_types(UserID, ReactionTypes, Reactions).

find_user_reaction_in_types(_UserID, [], _Reactions) ->
    not_found;
find_user_reaction_in_types(UserID, [Type | Rest], Reactions) ->
    ReactionList = maps:get(Type, Reactions, []),
    case lists:any(fun(ID) ->
        case mnesia:read({like, ID}) of
            [Like] -> Like#like.userID =:= UserID;
            [] -> false
        end
    end, ReactionList) of
        true ->
            LikeID = lists:foldl(fun(ID, Acc) ->
                case mnesia:read({like, ID}) of
                    [Like] when Like#like.userID =:= UserID -> ID;
                    _ -> Acc
                end
            end, undefined, ReactionList),
            {found, Type, LikeID};
        false ->
            find_user_reaction_in_types(UserID, Rest, Reactions)
    end.

remove_reaction_from_video(LikeID, VideoId) ->
    Fun = fun() ->
        case mnesia:read({like, LikeID}) of
            [] -> {error, reaction_not_found};
            [Like] ->
                ReactionType = Like#like.reaction_type,

                case mnesia:read({video, VideoId}) of
                    [] -> {error, video_not_found};
                    [Video] ->
                        Reactions = Video#video.reactions,
                        ReactionList = maps:get(ReactionType, Reactions, []),
                        UpdatedReactionList = lists:delete(LikeID, ReactionList),
                        UpdatedReactions = maps:put(ReactionType, UpdatedReactionList, Reactions),

                        ReactionCounts = Video#video.reaction_counts,
                        CurrentCount = maps:get(ReactionType, ReactionCounts, 0),
                        NewCount = max(0, CurrentCount - 1),
                        UpdatedReactionCounts = maps:put(ReactionType, NewCount, ReactionCounts),

                        mnesia:write(Video#video{
                            reactions = UpdatedReactions,
                            reaction_counts = UpdatedReactionCounts
                        }),

                        mnesia:delete({like, LikeID}),
                        ok
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_reactions_by_type(VideoId, ReactionType) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> [];
            [Video] ->
                Reactions = Video#video.reactions,
                ReactionList = maps:get(ReactionType, Reactions, []),
                lists:foldl(fun(ID, Acc) ->
                    case mnesia:read({like, ID}) of
                        [Like] -> [Like | Acc];
                        [] -> Acc
                    end
                end, [], ReactionList)
        end
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_all_reactions(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> #{};
            [Video] ->
                Reactions = Video#video.reactions,
                maps:map(fun(_ReactionType, ReactionList) ->
                    lists:foldl(fun(ID, Acc) ->
                        case mnesia:read({like, ID}) of
                            [Like] -> [Like | Acc];
                            [] -> Acc
                        end
                    end, [], ReactionList)
                end, Reactions)
        end
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_reaction_counts(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> #{};
            [Video] -> Video#video.reaction_counts
        end
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

has_user_reacted_with_type(UserID, VideoId, ReactionType) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> false;
            [Video] ->
                Reactions = Video#video.reactions,
                ReactionList = maps:get(ReactionType, Reactions, []),
                lists:any(fun(ID) ->
                    case mnesia:read({like, ID}) of
                        [Like] -> Like#like.userID =:= UserID;
                        [] -> false
                    end
                end, ReactionList)
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> false
    end.

get_user_reaction_type(UserID, VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> undefined;
            [Video] ->
                Reactions = Video#video.reactions,
                find_user_reaction_type(UserID, Reactions)
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> undefined
    end.

find_user_reaction_type(UserID, Reactions) ->
    ReactionTypes = [like, love, wow, haha, fire],
    find_user_reaction_type_in_types(UserID, ReactionTypes, Reactions).

find_user_reaction_type_in_types(_UserID, [], _Reactions) ->
    undefined;
find_user_reaction_type_in_types(UserID, [Type | Rest], Reactions) ->
    ReactionList = maps:get(Type, Reactions, []),
    case lists:any(fun(ID) ->
        case mnesia:read({like, ID}) of
            [Like] -> Like#like.userID =:= UserID;
            [] -> false
        end
    end, ReactionList) of
        true -> Type;
        false -> find_user_reaction_type_in_types(UserID, Rest, Reactions)
    end.

    add_video_comment(Author, VideoId, Content) ->
        Fun = fun() ->
            Id = nanoid:gen(),
            Date = calendar:universal_time(),
            UserID = userdb:get_user_id(Author),
            ContentToCache = if
                is_binary(Content) -> binary_to_list(Content);
                true -> Content
            end,
            ok = content_cache:set(Id, ContentToCache),
            PlaceholderContent = Id,
            case mnesia:read({video, VideoId}) of
                [] ->
                    {error, video_not_found};
                [Video] ->
                    case Video#video.allow_comments of
                        false -> {error, comments_disabled};
                        true ->
                            Comment = #comment{
                                id = Id,
                                user_id = UserID,
                                post = VideoId,
                                author = Author,
                                content = PlaceholderContent,
                                date_created = Date,
                                content_status = processing
                            },
                            mnesia:write(Comment),
                            CurrentComments = case Video#video.comments of
                                undefined -> [];
                                L when is_list(L) -> L;
                                _ -> []
                            end,
                            UpdatedComments = [Id | CurrentComments],
                            mnesia:write(Video#video{comments = UpdatedComments}),
                            case update_user_activity(Author, Date) of
                                {error, Reason} ->
                                    error_logger:warning_msg("Failed to update activity for ~p: ~p", [Author, Reason]),
                                    ok;
                                _ -> ok
                            end,
                            {ok, Id}
                    end
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, {ok, Id}} ->
                spawn(fun() ->
                    ContentToUse = content_cache:get(Id),
                    CIDString = case ContentToUse of
                        "" -> "";
                        _ -> ipfs_content:upload_text(ContentToUse)
                    end,
                    UpdateF = fun() ->
                        case mnesia:read({comment, Id}) of
                            [CommentToUpdate] ->
                                UpdatedComment = CommentToUpdate#comment{
                                    content = CIDString,
                                    content_status = ready
                                },
                                mnesia:write(UpdatedComment);
                            [] -> ok
                        end
                    end,
                    mnesia:transaction(UpdateF),
                    content_cache:delete(Id),
                    spawn(fun() ->
                        timer:sleep(5000),
                        case CIDString of
                            "" ->
                                ok;
                            _ ->
                                try
                                    {ok, #{id := _KeyID, name := _}} = ipfs_client_4:key_gen("video_comment_" ++ Id),
                                    PublishOptions = [
                                        {key, "video_comment_" ++ Id},
                                        {resolve, true},
                                        {lifetime, "12h0m0s"},
                                        {ttl, "1m0s"},
                                        {v1compat, true},
                                        {ipns_base, "base36"}
                                    ],
                                    case ipfs_client_5:name_publish("/ipfs/" ++ CIDString, PublishOptions) of
                                        {ok, #{name := IPNSKey}} ->
                                            update_comment_ipns(Id, IPNSKey);
                                        {error, _} ->
                                            ok
                                    end
                                catch
                                    _:_ -> ok
                                end
                        end
                    end)
                end),
                Id;
            {atomic, {error, Reason}} ->
                {error, Reason};
            {aborted, Reason} ->
                {error, {transaction_failed, Reason}}
        end.

    get_video_comment_content(CommentID) ->
        Parent = self(),
        Ref = make_ref(),
        spawn(fun() ->
            Result = read_comment_and_fetch_content(CommentID),
            Parent ! {Ref, Result}
        end),
        receive
            {Ref, {ok, Content}} -> Content;
            {Ref, OtherResult} -> OtherResult
        after 10000 ->
            {error, timeout}
        end.

    read_comment_and_fetch_content(CommentID) ->
        ReadFun = fun() ->
            case mnesia:read({comment, CommentID}) of
                [] ->
                    {error, comment_not_found};
                [Comment] ->
                    {ok, Comment}
            end
        end,
        case mnesia:transaction(ReadFun) of
            {atomic, {error, Reason}} ->
                {error, Reason};
            {atomic, {ok, Comment}} ->
                Content = Comment#comment.content,
                ContentStatus = case Comment#comment.content_status of
                    undefined -> processing;
                    Status -> Status
                end,
                fetch_comment_content(Content, CommentID, ContentStatus);
            Error ->
                Error
        end.

    fetch_comment_content(Content, CommentID, processing) ->
        case content_cache:get(CommentID) of
            undefined ->
                timer:sleep(1000),
                case content_cache:get(CommentID) of
                    undefined -> {error, content_processing};
                    CachedContent -> {ok, CachedContent}
                end;
            CachedContent -> {ok, CachedContent}
        end;
    fetch_comment_content(Content, CommentID, ready) when Content =:= CommentID ->
        case content_cache:get(CommentID) of
            undefined -> {error, content_cache_missing};
            CachedContent -> {ok, CachedContent}
        end;
    fetch_comment_content(Content, _CommentID, ready) ->
        IpfsRef = make_ref(),
        Parent = self(),
        IpfsWorker = spawn(fun() ->
            try
                Result = ipfs_content:get_text_content(Content),
                Parent ! {IpfsRef, {ok, Result}}
            catch
                _:Error ->
                    Parent ! {IpfsRef, {error, Error}}
            end
        end),
        MonitorRef = monitor(process, IpfsWorker),
        receive
            {IpfsRef, Result} ->
                demonitor(MonitorRef, [flush]),
                Result;
            {'DOWN', MonitorRef, process, IpfsWorker, normal} ->
                {error, ipfs_missing_result};
            {'DOWN', MonitorRef, process, IpfsWorker, Reason} ->
                {error, {ipfs_worker_crashed, Reason}}
        after 15000 ->
            exit(IpfsWorker, kill),
            demonitor(MonitorRef, [flush]),
            {error, ipfs_timeout}
        end;
    fetch_comment_content(Content, CommentID, _Status) ->
        fetch_comment_content(Content, CommentID, ready).

    update_user_activity(Author, Date) ->
        case userdb:get_user(Author) of
            [] -> {error, user_not_found};
            User when is_record(User, user) ->
                userdb:update_last_activity(User#user.id, Date);
            _ -> {error, invalid_user}
        end.

    update_comment_ipns(CommentId, IPNSKey) ->
        UpdateF = fun() ->
            case mnesia:read({comment, CommentId}) of
                [Comment] ->
                    UpdatedComment = Comment#comment{ipns = IPNSKey},
                    mnesia:write(UpdatedComment),
                    ok;
                [] ->
                    {error, not_found}
            end
        end,
        case mnesia:transaction(UpdateF) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} ->
                error_logger:error_msg("Failed to update comment ~p with IPNS: ~p", [CommentId, Reason]),
                {error, Reason};
            {aborted, Reason} ->
                error_logger:error_msg("Transaction aborted while updating comment ~p with IPNS: ~p", [CommentId, Reason]),
                {error, {transaction_failed, Reason}}
        end.

        update_video_comment(CommentID, NewContent) ->
            Fun = fun() ->
                case mnesia:read({comment, CommentID}) of
                    [] ->
                        {error, comment_not_found};
                    [Comment] ->
                        VideoId = Comment#comment.post,
                        case mnesia:read({video, VideoId}) of
                            [] ->
                                {error, invalid_comment_target};
                            [_Video] ->
                                ProcessingComment = Comment#comment{
                                    content = CommentID,
                                    content_status = processing
                                },
                                mnesia:write(ProcessingComment),
                                ContentToCache = case is_binary(NewContent) of
                                    true  -> binary_to_list(NewContent);
                                    false -> NewContent
                                end,
                                ok = content_cache:set(CommentID, ContentToCache),
                                spawn(fun() ->
                                    CIDString = case ContentToCache of
                                        "" -> "";
                                        _  -> ipfs_content:upload_text(ContentToCache)
                                    end,
                                    UpdateF = fun() ->
                                        case mnesia:read({comment, CommentID}) of
                                            [CommentToUpdate] ->
                                                FinalComment = CommentToUpdate#comment{
                                                    content = CIDString,
                                                    content_status = ready
                                                },
                                                mnesia:write(FinalComment);
                                            [] -> ok
                                        end
                                    end,
                                    mnesia:transaction(UpdateF),
                                    content_cache:delete(CommentID)
                                end),
                                {ok, CommentID}
                        end
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

            delete_video_comment(CommentID, VideoId) ->
                Fun = fun() ->
                    case mnesia:read({video, VideoId}) of
                        [] ->
                            {error, video_not_found};
                        [Video] ->
                            case lists:member(CommentID, Video#video.comments) of
                                false ->
                                    {error, comment_not_found};
                                true ->
                                    UpdatedComments = lists:delete(CommentID, Video#video.comments),
                                    mnesia:write(Video#video{comments = UpdatedComments}),

                                    mnesia:delete({comment, CommentID}),

                                    {ok, comment_deleted}
                            end
                    end
                end,
                case mnesia:transaction(Fun) of
                    {atomic, {ok, comment_deleted}} ->
                        {ok, comment_deleted};
                    {atomic, {error, Reason}} ->
                        {error, Reason};
                    {aborted, Reason} ->
                        {error, {transaction_failed, Reason}}
                end.

                react_to_video_comment(UserID, CommentId, ReactionType) ->
                    ValidReactions = [like, love, wow, haha, fire, celebrate, support, insightful, funny],
                    case lists:member(ReactionType, ValidReactions) of
                        false -> {error, invalid_reaction_type};
                        true ->
                            Fun = fun() ->
                                case mnesia:read({comment, CommentId}) of
                                    [] -> {error, comment_not_found};
                                    [Comment] ->
                                        ExistingReaction = find_user_reaction_in_comment(Comment, UserID),

                                        case ExistingReaction of
                                            {found, OldType, OldLikeID} when OldType =:= ReactionType ->
                                                Reactions = Comment#comment.reactions,
                                                ReactionList = maps:get(OldType, Reactions, []),
                                                UpdatedList = lists:delete(OldLikeID, ReactionList),
                                                UpdatedReactions = maps:put(OldType, UpdatedList, Reactions),

                                                ReactionCounts = Comment#comment.reaction_counts,
                                                CurrentCount = maps:get(OldType, ReactionCounts, 0),
                                                UpdatedCount = max(0, CurrentCount - 1),
                                                UpdatedReactionCounts = maps:put(OldType, UpdatedCount, ReactionCounts),

                                                mnesia:delete({like, OldLikeID}),

                                                mnesia:write(Comment#comment{
                                                    reactions = UpdatedReactions,
                                                    reaction_counts = UpdatedReactionCounts
                                                }),
                                                {removed, OldType};
                                            {found, OldType, OldLikeID} ->
                                                Reactions = Comment#comment.reactions,
                                                OldReactionList = maps:get(OldType, Reactions, []),
                                                UpdatedOldList = lists:delete(OldLikeID, OldReactionList),
                                                IntermediateReactions = maps:put(OldType, UpdatedOldList, Reactions),

                                                ReactionCounts = Comment#comment.reaction_counts,
                                                OldCount = maps:get(OldType, ReactionCounts, 0),
                                                UpdatedOldCount = max(0, OldCount - 1),
                                                IntermediateReactionCounts = maps:put(OldType, UpdatedOldCount, ReactionCounts),

                                                mnesia:delete({like, OldLikeID}),

                                                ID = nanoid:gen(),
                                                mnesia:write(#like{
                                                    id = ID,
                                                    comment = CommentId,
                                                    userID = UserID,
                                                    reaction_type = ReactionType,
                                                    date_created = calendar:universal_time()
                                                }),

                                                NewReactionList = maps:get(ReactionType, IntermediateReactions, []),
                                                FinalReactions = maps:put(ReactionType, [ID | NewReactionList], IntermediateReactions),

                                                NewCount = maps:get(ReactionType, IntermediateReactionCounts, 0),
                                                FinalReactionCounts = maps:put(ReactionType, NewCount + 1, IntermediateReactionCounts),

                                                mnesia:write(Comment#comment{
                                                    reactions = FinalReactions,
                                                    reaction_counts = FinalReactionCounts
                                                }),
                                                ID;
                                            not_found ->
                                                ID = nanoid:gen(),
                                                mnesia:write(#like{
                                                    id = ID,
                                                    comment = CommentId,
                                                    userID = UserID,
                                                    reaction_type = ReactionType,
                                                    date_created = calendar:universal_time()
                                                }),

                                                Reactions = Comment#comment.reactions,
                                                ReactionList = maps:get(ReactionType, Reactions, []),
                                                UpdatedReactions = maps:put(ReactionType, [ID | ReactionList], Reactions),

                                                ReactionCounts = Comment#comment.reaction_counts,
                                                CurrentCount = maps:get(ReactionType, ReactionCounts, 0),
                                                UpdatedReactionCounts = maps:put(ReactionType, CurrentCount + 1, ReactionCounts),

                                                mnesia:write(Comment#comment{
                                                    reactions = UpdatedReactions,
                                                    reaction_counts = UpdatedReactionCounts
                                                }),
                                                ID
                                        end
                                end
                            end,
                            case mnesia:transaction(Fun) of
                                {atomic, Result} -> Result;
                                {aborted, Reason} -> {error, {transaction_failed, Reason}}
                            end
                    end.

                find_user_reaction_in_comment(Comment, UserID) ->
                    Reactions = Comment#comment.reactions,
                    ReactionTypes = [like, love, wow, haha, fire, celebrate, support, insightful, funny],
                    find_user_reaction_in_comment_types(UserID, ReactionTypes, Reactions).

                find_user_reaction_in_comment_types(_UserID, [], _Reactions) ->
                    not_found;
                find_user_reaction_in_comment_types(UserID, [Type | Rest], Reactions) ->
                    ReactionList = maps:get(Type, Reactions, []),
                    case lists:any(fun(ID) ->
                        case mnesia:read({like, ID}) of
                            [Like] -> Like#like.userID =:= UserID;
                            [] -> false
                        end
                    end, ReactionList) of
                        true ->
                            LikeID = lists:foldl(fun(ID, Acc) ->
                                case mnesia:read({like, ID}) of
                                    [Like] when Like#like.userID =:= UserID -> ID;
                                    _ -> Acc
                                end
                            end, undefined, ReactionList),
                            {found, Type, LikeID};
                        false ->
                            find_user_reaction_in_comment_types(UserID, Rest, Reactions)
                    end.

                get_comment_reactions(CommentId) ->
                    Fun = fun() ->
                        case mnesia:read({comment, CommentId}) of
                            [] -> #{};
                            [Comment] -> Comment#comment.reaction_counts
                        end
                    end,
                    {atomic, Res} = mnesia:transaction(Fun),
                    Res.

                get_user_comment_reaction(UserID, CommentId) ->
                    Fun = fun() ->
                        case mnesia:read({comment, CommentId}) of
                            [] -> undefined;
                            [Comment] ->
                                Reactions = Comment#comment.reactions,
                                find_user_comment_reaction_type(UserID, Reactions)
                        end
                    end,

                    case mnesia:transaction(Fun) of
                        {atomic, Result} -> Result;
                        {aborted, _Reason} -> undefined
                    end.

                find_user_comment_reaction_type(UserID, Reactions) ->
                    ReactionTypes = [like, love, wow, haha, fire, celebrate, support, insightful, funny],
                    find_user_comment_reaction_type_in_types(UserID, ReactionTypes, Reactions).

                find_user_comment_reaction_type_in_types(_UserID, [], _Reactions) ->
                    undefined;
                find_user_comment_reaction_type_in_types(UserID, [Type | Rest], Reactions) ->
                    ReactionList = maps:get(Type, Reactions, []),
                    case lists:any(fun(ID) ->
                        case mnesia:read({like, ID}) of
                            [Like] -> Like#like.userID =:= UserID;
                            [] -> false
                        end
                    end, ReactionList) of
                        true -> Type;
                        false -> find_user_comment_reaction_type_in_types(UserID, Rest, Reactions)
                    end.

get_video_comments(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> [];
            [Video] ->
                CommentIds = Video#video.comments,
                lists:foldl(fun(CommentId, Acc) ->
                    case mnesia:read({comment, CommentId}) of
                        [Comment] -> [Comment | Acc];
                        [] -> Acc
                    end
                end, [], CommentIds)
        end
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_comment_count(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> 0;
            [Video] -> length(Video#video.comments)
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Count} -> Count;
        {aborted, _Reason} -> 0
    end.

increment_view_count(VideoId, UserId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                ViewId = nanoid:gen(),
                Now = calendar:universal_time(),

                View = #media_view{
                    id = ViewId,
                    media_id = VideoId,
                    media_type = video,
                    user_id = UserId,
                    view_duration = 0,
                    completion_percentage = 0,
                    date_viewed = Now
                },
                mnesia:write(View),

                mnesia:write(Video#video{views = Video#video.views + 1}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

    increment_unique_view(VideoId, UserId) when is_list(UserId) orelse is_binary(UserId) ->
        UserIdStr = case is_binary(UserId) of
            true -> binary_to_list(UserId);
            false -> UserId
        end,
        Fun = fun() ->
            case mnesia:read({video, VideoId}) of
                [] -> {error, video_not_found};
                [Video] ->
                    UniqueViewers = case Video#video.unique_viewers of
                        undefined -> [];
                        List when is_list(List) -> List;
                        _ -> []
                    end,
                    case lists:member(UserIdStr, UniqueViewers) of
                        true ->
                            {error, already_counted};
                        false ->
                            NewUniqueViewers = [UserIdStr | UniqueViewers],
                            NewCount = length(NewUniqueViewers),
                            mnesia:write(Video#video{
                                unique_viewers = NewUniqueViewers,
                                unique_views = NewCount
                            }),
                            ok
                    end
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

        get_unique_view_count(VideoId) ->
            Fun = fun() ->
                case mnesia:read({video, VideoId}) of
                    [] -> {error, video_not_found};
                    [Video] -> {ok, Video#video.unique_views}
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

track_watch_time(VideoId, _UserId, WatchSeconds) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                TotalWatchTime = Video#video.watch_time_total + WatchSeconds,
                Duration = Video#video.duration_seconds,

                AvgPercentage = if
                    Duration > 0 andalso Video#video.views > 0 ->
                        (TotalWatchTime / (Duration * Video#video.views)) * 100;
                    true -> 0.0
                end,

                mnesia:write(Video#video{
                    watch_time_total = TotalWatchTime,
                    avg_watch_percentage = AvgPercentage
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_view_stats(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                {ok, #{
                    views => Video#video.views,
                    unique_views => Video#video.unique_views,
                    watch_time_total => Video#video.watch_time_total,
                    avg_watch_percentage => Video#video.avg_watch_percentage
                }}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

    share_video(VideoId, UserId) ->
            share_video(VideoId, UserId, "").

            share_video(VideoId, UserId, Description) ->
                Fun = fun() ->
                    case mnesia:read({video, VideoId}) of
                        [] -> {error, video_not_found};
                        [Video] ->
                            mnesia:write(Video#video{shares = Video#video.shares + 1}),

                            case mnesia:read({user, UserId}) of
                                [] ->
                                    {error, user_not_found};
                                [User] ->
                                    Username = case User#user.username of
                                        U when is_list(U) -> U;
                                        U when is_binary(U) -> binary_to_list(U);
                                        _ -> "Unknown"
                                    end,

                                    VideoIdStr = if
                                        is_list(VideoId) -> VideoId;
                                        is_binary(VideoId) -> binary_to_list(VideoId);
                                        true -> VideoId
                                    end,

                                    VideoTitle = if
                                        is_binary(Video#video.title) -> Video#video.title;
                                        is_list(Video#video.title) -> list_to_binary(Video#video.title);
                                        true -> <<"Shared Video">>
                                    end,

                                    VideoUrl = "https://mazaryn.io/en/videos/" ++ VideoIdStr,

                                    Content = case Description of
                                        "" ->
                                            iolist_to_binary([
                                                "VIDEO_SHARE:", VideoIdStr, "|", VideoUrl, "|", VideoTitle, "\n"
                                            ]);
                                        Desc when is_binary(Desc) andalso byte_size(Desc) > 0 ->
                                            iolist_to_binary([
                                                "VIDEO_SHARE:", VideoIdStr, "|", VideoUrl, "|", VideoTitle, "\n",
                                                Desc
                                            ]);
                                        Desc when is_list(Desc) andalso length(Desc) > 0 ->
                                            DescBin = list_to_binary(Desc),
                                            iolist_to_binary([
                                                "VIDEO_SHARE:", VideoIdStr, "|", VideoUrl, "|", VideoTitle, "\n",
                                                DescBin
                                            ]);
                                        _ ->
                                            iolist_to_binary([
                                                "VIDEO_SHARE:", VideoIdStr, "|", VideoUrl, "|", VideoTitle, "\n"
                                            ])
                                    end,

                                    ThumbnailUrl = case Video#video.thumbnail_url of
                                        undefined -> undefined;
                                        "" -> undefined;
                                        Thumb when is_list(Thumb) -> Thumb;
                                        Thumb when is_binary(Thumb) -> binary_to_list(Thumb);
                                        _ -> undefined
                                    end,

                                    {ok, #{
                                        username => Username,
                                        content => binary_to_list(Content),
                                        media => ThumbnailUrl,
                                        video_id => VideoIdStr,
                                        video_url => VideoUrl,
                                        video_title => binary_to_list(VideoTitle),
                                        hashtags => [<<"video">>, <<"shared">>],
                                        emoji => [],
                                        mention => []
                                    }}
                            end
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, {ok, PostData}} ->
                        AuthorUsername = maps:get(username, PostData),
                        Content = maps:get(content, PostData),
                        Media = maps:get(media, PostData),
                        Hashtags = maps:get(hashtags, PostData),
                        Emoji = maps:get(emoji, PostData),
                        Mention = maps:get(mention, PostData),

                        case postdb:insert(AuthorUsername, Content, Media, Hashtags, "", Emoji, Mention) of
                            PostId when is_list(PostId) orelse is_binary(PostId) ->
                                {ok, PostId};
                            {error, Reason} ->
                                {error, {post_creation_failed, Reason}}
                        end;
                    {atomic, {error, Reason}} ->
                        {error, Reason};
                    {aborted, Reason} ->
                        {error, {transaction_failed, Reason}}
                end.

save_video(VideoId, UserId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{saves = Video#video.saves + 1}),

                case mnesia:read({user, UserId}) of
                    [User] ->
                        SavedPosts = User#user.saved_posts,
                        mnesia:write(User#user{saved_posts = [VideoId | SavedPosts]});
                    [] -> ok
                end,

                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

unsave_video(VideoId, UserId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{saves = max(0, Video#video.saves - 1)}),

                case mnesia:read({user, UserId}) of
                    [User] ->
                        SavedPosts = User#user.saved_posts,
                        UpdatedSavedPosts = lists:delete(VideoId, SavedPosts),
                        mnesia:write(User#user{saved_posts = UpdatedSavedPosts});
                    [] -> ok
                end,

                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_to_playlist(VideoId, PlaylistId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                PlaylistIds = Video#video.playlist_ids,
                UpdatedPlaylistIds = case lists:member(PlaylistId, PlaylistIds) of
                    true -> PlaylistIds;
                    false -> [PlaylistId | PlaylistIds]
                end,

                CurrentData = Video#video.data,
                PlaylistCount = maps:get(added_to_playlists, CurrentData, 0),
                UpdatedData = maps:put(added_to_playlists, PlaylistCount + 1, CurrentData),

                mnesia:write(Video#video{
                    playlist_ids = UpdatedPlaylistIds,
                    data = UpdatedData
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

remove_from_playlist(VideoId, PlaylistId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                PlaylistIds = Video#video.playlist_ids,
                UpdatedPlaylistIds = lists:delete(PlaylistId, PlaylistIds),

                mnesia:write(Video#video{playlist_ids = UpdatedPlaylistIds}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

link_to_series(VideoId, SeriesId, EpisodeNumber) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{
                    series_id = SeriesId,
                    episode_number = EpisodeNumber
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_interactive_hotspot(VideoId, Timestamp, Duration, Position, Content) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                HotspotId = nanoid:gen(),

                Hotspot = #video_hotspot{
                    id = HotspotId,
                    video_id = VideoId,
                    timestamp = Timestamp,
                    duration = Duration,
                    position = Position,
                    hotspot_type = interactive,
                    content = Content
                },
                mnesia:write(Hotspot),

                InteractiveElements = Video#video.interactive_elements,
                mnesia:write(Video#video{
                    interactive_elements = [HotspotId | InteractiveElements]
                }),

                {ok, HotspotId}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

create_poll(VideoId, Timestamp, Question, Options) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                PollId = nanoid:gen(),

                Poll = #interactive_poll{
                    id = PollId,
                    video_id = VideoId,
                    timestamp = Timestamp,
                    question = Question,
                    options = Options,
                    total_votes = 0
                },
                mnesia:write(Poll),

                InteractiveElements = Video#video.interactive_elements,
                mnesia:write(Video#video{
                    interactive_elements = [PollId | InteractiveElements]
                }),

                {ok, PollId}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

vote_on_poll(PollId, _UserId, OptionIndex) ->
    Fun = fun() ->
        case mnesia:read({interactive_poll, PollId}) of
            [] -> {error, poll_not_found};
            [Poll] ->
                Options = Poll#interactive_poll.options,
                UpdatedOptions = lists:map(fun({Idx, Text, Votes}) ->
                    if
                        Idx =:= OptionIndex -> {Idx, Text, Votes + 1};
                        true -> {Idx, Text, Votes}
                    end
                end, Options),

                mnesia:write(Poll#interactive_poll{
                    options = UpdatedOptions,
                    total_votes = Poll#interactive_poll.total_votes + 1
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

set_monetization(VideoId, RevenueModel, Price) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{
                    monetized = true,
                    revenue_model = RevenueModel,
                    price = Price
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_ad_break(VideoId, Timestamp, AdType) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                AdBreak = {Timestamp, AdType},
                AdBreaks = Video#video.ad_breaks,
                UpdatedAdBreaks = [AdBreak | AdBreaks],

                mnesia:write(Video#video{ad_breaks = UpdatedAdBreaks}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

set_sponsor_info(VideoId, SponsorInfo) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{sponsor_info = SponsorInfo}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

track_viewer_retention(VideoId, Timestamp, PercentageViewing) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                RetentionData = {Timestamp, PercentageViewing},
                ViewerRetention = Video#video.viewer_retention,
                UpdatedRetention = [RetentionData | ViewerRetention],

                mnesia:write(Video#video{viewer_retention = UpdatedRetention}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

track_geographic_view(VideoId, Country, City) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                GeographicViews = Video#video.geographic_views,
                Key = {Country, City},
                CurrentCount = maps:get(Key, GeographicViews, 0),
                UpdatedGeographicViews = maps:put(Key, CurrentCount + 1, GeographicViews),

                mnesia:write(Video#video{geographic_views = UpdatedGeographicViews}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

track_device_view(VideoId, DeviceType) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                DeviceBreakdown = Video#video.device_breakdown,
                CurrentCount = maps:get(DeviceType, DeviceBreakdown, 0),
                UpdatedDeviceBreakdown = maps:put(DeviceType, CurrentCount + 1, DeviceBreakdown),

                mnesia:write(Video#video{device_breakdown = UpdatedDeviceBreakdown}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

track_referral(VideoId, Source) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                ReferralSources = Video#video.referral_sources,
                CurrentCount = maps:get(Source, ReferralSources, 0),
                UpdatedReferralSources = maps:put(Source, CurrentCount + 1, ReferralSources),

                mnesia:write(Video#video{referral_sources = UpdatedReferralSources}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

start_live_stream(VideoId, StreamUrl) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                Now = calendar:universal_time(),

                LiveStreamId = nanoid:gen(),
                LiveStream = #live_stream{
                    id = LiveStreamId,
                    video_id = VideoId,
                    streamer_id = Video#video.user_id,
                    playback_url = StreamUrl,
                    status = live,
                    actual_start = Now,
                    current_viewers = 0,
                    peak_viewers = 0
                },
                mnesia:write(LiveStream),

                mnesia:write(Video#video{
                    is_live = true,
                    live_stream_url = StreamUrl,
                    live_started_at = Now,
                    live_chat_id = LiveStreamId
                }),

                {ok, LiveStreamId}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

end_live_stream(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                Now = calendar:universal_time(),

                LiveChatId = Video#video.live_chat_id,
                case mnesia:read({live_stream, LiveChatId}) of
                    [LiveStream] ->
                        mnesia:write(LiveStream#live_stream{
                            status = ended,
                            ended_at = Now
                        });
                    [] -> ok
                end,

                mnesia:write(Video#video{
                    is_live = false,
                    live_ended_at = Now
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_live_viewers(VideoId, ViewerCount) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                PeakViewers = max(Video#video.peak_concurrent_viewers, ViewerCount),

                mnesia:write(Video#video{
                    live_viewers_current = ViewerCount,
                    peak_concurrent_viewers = PeakViewers
                }),

                LiveChatId = Video#video.live_chat_id,
                case mnesia:read({live_stream, LiveChatId}) of
                    [LiveStream] ->
                        mnesia:write(LiveStream#live_stream{
                            current_viewers = ViewerCount,
                            peak_viewers = max(LiveStream#live_stream.peak_viewers, ViewerCount)
                        });
                    [] -> ok
                end,

                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_live_stream_info(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                case Video#video.is_live of
                    true ->
                        {ok, #{
                            stream_url => Video#video.live_stream_url,
                            started_at => Video#video.live_started_at,
                            current_viewers => Video#video.live_viewers_current,
                            peak_viewers => Video#video.peak_concurrent_viewers,
                            chat_id => Video#video.live_chat_id
                        }};
                    false ->
                        {error, not_live}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

create_video_clip(SourceVideoId, CreatorId, Title, StartTimestamp, EndTimestamp) ->
    Fun = fun() ->
        case mnesia:read({video, SourceVideoId}) of
            [] -> {error, video_not_found};
            [_Video] ->
                ClipId = nanoid:gen(),
                Now = calendar:universal_time(),
                Duration = EndTimestamp - StartTimestamp,

                Clip = #video_clip{
                    id = ClipId,
                    source_video_id = SourceVideoId,
                    creator_id = CreatorId,
                    title = Title,
                    start_timestamp = StartTimestamp,
                    end_timestamp = EndTimestamp,
                    duration = Duration,
                    date_created = Now
                },
                mnesia:write(Clip),

                {ok, ClipId}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

enable_duet(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{duet_enabled = true}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

disable_duet(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{duet_enabled = false}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_duet_video(VideoId, DuetVideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                DuetVideos = Video#video.duet_videos,
                UpdatedDuetVideos = [DuetVideoId | DuetVideos],

                mnesia:write(Video#video{duet_videos = UpdatedDuetVideos}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

enable_stitch(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{stitch_enabled = true}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

disable_stitch(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{stitch_enabled = false}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_stitch_video(VideoId, StitchVideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                StitchVideos = Video#video.stitch_videos,
                UpdatedStitchVideos = [StitchVideoId | StitchVideos],

                mnesia:write(Video#video{stitch_videos = UpdatedStitchVideos}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_effect(VideoId, Effect) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                EffectsUsed = Video#video.effects_used,
                UpdatedEffects = [Effect | EffectsUsed],

                mnesia:write(Video#video{effects_used = UpdatedEffects}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_filter(VideoId, Filter) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                FiltersApplied = Video#video.filters_applied,
                UpdatedFilters = [Filter | FiltersApplied],

                mnesia:write(Video#video{filters_applied = UpdatedFilters}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

set_sound(VideoId, SoundId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{sound_id = SoundId}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

create_challenge(CreatorId, ChallengeName, Hashtag) ->
    Fun = fun() ->
        ChallengeId = nanoid:gen(),
        Now = calendar:universal_time(),

        Challenge = #viral_challenge{
            id = ChallengeId,
            challenge_name = ChallengeName,
            creator_id = CreatorId,
            hashtag = Hashtag,
            start_date = Now,
            participant_count = 0
        },
        mnesia:write(Challenge),

        {ok, ChallengeId}
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

submit_to_challenge(VideoId, ChallengeId) ->
    Fun = fun() ->
        case {mnesia:read({video, VideoId}), mnesia:read({viral_challenge, ChallengeId})} of
            {[], _} -> {error, video_not_found};
            {_, []} -> {error, challenge_not_found};
            {[Video], [Challenge]} ->
                mnesia:write(Video#video{challenge_id = ChallengeId}),

                VideoSubmissions = Challenge#viral_challenge.video_submissions,
                UpdatedSubmissions = [VideoId | VideoSubmissions],

                mnesia:write(Challenge#viral_challenge{
                    video_submissions = UpdatedSubmissions,
                    participant_count = Challenge#viral_challenge.participant_count + 1
                }),

                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

mint_video_nft(VideoId, CreatorId, Blockchain, TokenStandard, EditionNumber, TotalEditions) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                NFTId = nanoid:gen(),
                Now = calendar:universal_time(),

                NFT = #media_nft{
                    id = NFTId,
                    media_id = VideoId,
                    media_type = video,
                    creator_id = CreatorId,
                    blockchain = Blockchain,
                    token_standard = TokenStandard,
                    edition_number = EditionNumber,
                    total_editions = TotalEditions,
                    date_minted = Now
                },
                mnesia:write(NFT),

                mnesia:write(Video#video{
                    nft_minted = true,
                    nft_token_id = NFTId
                }),

                {ok, NFTId}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_video_nft(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                case Video#video.nft_minted of
                    true ->
                        NFTId = Video#video.nft_token_id,
                        case mnesia:read({media_nft, NFTId}) of
                            [NFT] -> {ok, NFT};
                            [] -> {error, nft_not_found}
                        end;
                    false ->
                        {error, nft_not_minted}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_collaboration(VideoId, CollaboratorId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                CollaborationTree = Video#video.collaboration_tree,
                UpdatedTree = [CollaboratorId | CollaborationTree],

                mnesia:write(Video#video{collaboration_tree = UpdatedTree}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

set_remix_permissions(VideoId, Permissions) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{remix_permissions = Permissions}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

set_revenue_split(VideoId, RevenueSplit) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{collaboration_revenue_split = RevenueSplit}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

pin_video(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                IPFSCID = Video#video.ipfs_cid,
                case is_cid_ready(IPFSCID) of
                    false -> {error, content_not_ready};
                    true ->
                        spawn(fun() ->
                            try
                                ipfs_client_5:pin_add([{arg, IPFSCID}])
                            catch
                                _:Error ->
                                    error_logger:error_msg("Failed to pin video ~p: ~p", [VideoId, Error])
                            end
                        end),
                        ok
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

unpin_video(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                IPFSCID = Video#video.ipfs_cid,
                case is_cid_ready(IPFSCID) of
                    false -> {error, content_not_ready};
                    true ->
                        spawn(fun() ->
                            try
                                ipfs_client_5:pin_rm([{arg, IPFSCID}])
                            catch
                                _:Error ->
                                    error_logger:error_msg("Failed to unpin video ~p: ~p", [VideoId, Error])
                            end
                        end),
                        ok
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_pin_status(VideoId, PinInfo) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{pin_info = PinInfo}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

report_video(ReporterId, VideoId, Type, Description) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                ReportId = nanoid:gen(),
                Now = calendar:universal_time(),

                Report = #report{
                    id = ReportId,
                    type = Type,
                    description = Description,
                    reporter = ReporterId,
                    date_created = Now,
                    data = #{video_id => VideoId}
                },
                mnesia:write(Report),

                UpdatedReports = [ReportId | Video#video.reported],
                mnesia:write(Video#video{reported = UpdatedReports}),

                {ok, ReportId}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

set_age_restriction(VideoId, AgeRestriction) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{age_restriction = AgeRestriction}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

set_content_warnings(VideoId, Warnings) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{content_warnings = Warnings}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

    search_videos(Query) ->
        Fun = fun() ->
            AllVideos = mnesia:match_object(#video{_ = '_'}),
            QueryLower = string:to_lower(ensure_string(Query)),
            lists:filter(fun(Video) ->
                Title = ensure_string(Video#video.title),
                TitleMatch = string:find(string:to_lower(Title), QueryLower) =/= nomatch,
                DescMatch = case Video#video.description of
                    undefined -> false;
                    Desc ->
                        DescStr = ensure_string(Desc),
                        string:find(string:to_lower(DescStr), QueryLower) =/= nomatch
                end,
                TagMatch = lists:any(fun(Tag) ->
                    TagStr = ensure_string(Tag),
                    string:find(string:to_lower(TagStr), QueryLower) =/= nomatch
                end, Video#video.hashtags),
                TitleMatch orelse DescMatch orelse TagMatch
            end, AllVideos)
        end,

        {atomic, Res} = mnesia:transaction(Fun),
        Res.

        search_videos_advanced(SearchParams) ->
            #{
                query := Query,
                tags := Tags,
                min_views := MinViews,
                privacy := Privacy,
                is_live := IsLive
            } = SearchParams,

            Fun = fun() ->
                AllVideos = mnesia:match_object(#video{_ = '_'}),
                QueryLower = string:to_lower(ensure_string(Query)),

                lists:filter(fun(Video) ->
                    TitleMatch = case Query of
                        "" -> true;
                        _ ->
                            Title = ensure_string(Video#video.title),
                            string:find(string:to_lower(Title), QueryLower) =/= nomatch
                    end,

                    TagMatch = case Tags of
                        [] -> true;
                        _ -> lists:any(fun(Tag) ->
                            TagStr = ensure_string(Tag),
                            lists:member(TagStr, [ensure_string(T) || T <- Video#video.hashtags])
                        end, Tags)
                    end,

                    ViewsMatch = Video#video.views >= MinViews,

                    PrivacyMatch = case Privacy of
                        any -> true;
                        _ -> Video#video.privacy =:= Privacy
                    end,

                    LiveMatch = case IsLive of
                        any -> true;
                        _ -> Video#video.is_live =:= IsLive
                    end,

                    TitleMatch andalso TagMatch andalso ViewsMatch andalso PrivacyMatch andalso LiveMatch
                end, AllVideos)
            end,

            {atomic, Res} = mnesia:transaction(Fun),
            Res.

get_trending_videos(Limit) ->
    Fun = fun() ->
        AllVideos = mnesia:match_object(#video{privacy = public, _ = '_'}),
        Sorted = lists:sort(fun(A, B) ->
            ScoreA = calculate_trending_score_value(A),
            ScoreB = calculate_trending_score_value(B),
            ScoreA > ScoreB
        end, AllVideos),
        lists:sublist(Sorted, Limit)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_featured_videos() ->
    Fun = fun() ->
        AllVideos = mnesia:match_object(#video{privacy = public, _ = '_'}),
        Sorted = lists:sort(fun(A, B) ->
            A#video.trending_score > B#video.trending_score
        end, AllVideos),
        lists:sublist(Sorted, 20)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

increment_download_count(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                CurrentData = Video#video.data,
                DownloadCount = maps:get(download_count, CurrentData, 0),
                UpdatedData = maps:put(download_count, DownloadCount + 1, CurrentData),

                mnesia:write(Video#video{data = UpdatedData}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

start_raid(SourceStreamId, TargetStreamId, ViewerCount) ->
    Fun = fun() ->
        RaidId = nanoid:gen(),
        Now = calendar:universal_time(),

        case mnesia:read({live_stream, SourceStreamId}) of
            [SourceStream] ->
                case mnesia:read({live_stream, TargetStreamId}) of
                    [_TargetStream] ->
                        Raid = #stream_raid{
                            id = RaidId,
                            source_stream_id = SourceStreamId,
                            target_stream_id = TargetStreamId,
                            raider_id = SourceStream#live_stream.streamer_id,
                            viewer_count = ViewerCount,
                            raid_timestamp = Now
                        },
                        mnesia:write(Raid),

                        {ok, RaidId};
                    [] ->
                        {error, target_stream_not_found}
                end;
            [] ->
                {error, source_stream_not_found}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

host_channel(VideoId, HostedChannelId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                HostedChannels = Video#video.hosted_channels,
                UpdatedHostedChannels = [HostedChannelId | HostedChannels],

                mnesia:write(Video#video{
                    host_enabled = true,
                    hosted_channels = UpdatedHostedChannels
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_moderator(VideoId, ModeratorId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                Moderators = Video#video.moderators,
                UpdatedModerators = case lists:member(ModeratorId, Moderators) of
                    true -> Moderators;
                    false -> [ModeratorId | Moderators]
                end,

                mnesia:write(Video#video{moderators = UpdatedModerators}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

remove_moderator(VideoId, ModeratorId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                Moderators = Video#video.moderators,
                UpdatedModerators = lists:delete(ModeratorId, Moderators),

                mnesia:write(Video#video{moderators = UpdatedModerators}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_vip(VideoId, VIPId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                VIPs = Video#video.vips,
                UpdatedVIPs = case lists:member(VIPId, VIPs) of
                    true -> VIPs;
                    false -> [VIPId | VIPs]
                end,

                mnesia:write(Video#video{vips = UpdatedVIPs}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

remove_vip(VideoId, VIPId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                VIPs = Video#video.vips,
                UpdatedVIPs = lists:delete(VIPId, VIPs),

                mnesia:write(Video#video{vips = UpdatedVIPs}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

ban_user(VideoId, UserId, Reason) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                CurrentData = Video#video.data,
                BannedUsers = maps:get(banned_users, CurrentData, []),
                UpdatedBannedUsers = [{UserId, Reason, calendar:universal_time()} | BannedUsers],
                UpdatedData = maps:put(banned_users, UpdatedBannedUsers, CurrentData),

                mnesia:write(Video#video{data = UpdatedData}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

enable_channel_points(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{channel_points_enabled = true}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

create_channel_reward(VideoId, RewardName, Description, Cost, RewardType) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                RewardId = nanoid:gen(),

                Reward = #channel_reward{
                    id = RewardId,
                    channel_id = VideoId,
                    reward_name = RewardName,
                    description = Description,
                    cost = Cost,
                    reward_type = RewardType
                },
                mnesia:write(Reward),

                PointRewards = Video#video.point_rewards,
                mnesia:write(Video#video{point_rewards = [RewardId | PointRewards]}),

                {ok, RewardId}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

redeem_reward(VideoId, UserId, RewardId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                case mnesia:read({channel_reward, RewardId}) of
                    [Reward] ->
                        CustomRewardsRedeemed = Video#video.custom_rewards_redeemed,
                        RedemptionEntry = {RewardId, UserId, calendar:universal_time()},

                        mnesia:write(Video#video{
                            custom_rewards_redeemed = [RedemptionEntry | CustomRewardsRedeemed]
                        }),

                        mnesia:write(Reward#channel_reward{
                            redemption_count = Reward#channel_reward.redemption_count + 1
                        }),

                        ok;
                    [] ->
                        {error, reward_not_found}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

enable_clip_creation(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{clips_enabled = true}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

disable_clip_creation(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                mnesia:write(Video#video{clips_enabled = false}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_simulcast_destination(VideoId, Destination) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                SimulcastDestinations = Video#video.simulcast_destinations,
                UpdatedDestinations = [Destination | SimulcastDestinations],

                mnesia:write(Video#video{simulcast_destinations = UpdatedDestinations}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

remove_simulcast_destination(VideoId, Destination) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                SimulcastDestinations = Video#video.simulcast_destinations,
                UpdatedDestinations = lists:delete(Destination, SimulcastDestinations),

                mnesia:write(Video#video{simulcast_destinations = UpdatedDestinations}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

calculate_trending_score(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                Score = calculate_trending_score_value(Video),

                mnesia:write(Video#video{trending_score = Score}),
                {ok, Score}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

calculate_trending_score_value(Video) ->
    Views = Video#video.views,
    Shares = Video#video.shares,

    ReactionCounts = Video#video.reaction_counts,
    TotalReactions = maps:fold(fun(_Type, Count, Acc) -> Count + Acc end, 0, ReactionCounts),

    Comments = length(Video#video.comments),

    Now = calendar:universal_time(),
    Created = Video#video.date_created,
    HoursOld = calendar:datetime_to_gregorian_seconds(Now) -
               calendar:datetime_to_gregorian_seconds(Created),
    HoursOldFloat = HoursOld / 3600,

    TimeDecay = math:pow(HoursOldFloat + 2, 1.5),

    (Views * 1.0 + TotalReactions * 5.0 + Comments * 3.0 + Shares * 10.0) / TimeDecay.

update_analytics(VideoId) ->
    Fun = fun() ->
        case mnesia:read({video, VideoId}) of
            [] -> {error, video_not_found};
            [Video] ->
                Now = calendar:universal_time(),

                Analytics = #media_analytics{
                    media_id = VideoId,
                    media_type = video,
                    date = Now,
                    views = Video#video.views,
                    unique_viewers = Video#video.unique_views,
                    watch_time = Video#video.watch_time_total,
                    avg_watch_percentage = Video#video.avg_watch_percentage,
                    likes = maps:fold(fun(_Type, Count, Acc) -> Count + Acc end, 0, Video#video.reaction_counts),
                    comments = length(Video#video.comments),
                    shares = Video#video.shares,
                    geographic_breakdown = Video#video.geographic_views,
                    device_breakdown = Video#video.device_breakdown
                },
                mnesia:write(Analytics),

                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

upload_video_to_ipfs(VideoId, VideoFile) ->
    try
        Size = byte_size(VideoFile),

        VideoCID = if
            Size > ?CHUNK_SIZE ->
                upload_large_file_chunked(VideoFile);
            true ->
                ipfs_media:upload_media(VideoFile)
        end,

        UpdateF = fun() ->
            case mnesia:read({video, VideoId}) of
                [Video] ->
                    UpdatedVideo = Video#video{
                        ipfs_cid = VideoCID,
                        file_url = VideoCID,
                        status = ready
                    },
                    mnesia:write(UpdatedVideo);
                [] -> ok
            end
        end,
        mnesia:transaction(UpdateF),

        content_cache:delete({video_file, VideoId}),

        ok
    catch
        Exception:Error:Stacktrace ->
            error_logger:error_msg(
                "Exception while uploading video ~p to IPFS: ~p:~p~n~p",
                [VideoId, Exception, Error, Stacktrace]
            ),

            UpdateStatusF = fun() ->
                case mnesia:read({video, VideoId}) of
                    [Video] ->
                        mnesia:write(Video#video{status = failed});
                    [] -> ok
                end
            end,
            mnesia:transaction(UpdateStatusF)
    end.

upload_video_update(VideoId, VideoFile) ->
    upload_video_to_ipfs(VideoId, VideoFile).

upload_video_version(VideoId, VideoFile) ->
    try
        VideoCID = ipfs_media:upload_media(VideoFile),

        UpdateF = fun() ->
            case mnesia:read({video, VideoId}) of
                [Video] ->
                    CurrentData = Video#video.data,
                    VersionHistory = maps:get(version_history, CurrentData, []),

                    [{VersionNum, _PendingCID, Timestamp, Description} | RestVersions] = VersionHistory,
                    UpdatedVersionEntry = {VersionNum, VideoCID, Timestamp, Description},
                    UpdatedVersionHistory = [UpdatedVersionEntry | RestVersions],

                    UpdatedData = maps:put(version_history, UpdatedVersionHistory, CurrentData),

                    UpdatedVideo = Video#video{
                        ipfs_cid = VideoCID,
                        file_url = VideoCID,
                        status = ready,
                        data = UpdatedData
                    },
                    mnesia:write(UpdatedVideo);
                [] -> ok
            end
        end,
        mnesia:transaction(UpdateF),

        content_cache:delete({video_version, VideoId}),

                ok
            catch
                Exception:Error:Stacktrace ->
                    error_logger:error_msg(
                        "Exception while uploading video version ~p: ~p:~p~n~p",
                        [VideoId, Exception, Error, Stacktrace]
                    )
            end.

        upload_large_file_chunked(BinaryContent) ->
            Chunks = split_into_chunks(BinaryContent, ?CHUNK_SIZE),
            ChunkCIDs = [ipfs_media:upload_media(Chunk) || Chunk <- Chunks],

            Manifest = #{
                <<"type">> => <<"chunked">>,
                <<"chunks">> => ChunkCIDs,
                <<"total_size">> => byte_size(BinaryContent)
            },

            ManifestJSON = jsx:encode(Manifest),
            ipfs_content:upload_text(binary_to_list(ManifestJSON)).

        split_into_chunks(Binary, ChunkSize) ->
            split_into_chunks(Binary, ChunkSize, []).

        split_into_chunks(<<>>, _ChunkSize, Acc) ->
            lists:reverse(Acc);
        split_into_chunks(Binary, ChunkSize, Acc) when byte_size(Binary) =< ChunkSize ->
            lists:reverse([Binary | Acc]);
        split_into_chunks(Binary, ChunkSize, Acc) ->
            <<Chunk:ChunkSize/binary, Rest/binary>> = Binary,
            split_into_chunks(Rest, ChunkSize, [Chunk | Acc]).

        validate_video_file(FilePath) ->
            case filelib:is_file(FilePath) of
                false ->
                    {error, file_not_found};
                true ->
                    case file:read_file_info(FilePath) of
                        {ok, FileInfo} ->
                            Size = FileInfo#file_info.size,
                            if
                                Size > ?MAX_FILE_SIZE ->
                                    {error, {file_too_large, Size, ?MAX_FILE_SIZE}};
                                Size == 0 ->
                                    {error, empty_file};
                                true ->
                                    Extension = string:to_lower(filename:extension(FilePath)),
                                    case lists:member(Extension, ?SUPPORTED_FORMATS) of
                                        true ->
                                            {ok, FileInfo};
                                        false ->
                                            {error, {unsupported_format, Extension}}
                                    end
                            end;
                        {error, Reason} ->
                            {error, {file_info_error, Reason}}
                    end
            end.

        calculate_content_size(Content) when is_list(Content) ->
            length(Content);
        calculate_content_size(Content) when is_binary(Content) ->
            byte_size(Content);
        calculate_content_size(_) ->
            0.

        is_cid_ready({pending, _}) -> false;
        is_cid_ready({pending_update, _}) -> false;
        is_cid_ready({pending_version, _}) -> false;
        is_cid_ready(undefined) -> false;
        is_cid_ready(_) -> true.

        receive_result({Pid, Ref}) ->
            receive
                {'DOWN', Ref, process, Pid, {result, Result}} ->
                    Result;
                {'DOWN', Ref, process, Pid, Reason} ->
                    error_logger:error_msg("Process ~p failed: ~p", [Pid, Reason]),
                    exit({concurrent_operation_failed, Reason})
            after 30000 ->
                exit(Pid, kill),
                exit(timeout)
            end.

        write_video_with_retry(Video, UserId, RetriesLeft) when RetriesLeft > 0 ->
            Fun = fun() ->
                mnesia:write(Video),

                case mnesia:read({user, UserId}) of
                    [User] ->
                        UserMedia = User#user.media,
                        UpdatedMedia = case UserMedia of
                            undefined -> [Video#video.id];
                            List when is_list(List) -> [Video#video.id | List];
                            _ -> [Video#video.id]
                        end,
                        mnesia:write(User#user{media = UpdatedMedia});
                    [] ->
                        error_logger:warning_msg("User ~p not found", [UserId])
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, _} ->
                    ok;
                {aborted, Reason} ->
                    error_logger:warning_msg("Video write failed (retries left: ~p): ~p",
                                           [RetriesLeft, Reason]),
                    timer:sleep(?BACKOFF_TIME * ((?MAX_RETRIES - RetriesLeft) + 1)),
                    write_video_with_retry(Video, UserId, RetriesLeft - 1)
            end;
        write_video_with_retry(_Video, _UserId, 0) ->
            {error, max_retries_exceeded}.

            create_video_with_rust(CreatorId, Title, Description, VideoFileOrPath, Duration, Privacy, Tags, AllowComments, AllowDownloads, Monetized) ->
                Fun = fun() ->
                    Id = nanoid:gen(),
                    Now = calendar:universal_time(),
                    AI_Video_ID = ai_videodb:insert_in_transaction(Id),

                    {SizeBytes, IsFilePath} = case VideoFileOrPath of
                        Path when is_list(Path) ->
                            case filelib:is_file(Path) of
                                true ->
                                    case file:read_file_info(Path) of
                                        {ok, FileInfo} -> {FileInfo#file_info.size, true};
                                        _ -> {0, false}
                                    end;
                                false ->
                                    {length(Path), false}
                            end;
                        Binary when is_binary(Binary) ->
                            {byte_size(Binary), false}
                    end,

                    if
                        IsFilePath ->
                            ok = content_cache:set({video_file_path, Id}, VideoFileOrPath);
                        true ->
                            ok = content_cache:set({video_file, Id}, VideoFileOrPath)
                    end,

                    Video = #video{
                        id = Id,
                        ai_video_id = AI_Video_ID,
                        user_id = CreatorId,
                        file_url = {pending, Id},
                        ipfs_cid = {pending, Id},
                        status = processing,
                        title = Title,
                        description = Description,
                        duration_seconds = Duration,
                        privacy = Privacy,
                        hashtags = Tags,
                        allow_comments = AllowComments,
                        allow_downloads = AllowDownloads,
                        monetized = Monetized,
                        file_size_bytes = SizeBytes,
                        date_created = Now,
                        date_updated = Now,
                        views = 0,
                        unique_views = 0,
                        likes = [],
                        thumbnail_url = undefined,
                        original_resolution = undefined,
                        codec = undefined,
                        bitrate = undefined,
                        frame_rate = undefined,
                        subtitles = [],
                        available_qualities = [],
                        reactions = #{
                            like => [],
                            love => [],
                            wow => [],
                            haha => [],
                            fire => []
                        },
                        reaction_counts = #{
                            like => 0,
                            love => 0,
                            wow => 0,
                            haha => 0,
                            fire => 0
                        },
                        comments = [],
                        shares = 0,
                        saves = 0,
                        data = #{is_file_path => IsFilePath}
                    },

                    mnesia:write(Video),

                    case mnesia:read({user, CreatorId}) of
                        [User] ->
                            CurrentMedia = case User#user.media of
                                undefined -> [Id];
                                List when is_list(List) -> [Id | List];
                                _ -> [Id]
                            end,
                            mnesia:write(User#user{media = CurrentMedia});
                        [] ->
                            error_logger:warning_msg("User ~p not found when creating video", [CreatorId])
                    end,

                    {ok, Id}
                end,

                case mnesia:transaction(Fun) of
                    {atomic, {ok, Id}} ->
                        spawn(fun() ->
                            upload_video_to_ipfs_simple(Id, VideoFileOrPath)
                        end),
                        Id;
                    {atomic, {error, Reason}} ->
                        {error, Reason};
                    {aborted, Reason} ->
                        {error, {transaction_failed, Reason}}
                end.

                upload_video_to_ipfs_simple(VideoId, VideoFileOrPath) ->
                    try
                        {InputType, Size, ActualPath} = case VideoFileOrPath of
                            Path when is_list(Path) ->
                                case filelib:is_file(Path) of
                                    true ->
                                        case file:read_file_info(Path) of
                                            {ok, FileInfo} ->
                                                {file_path, FileInfo#file_info.size, Path};
                                            {error, _} ->
                                                {binary, 0, undefined}
                                        end;
                                    false ->
                                        {binary, length(Path), undefined}
                                end;
                            Binary when is_binary(Binary) ->
                                {binary, byte_size(Binary), undefined}
                        end,

                        error_logger:info_msg("Starting IPFS upload for video ~p, type: ~p, size: ~p bytes",
                                              [VideoId, InputType, Size]),

                        VideoCID = case InputType of
                            file_path ->
                                error_logger:info_msg("Uploading video from file path: ~p", [ActualPath]),
                                case ipfs_video:upload_video(ActualPath) of
                                    {error, Reason} ->
                                        error_logger:error_msg("IPFS upload failed: ~p", [Reason]),
                                        error;
                                    CID when is_list(CID) ->
                                        error_logger:info_msg("Successfully uploaded video to IPFS: ~p", [CID]),
                                        CID;
                                    CID when is_binary(CID) ->
                                        error_logger:info_msg("Successfully uploaded video to IPFS: ~p", [CID]),
                                        binary_to_list(CID);
                                    Other ->
                                        error_logger:error_msg("Unexpected IPFS upload result: ~p", [Other]),
                                        error
                                end;
                            binary ->
                                if Size > ?CHUNK_SIZE ->
                                    error_logger:info_msg("Large video binary detected, using chunked upload"),
                                    upload_large_file_chunked(VideoFileOrPath);
                                true ->
                                    error_logger:info_msg("Uploading video binary to IPFS"),
                                    case ipfs_video:upload_video(VideoFileOrPath) of
                                        {error, Reason} ->
                                            error_logger:error_msg("IPFS upload failed: ~p", [Reason]),
                                            error;
                                        CID when is_list(CID) ->
                                            CID;
                                        CID when is_binary(CID) ->
                                            binary_to_list(CID);
                                        Other ->
                                            error_logger:error_msg("Unexpected IPFS upload result: ~p", [Other]),
                                            error
                                    end
                                end
                        end,

                        case VideoCID of
                            error ->
                                error_logger:error_msg("Failed to upload video content to IPFS for video ~p", [VideoId]),
                                UpdateF = fun() ->
                                    case mnesia:read({video, VideoId}) of
                                        [Video] ->
                                            mnesia:write(Video#video{
                                                ipfs_cid = {error, ipfs_upload_failed},
                                                file_url = {error, ipfs_upload_failed},
                                                status = failed
                                            });
                                        [] -> ok
                                    end
                                end,
                                mnesia:transaction(UpdateF);
                            ValidCID ->
                                error_logger:info_msg("Video content uploaded successfully: ~p", [ValidCID]),

                                UpdateF = fun() ->
                                    case mnesia:read({video, VideoId}) of
                                        [Video] ->
                                            UpdatedVideo = Video#video{
                                                ipfs_cid = ValidCID,
                                                file_url = ValidCID,
                                                status = ready
                                            },
                                            mnesia:write(UpdatedVideo);
                                        [] -> ok
                                    end
                                end,
                                mnesia:transaction(UpdateF),

                                content_cache:delete({video_file, VideoId}),
                                content_cache:delete({video_file_path, VideoId}),

                                case InputType of
                                    file_path when is_list(ActualPath) ->
                                        spawn(fun() ->
                                            timer:sleep(30000),
                                            try
                                                case filelib:is_file(ActualPath) of
                                                    true ->
                                                        file:delete(ActualPath),
                                                        error_logger:info_msg("Cleaned up temp video file: ~p", [ActualPath]);
                                                    false ->
                                                        ok
                                                end
                                            catch
                                                Exception:Error ->
                                                    error_logger:warning_msg("Failed to cleanup temp video ~p: ~p:~p",
                                                                           [ActualPath, Exception, Error])
                                            end
                                        end);
                                    _ ->
                                        ok
                                end,

                                spawn(fun() ->
                                    timer:sleep(10000),
                                    try
                                        KeyName = "video_" ++ VideoId,
                                        error_logger:info_msg("Generating IPNS key: ~p", [KeyName]),
                                        {ok, #{id := _KeyID, name := _BinID}} = ipfs_client_4:key_gen(KeyName),

                                        PublishOptions = [
                                            {key, KeyName},
                                            {resolve, false},
                                            {lifetime, "168h0m0s"},
                                            {ttl, "15m0s"},
                                            {v1compat, true},
                                            {ipns_base, "base36"},
                                            {quieter, true},
                                            {'allow-offline', true}
                                        ],

                                        error_logger:info_msg("Publishing to IPNS: /ipfs/~p", [ValidCID]),
                                        case ipfs_client_5:name_publish("/ipfs/" ++ ValidCID, PublishOptions) of
                                            {ok, #{name := IPNSKey}} ->
                                                error_logger:info_msg("IPNS publish successful: ~p", [IPNSKey]),
                                                update_video_ipns(VideoId, IPNSKey);
                                            {error, IPNSReason} ->
                                                error_logger:error_msg("IPNS publish failed for video ~p: ~p", [VideoId, IPNSReason])
                                        end
                                    catch
                                        Exception:Error:Stacktrace ->
                                            error_logger:error_msg(
                                                "Exception while publishing to IPNS for video ~p: ~p:~p~n~p",
                                                [VideoId, Exception, Error, Stacktrace]
                                            )
                                    end
                                end),

                                ok
                        end
                    catch
                        Exception:Error:Stacktrace ->
                            error_logger:error_msg(
                                "Exception while uploading video ~p to IPFS: ~p:~p~n~p",
                                [VideoId, Exception, Error, Stacktrace]
                            ),
                            UpdateStatusF = fun() ->
                                case mnesia:read({video, VideoId}) of
                                    [Video] ->
                                        mnesia:write(Video#video{
                                            status = failed,
                                            ipfs_cid = {error, ipfs_upload_failed}
                                        });
                                    [] -> ok
                                end
                            end,
                            mnesia:transaction(UpdateStatusF)
                    end.

                    upload_video_to_ipfs_with_rust(VideoId, VideoFile) when is_binary(VideoFile) ->
                        try
                            Size = byte_size(VideoFile),

                            VideoCID = if
                                Size > ?CHUNK_SIZE ->
                                    upload_large_file_chunked(VideoFile);
                                true ->
                                    ipfs_media:upload_media(VideoFile)
                            end,
                            TempDir = "/tmp",
                            filelib:ensure_dir(TempDir ++ "/"),
                            TempFilePath = TempDir ++ "/mazaryn_video_" ++ VideoId ++ ".webm",

                            case file:write_file(TempFilePath, VideoFile) of
                                ok ->
                                    VideoInfo = case media_video_client:get_video_info(TempFilePath) of
                                        {ok, Info} -> Info;
                                        {error, InfoReason} ->
                                            error_logger:warning_msg("Failed to get video info: ~p", [InfoReason]),
                                            #{}
                                    end,

                                    file:delete(TempFilePath),

                                    Duration = maps:get(duration, VideoInfo, 0.0),
                                    Resolution = case maps:get(resolution, VideoInfo, undefined) of
                                        #{width := W, height := H} ->
                                            integer_to_list(W) ++ "x" ++ integer_to_list(H);
                                        _ -> "unknown"
                                    end,
                                    Codec = maps:get(codec, VideoInfo, "unknown"),
                                    Bitrate = maps:get(bitrate, VideoInfo, 0),
                                    FrameRate = maps:get(frame_rate, VideoInfo, 0.0),

                                    UpdateF = fun() ->
                                        case mnesia:read({video, VideoId}) of
                                            [Video] ->
                                                UpdatedVideo = Video#video{
                                                    ipfs_cid = VideoCID,
                                                    file_url = VideoCID,
                                                    status = ready,
                                                    duration_seconds = Duration,
                                                    original_resolution = Resolution,
                                                    codec = Codec,
                                                    bitrate = Bitrate,
                                                    frame_rate = FrameRate
                                                },
                                                mnesia:write(UpdatedVideo);
                                            [] -> ok
                                        end
                                    end,
                                    mnesia:transaction(UpdateF),

                                    content_cache:delete({video_file, VideoId}),

                                    spawn(fun() ->
                                        timer:sleep(5000),
                                        case mnesia:read({video, VideoId}) of
                                            [Video] ->
                                                case Video#video.ipfs_cid of
                                                    CID when is_list(CID) ->
                                                        Qualities = ["1080p", "720p", "480p", "360p"],
                                                        case media_video_client:create_transcoding_job(
                                                            VideoId,
                                                            CID,
                                                            Qualities
                                                        ) of
                                                            {ok, JobId} ->
                                                                error_logger:info_msg("Transcoding job created: ~p for video ~p", [JobId, VideoId]),
                                                                UpdateTranscodingF = fun() ->
                                                                    case mnesia:read({video, VideoId}) of
                                                                        [V] ->
                                                                            CurrentData = V#video.data,
                                                                            UpdatedData = maps:put(transcoding_job_id, JobId, CurrentData),
                                                                            mnesia:write(V#video{data = UpdatedData});
                                                                        [] -> ok
                                                                    end
                                                                end,
                                                                mnesia:transaction(UpdateTranscodingF);
                                                            {error, TranscodingReason} ->
                                                                error_logger:error_msg("Failed to create transcoding job for video ~p: ~p", [VideoId, TranscodingReason])
                                                        end;
                                                    _ ->
                                                        error_logger:warning_msg("Video CID not ready for transcoding: ~p", [VideoId])
                                                end;
                                            [] ->
                                                error_logger:warning_msg("Video not found for transcoding: ~p", [VideoId])
                                        end
                                    end),

                                    spawn(fun() ->
                                        timer:sleep(20000),
                                        try
                                            KeyName = "video_" ++ VideoId,
                                            {ok, #{id := _KeyID, name := _BinID}} = ipfs_client_4:key_gen(KeyName),

                                            PublishOptions = [
                                                {key, KeyName},
                                                {resolve, false},
                                                {lifetime, "168h0m0s"},
                                                {ttl, "15m0s"},
                                                {v1compat, true},
                                                {ipns_base, "base36"},
                                                {quieter, true},
                                                {'allow-offline', true}
                                            ],

                                            case ipfs_client_5:name_publish("/ipfs/" ++ VideoCID, PublishOptions) of
                                                {ok, #{name := IPNSKey}} ->
                                                    update_video_ipns(VideoId, IPNSKey);
                                                {error, IPNSReason} ->
                                                    error_logger:error_msg("IPNS publish failed for video ~p: ~p", [VideoId, IPNSReason])
                                            end
                                        catch
                                            IPNSException:IPNSError:IPNSStacktrace ->
                                                error_logger:error_msg(
                                                    "Exception while publishing to IPNS for video ~p: ~p:~p~n~p",
                                                    [VideoId, IPNSException, IPNSError, IPNSStacktrace]
                                                )
                                        end
                                    end),

                                    ok;
                                {error, WriteReason} ->
                                    error_logger:error_msg("Failed to write temp file for video ~p: ~p", [VideoId, WriteReason]),

                                    UpdateF = fun() ->
                                        case mnesia:read({video, VideoId}) of
                                            [Video] ->
                                                UpdatedVideo = Video#video{
                                                    ipfs_cid = VideoCID,
                                                    file_url = VideoCID,
                                                    status = ready
                                                },
                                                mnesia:write(UpdatedVideo);
                                            [] -> ok
                                        end
                                    end,
                                    mnesia:transaction(UpdateF),
                                    content_cache:delete({video_file, VideoId})
                            end
                        catch
                            Exception:Error:Stacktrace ->
                                error_logger:error_msg(
                                    "Exception while uploading video ~p to IPFS: ~p:~p~n~p",
                                    [VideoId, Exception, Error, Stacktrace]
                                ),

                                UpdateStatusF = fun() ->
                                    case mnesia:read({video, VideoId}) of
                                        [Video] ->
                                            mnesia:write(Video#video{status = failed});
                                        [] -> ok
                                    end
                                end,
                                mnesia:transaction(UpdateStatusF)
                        end.

            update_video_ipns(VideoId, IPNSKey) ->
                UpdateF = fun() ->
                    case mnesia:read({video, VideoId}) of
                        [Video] ->
                            CurrentData = Video#video.data,
                            UpdatedData = maps:put(ipns, IPNSKey, CurrentData),
                            mnesia:write(Video#video{data = UpdatedData}),
                            ok;
                        [] ->
                            {error, not_found}
                    end
                end,

                case mnesia:transaction(UpdateF) of
                    {atomic, ok} ->
                        ok;
                    {atomic, {error, Reason}} ->
                        error_logger:error_msg("Failed to update video ~p with IPNS: ~p", [VideoId, Reason]),
                        {error, Reason};
                    {aborted, Reason} ->
                        error_logger:error_msg("Transaction aborted while updating video ~p with IPNS: ~p", [VideoId, Reason]),
                        {error, {transaction_failed, Reason}}
                end.

                start_live_stream_with_rust(VideoId, StreamerId, Title, LatencyMode) ->
                    case get_video(VideoId) of
                        {ok, Video} ->
                            StreamKey = generate_stream_key(),

                            LatencyModeStr = case LatencyMode of
                                normal -> <<"normal">>;
                                low_latency -> <<"low_latency">>;
                                ultra_low -> <<"ultra_low">>;
                                _ -> atom_to_binary(LatencyMode, utf8)
                            end,

                            ReqBody = jsx:encode(#{
                                video_id => list_to_binary(VideoId),
                                streamer_id => list_to_binary(StreamerId),
                                title => Title,
                                latency_mode => LatencyModeStr
                            }),

                            case httpc:request(post,
                                {"http://127.0.0.1:2020/media/streams",
                                 [{"content-type", "application/json"}],
                                 "application/json",
                                 binary_to_list(ReqBody)
                                }, [], []) of
                                {ok, {{_, 200, _}, _, ResponseBody}} ->
                                    Response = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
                                    StreamId = maps:get(<<"stream_id">>, Response),
                                    IngestUrl = maps:get(<<"ingest_url">>, Response),
                                    PlaybackUrl = maps:get(<<"playback_url">>, Response),
                                    RustStreamKey = maps:get(<<"stream_key">>, Response, list_to_binary(StreamKey)),

                                    LiveStream = #live_stream{
                                        id = binary_to_list(StreamId),
                                        video_id = VideoId,
                                        streamer_id = StreamerId,
                                        stream_key = StreamKey,
                                        ingest_url = binary_to_list(IngestUrl),
                                        playback_url = binary_to_list(PlaybackUrl),
                                        protocol = <<"RTMP">>,
                                        latency_mode = LatencyMode,
                                        status = created,
                                        current_viewers = 0,
                                        peak_viewers = 0,
                                        total_unique_viewers = 0,
                                        viewer_timeline = [],
                                        donations_enabled = true,
                                        super_chat_messages = [],
                                        simulcast_platforms = [],
                                        auto_record = true,
                                        highlights = [],
                                        subscription_only = false,
                                        donations_total = 0.0,
                                        dropped_frames = 0,
                                        data = #{}
                                    },

                                    mnesia:transaction(fun() ->
                                        mnesia:write(LiveStream)
                                    end),

                                    UpdatedVideo = Video#video{
                                        is_live = true,
                                        live_stream_url = binary_to_list(PlaybackUrl)
                                    },
                                    mnesia:transaction(fun() ->
                                        mnesia:write(UpdatedVideo)
                                    end),

                                    {ok, StreamId, StreamKey, IngestUrl, PlaybackUrl};
                                {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
                                    {error, {http_error, StatusCode, ErrorBody}};
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end.

            track_video_playback(VideoId, UserId, Quality) ->
                case media_video_client:create_session(VideoId, UserId, Quality) of
                    {ok, SessionId, _, _} ->
                        increment_view_count(VideoId, UserId),
                        {ok, SessionId};
                    {error, Reason} ->
                        {error, Reason}
                end.

                get_video(VideoId) ->
                    case mnesia:transaction(fun() ->
                        mnesia:read(video, VideoId)
                    end) of
                        {atomic, [Video]} -> {ok, Video};
                        {atomic, []} -> {error, not_found};
                        {aborted, Reason} -> {error, Reason}
                    end.

                generate_stream_key() ->
                    binary_to_list(base64:encode(crypto:strong_rand_bytes(32))).

            update_playback_position(SessionId, Position) ->
                media_video_client:update_position(SessionId, Position).

            get_video_analytics(VideoId) ->
                case media_video_client:get_video_metrics(VideoId) of
                    {ok, Metrics} ->
                        case get_view_stats(VideoId) of
                            {ok, MnesiaStats} ->
                                {ok, maps:merge(Metrics, MnesiaStats)};
                            _ ->
                                {ok, Metrics}
                        end;
                    Error ->
                        Error
                end.

            export_video_analytics(VideoId) ->
                media_video_client:get_analytics_report(VideoId).

                detect_video_format(FilePath) when is_binary(FilePath) ->
                    detect_video_format(binary_to_list(FilePath));
                detect_video_format(FilePath) when is_list(FilePath) ->
                    Extension = string:to_lower(filename:extension(FilePath)),
                    case Extension of
                        ".mp4" -> mp4;
                        ".webm" -> webm;
                        ".mkv" -> mkv;
                        ".mov" -> mov;
                        ".avi" -> avi;
                        ".flv" -> flv;
                        ".wmv" -> wmv;
                        ".m4v" -> m4v;
                        ".mpg" -> mpg;
                        ".mpeg" -> mpeg;
                        _ -> unknown
                    end.

            get_video_info_from_rust(FilePath) ->
                media_video_client:get_video_info(FilePath).

            get_video_cid(VideoId) ->
                Fun = fun() ->
                    case mnesia:read({video, VideoId}) of
                        [] -> {error, video_not_found};
                        [Video] -> {ok, Video#video.ipfs_cid}
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, {ok, CID}} -> CID;
                    {atomic, {error, Reason}} -> {error, Reason};
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            get_transcoding_status(VideoId) ->
                Fun = fun() ->
                    case mnesia:read({video, VideoId}) of
                        [] -> {error, video_not_found};
                        [Video] ->
                            case maps:get(transcoding_job_id, Video#video.data, undefined) of
                                undefined -> {error, no_transcoding_job};
                                JobId -> {ok, JobId}
                            end
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, {ok, JobId}} ->
                        media_video_client:get_transcoding_job(JobId);
                    {atomic, {error, Reason}} ->
                        {error, Reason};
                    {aborted, Reason} ->
                        {error, {transaction_failed, Reason}}
                end.

            get_available_qualities(VideoId) ->
                case get_transcoding_status(VideoId) of
                    {ok, JobInfo} ->
                        case maps:get(status, JobInfo, undefined) of
                            <<"Completed">> ->
                                Qualities = maps:get(qualities, JobInfo, []),
                                {ok, Qualities};
                            _ ->
                                {error, transcoding_in_progress}
                        end;
                    Error ->
                        Error
                end.

                ensure_string(Value) when is_binary(Value) -> binary_to_list(Value);
                ensure_string(Value) when is_list(Value) -> Value;
                ensure_string(Value) when is_atom(Value) -> atom_to_list(Value);
                ensure_string(_) -> "".
