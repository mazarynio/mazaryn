-module(video_server).
-author("Zaryn Technologies").
-include_lib("kernel/include/logger.hrl").
-export([
    start_link/0,
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
    react_to_video/3,
    remove_reaction_from_video/2,
    get_reactions_by_type/2,
    get_all_reactions/1,
    get_reaction_counts/1,
    has_user_reacted_with_type/3,
    get_user_reaction_type/2,
    add_video_comment/3,
    get_video_comments/1,
    get_comment_count/1,
    increment_view_count/2,
    increment_unique_view/2,
    track_watch_time/3,
    get_view_stats/1,
    share_video/2,
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
    update_analytics/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {worker_pool_size = 100, worker_pool = []}).
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).
create_video(CreatorId, Title, Description, VideoFile, Duration, Privacy, Tags, AllowComments, AllowDownloads, Monetized) ->
    gen_server:call({global, ?MODULE}, {create_video, CreatorId, Title, Description, VideoFile, Duration, Privacy, Tags, AllowComments, AllowDownloads, Monetized}).
create_video_concurrent(CreatorId, Title, Description, VideoFile, Duration, Privacy, Tags, AllowComments, AllowDownloads, Monetized) ->
    RequestId = make_ref(),
    gen_server:cast({global, ?MODULE}, {create_video_concurrent, CreatorId, Title, Description, VideoFile, Duration, Privacy, Tags, AllowComments, AllowDownloads, Monetized, {self(), RequestId}}),
    receive
        {video_creation_result, RequestId, Result} -> Result
    after 45000 ->
        {error, timeout}
    end.
create_video_from_file(CreatorId, Title, Description, FilePath, Duration, Privacy, Tags, AllowComments, Monetized) ->
    gen_server:call({global, ?MODULE}, {create_video_from_file, CreatorId, Title, Description, FilePath, Duration, Privacy, Tags, AllowComments, Monetized}, 60000).

upload_video_file(VideoId, FilePath) ->
    gen_server:call({global, ?MODULE}, {upload_video_file, VideoId, FilePath}, 60000).

update_video(VideoId, CreatorId, NewTitle, NewDescription, NewDuration, NewPrivacy, NewTags, NewAllowComments, NewAllowDownloads, NewAllowRemixes, NewMonetized) ->
    gen_server:call({global, ?MODULE}, {update_video, VideoId, CreatorId, NewTitle, NewDescription, NewDuration, NewPrivacy, NewTags, NewAllowComments, NewAllowDownloads, NewAllowRemixes, NewMonetized}).

delete_video(VideoId, UserId) ->
    gen_server:call({global, ?MODULE}, {delete_video, VideoId, UserId}).

get_video_by_id(VideoId) ->
    gen_server:call({global, ?MODULE}, {get_video_by_id, VideoId}).

get_videos_by_creator(CreatorId) ->
    gen_server:call({global, ?MODULE}, {get_videos_by_creator, CreatorId}).

get_videos_by_business(BusinessId) ->
    gen_server:call({global, ?MODULE}, {get_videos_by_business, BusinessId}).

get_public_videos() ->
    gen_server:call({global, ?MODULE}, {get_public_videos}).

get_videos_by_tag(Tag) ->
    gen_server:call({global, ?MODULE}, {get_videos_by_tag, Tag}).

get_video_content(VideoId) ->
    gen_server:call({global, ?MODULE}, {get_video_content, VideoId}).

get_video_metadata(VideoId) ->
    gen_server:call({global, ?MODULE}, {get_video_metadata, VideoId}).

get_video_qualities(VideoId) ->
    gen_server:call({global, ?MODULE}, {get_video_qualities, VideoId}).

create_video_version(VideoId, UserId, NewVideoFile, ChangeDescription) ->
    gen_server:call({global, ?MODULE}, {create_video_version, VideoId, UserId, NewVideoFile, ChangeDescription}, 60000).

get_video_versions(VideoId) ->
    gen_server:call({global, ?MODULE}, {get_video_versions, VideoId}).

add_thumbnail(VideoId, ThumbnailData, Timestamp) ->
    gen_server:call({global, ?MODULE}, {add_thumbnail, VideoId, ThumbnailData, Timestamp}, 30000).

add_preview(VideoId, PreviewData) ->
    gen_server:call({global, ?MODULE}, {add_preview, VideoId, PreviewData}, 30000).

add_chapter(VideoId, Title, StartTime, EndTime) ->
    gen_server:call({global, ?MODULE}, {add_chapter, VideoId, Title, StartTime, EndTime}).

add_subtitle(VideoId, Language, SubtitleData, Format) ->
    gen_server:call({global, ?MODULE}, {add_subtitle, VideoId, Language, SubtitleData, Format}, 30000).

add_audio_track(VideoId, Language, AudioData, TrackType) ->
    gen_server:call({global, ?MODULE}, {add_audio_track, VideoId, Language, AudioData, TrackType}, 30000).

react_to_video(UserID, VideoId, ReactionType) ->
    gen_server:call({global, ?MODULE}, {react_to_video, UserID, VideoId, ReactionType}).

remove_reaction_from_video(LikeID, VideoId) ->
    gen_server:call({global, ?MODULE}, {remove_reaction_from_video, LikeID, VideoId}).

get_reactions_by_type(VideoId, ReactionType) ->
    gen_server:call({global, ?MODULE}, {get_reactions_by_type, VideoId, ReactionType}).

get_all_reactions(VideoId) ->
    gen_server:call({global, ?MODULE}, {get_all_reactions, VideoId}).

get_reaction_counts(VideoId) ->
    gen_server:call({global, ?MODULE}, {get_reaction_counts, VideoId}).

has_user_reacted_with_type(UserID, VideoId, ReactionType) ->
    gen_server:call({global, ?MODULE}, {has_user_reacted_with_type, UserID, VideoId, ReactionType}).

get_user_reaction_type(UserID, VideoId) ->
    gen_server:call({global, ?MODULE}, {get_user_reaction_type, UserID, VideoId}).

add_video_comment(Author, VideoId, Content) ->
    gen_server:call({global, ?MODULE}, {add_video_comment, Author, VideoId, Content}).

get_video_comments(VideoId) ->
    gen_server:call({global, ?MODULE}, {get_video_comments, VideoId}).

get_comment_count(VideoId) ->
    gen_server:call({global, ?MODULE}, {get_comment_count, VideoId}).

increment_view_count(VideoId, UserId) ->
    gen_server:call({global, ?MODULE}, {increment_view_count, VideoId, UserId}).

increment_unique_view(VideoId, UserId) ->
    gen_server:call({global, ?MODULE}, {increment_unique_view, VideoId, UserId}).

track_watch_time(VideoId, UserId, WatchSeconds) ->
    gen_server:call({global, ?MODULE}, {track_watch_time, VideoId, UserId, WatchSeconds}).

get_view_stats(VideoId) ->
    gen_server:call({global, ?MODULE}, {get_view_stats, VideoId}).

share_video(VideoId, UserId) ->
    gen_server:call({global, ?MODULE}, {share_video, VideoId, UserId}).

save_video(VideoId, UserId) ->
    gen_server:call({global, ?MODULE}, {save_video, VideoId, UserId}).

unsave_video(VideoId, UserId) ->
    gen_server:call({global, ?MODULE}, {unsave_video, VideoId, UserId}).

add_to_playlist(VideoId, PlaylistId) ->
    gen_server:call({global, ?MODULE}, {add_to_playlist, VideoId, PlaylistId}).

remove_from_playlist(VideoId, PlaylistId) ->
    gen_server:call({global, ?MODULE}, {remove_from_playlist, VideoId, PlaylistId}).

link_to_series(VideoId, SeriesId, EpisodeNumber) ->
    gen_server:call({global, ?MODULE}, {link_to_series, VideoId, SeriesId, EpisodeNumber}).

add_interactive_hotspot(VideoId, Timestamp, Duration, Position, Content) ->
    gen_server:call({global, ?MODULE}, {add_interactive_hotspot, VideoId, Timestamp, Duration, Position, Content}).

create_poll(VideoId, Timestamp, Question, Options) ->
    gen_server:call({global, ?MODULE}, {create_poll, VideoId, Timestamp, Question, Options}).

vote_on_poll(PollId, UserId, OptionIndex) ->
    gen_server:call({global, ?MODULE}, {vote_on_poll, PollId, UserId, OptionIndex}).

set_monetization(VideoId, RevenueModel, Price) ->
    gen_server:call({global, ?MODULE}, {set_monetization, VideoId, RevenueModel, Price}).

add_ad_break(VideoId, Timestamp, AdType) ->
    gen_server:call({global, ?MODULE}, {add_ad_break, VideoId, Timestamp, AdType}).

set_sponsor_info(VideoId, SponsorInfo) ->
    gen_server:call({global, ?MODULE}, {set_sponsor_info, VideoId, SponsorInfo}).

track_viewer_retention(VideoId, Timestamp, PercentageViewing) ->
    gen_server:call({global, ?MODULE}, {track_viewer_retention, VideoId, Timestamp, PercentageViewing}).

track_geographic_view(VideoId, Country, City) ->
    gen_server:call({global, ?MODULE}, {track_geographic_view, VideoId, Country, City}).

track_device_view(VideoId, DeviceType) ->
    gen_server:call({global, ?MODULE}, {track_device_view, VideoId, DeviceType}).

track_referral(VideoId, Source) ->
    gen_server:call({global, ?MODULE}, {track_referral, VideoId, Source}).

start_live_stream(VideoId, StreamUrl) ->
    gen_server:call({global, ?MODULE}, {start_live_stream, VideoId, StreamUrl}).

end_live_stream(VideoId) ->
    gen_server:call({global, ?MODULE}, {end_live_stream, VideoId}).

update_live_viewers(VideoId, ViewerCount) ->
    gen_server:call({global, ?MODULE}, {update_live_viewers, VideoId, ViewerCount}).

get_live_stream_info(VideoId) ->
    gen_server:call({global, ?MODULE}, {get_live_stream_info, VideoId}).

create_video_clip(SourceVideoId, CreatorId, Title, StartTimestamp, EndTimestamp) ->
    gen_server:call({global, ?MODULE}, {create_video_clip, SourceVideoId, CreatorId, Title, StartTimestamp, EndTimestamp}).

enable_duet(VideoId) ->
    gen_server:call({global, ?MODULE}, {enable_duet, VideoId}).

disable_duet(VideoId) ->
    gen_server:call({global, ?MODULE}, {disable_duet, VideoId}).

add_duet_video(VideoId, DuetVideoId) ->
    gen_server:call({global, ?MODULE}, {add_duet_video, VideoId, DuetVideoId}).

enable_stitch(VideoId) ->
    gen_server:call({global, ?MODULE}, {enable_stitch, VideoId}).

disable_stitch(VideoId) ->
    gen_server:call({global, ?MODULE}, {disable_stitch, VideoId}).

add_stitch_video(VideoId, StitchVideoId) ->
    gen_server:call({global, ?MODULE}, {add_stitch_video, VideoId, StitchVideoId}).

add_effect(VideoId, Effect) ->
    gen_server:call({global, ?MODULE}, {add_effect, VideoId, Effect}).

add_filter(VideoId, Filter) ->
    gen_server:call({global, ?MODULE}, {add_filter, VideoId, Filter}).

set_sound(VideoId, SoundId) ->
    gen_server:call({global, ?MODULE}, {set_sound, VideoId, SoundId}).

create_challenge(CreatorId, ChallengeName, Hashtag) ->
    gen_server:call({global, ?MODULE}, {create_challenge, CreatorId, ChallengeName, Hashtag}).

submit_to_challenge(VideoId, ChallengeId) ->
    gen_server:call({global, ?MODULE}, {submit_to_challenge, VideoId, ChallengeId}).

mint_video_nft(VideoId, CreatorId, Blockchain, TokenStandard, EditionNumber, TotalEditions) ->
    gen_server:call({global, ?MODULE}, {mint_video_nft, VideoId, CreatorId, Blockchain, TokenStandard, EditionNumber, TotalEditions}).

get_video_nft(VideoId) ->
    gen_server:call({global, ?MODULE}, {get_video_nft, VideoId}).

add_collaboration(VideoId, CollaboratorId) ->
    gen_server:call({global, ?MODULE}, {add_collaboration, VideoId, CollaboratorId}).

set_remix_permissions(VideoId, Permissions) ->
    gen_server:call({global, ?MODULE}, {set_remix_permissions, VideoId, Permissions}).

set_revenue_split(VideoId, RevenueSplit) ->
    gen_server:call({global, ?MODULE}, {set_revenue_split, VideoId, RevenueSplit}).

pin_video(VideoId) ->
    gen_server:call({global, ?MODULE}, {pin_video, VideoId}, 30000).

unpin_video(VideoId) ->
    gen_server:call({global, ?MODULE}, {unpin_video, VideoId}, 30000).

update_pin_status(VideoId, PinInfo) ->
    gen_server:call({global, ?MODULE}, {update_pin_status, VideoId, PinInfo}).

report_video(ReporterId, VideoId, Type, Description) ->
    gen_server:call({global, ?MODULE}, {report_video, ReporterId, VideoId, Type, Description}).

set_age_restriction(VideoId, AgeRestriction) ->
    gen_server:call({global, ?MODULE}, {set_age_restriction, VideoId, AgeRestriction}).

set_content_warnings(VideoId, Warnings) ->
    gen_server:call({global, ?MODULE}, {set_content_warnings, VideoId, Warnings}).

search_videos(Query) ->
    gen_server:call({global, ?MODULE}, {search_videos, Query}).

search_videos_advanced(SearchParams) ->
    gen_server:call({global, ?MODULE}, {search_videos_advanced, SearchParams}).

get_trending_videos(Limit) ->
    gen_server:call({global, ?MODULE}, {get_trending_videos, Limit}).

get_featured_videos() ->
    gen_server:call({global, ?MODULE}, {get_featured_videos}).

increment_download_count(VideoId) ->
    gen_server:call({global, ?MODULE}, {increment_download_count, VideoId}).

start_raid(SourceStreamId, TargetStreamId, ViewerCount) ->
    gen_server:call({global, ?MODULE}, {start_raid, SourceStreamId, TargetStreamId, ViewerCount}).

host_channel(VideoId, HostedChannelId) ->
    gen_server:call({global, ?MODULE}, {host_channel, VideoId, HostedChannelId}).

add_moderator(VideoId, ModeratorId) ->
    gen_server:call({global, ?MODULE}, {add_moderator, VideoId, ModeratorId}).

remove_moderator(VideoId, ModeratorId) ->
    gen_server:call({global, ?MODULE}, {remove_moderator, VideoId, ModeratorId}).

add_vip(VideoId, VIPId) ->
    gen_server:call({global, ?MODULE}, {add_vip, VideoId, VIPId}).

remove_vip(VideoId, VIPId) ->
    gen_server:call({global, ?MODULE}, {remove_vip, VideoId, VIPId}).

ban_user(VideoId, UserId, Reason) ->
    gen_server:call({global, ?MODULE}, {ban_user, VideoId, UserId, Reason}).

enable_channel_points(VideoId) ->
    gen_server:call({global, ?MODULE}, {enable_channel_points, VideoId}).

create_channel_reward(VideoId, RewardName, Description, Cost, RewardType) ->
    gen_server:call({global, ?MODULE}, {create_channel_reward, VideoId, RewardName, Description, Cost, RewardType}).

redeem_reward(VideoId, UserId, RewardId) ->
    gen_server:call({global, ?MODULE}, {redeem_reward, VideoId, UserId, RewardId}).

enable_clip_creation(VideoId) ->
    gen_server:call({global, ?MODULE}, {enable_clip_creation, VideoId}).

disable_clip_creation(VideoId) ->
    gen_server:call({global, ?MODULE}, {disable_clip_creation, VideoId}).

add_simulcast_destination(VideoId, Destination) ->
    gen_server:call({global, ?MODULE}, {add_simulcast_destination, VideoId, Destination}).

remove_simulcast_destination(VideoId, Destination) ->
    gen_server:call({global, ?MODULE}, {remove_simulcast_destination, VideoId, Destination}).

calculate_trending_score(VideoId) ->
    gen_server:call({global, ?MODULE}, {calculate_trending_score, VideoId}).

update_analytics(VideoId) ->
    gen_server:call({global, ?MODULE}, {update_analytics, VideoId}).


init([]) ->
    ?LOG_NOTICE("Video server has been started - ~p", [self()]),
    PoolSize = application:get_env(social_network, video_worker_pool_size, 100),
    WorkerPool = initialize_worker_pool(PoolSize),
    {ok, #state{worker_pool_size = PoolSize, worker_pool = WorkerPool}}.

initialize_worker_pool(Size) ->
    [spawn_link(fun() -> worker_loop() end) || _ <- lists:seq(1, Size)].
worker_loop() ->
    receive
        {create_video, CreatorId, Title, Description, VideoFile, Duration, Privacy, Tags, AllowComments, AllowDownloads, Monetized, From} ->
            Result = videodb:create_video(CreatorId, Title, Description, VideoFile, Duration, Privacy, Tags, AllowComments, AllowDownloads, Monetized),
            gen_server:reply(From, Result),
            worker_loop();
        {create_video_concurrent, CreatorId, Title, Description, VideoFile, Duration, Privacy, Tags, AllowComments, AllowDownloads, Monetized, {Pid, RequestId}} ->
            Result = videodb:create_video_concurrent(CreatorId, Title, Description, VideoFile, Duration, Privacy, Tags, AllowComments, AllowDownloads, Monetized),
            Pid ! {video_creation_result, RequestId, Result},
            worker_loop();
        {create_video_from_file, CreatorId, Title, Description, FilePath, Duration, Privacy, Tags, AllowComments, Monetized, From} ->
            Result = videodb:create_video_from_file(CreatorId, Title, Description, FilePath, Duration, Privacy, Tags, AllowComments, Monetized),
            gen_server:reply(From, Result),
            worker_loop();
        {upload_video_file, VideoId, FilePath, From} ->
            Result = videodb:upload_video_file(VideoId, FilePath),
            gen_server:reply(From, Result),
            worker_loop();
        {create_video_version, VideoId, UserId, NewVideoFile, ChangeDescription, From} ->
            Result = videodb:create_video_version(VideoId, UserId, NewVideoFile, ChangeDescription),
            gen_server:reply(From, Result),
            worker_loop();
        {add_thumbnail, VideoId, ThumbnailData, Timestamp, From} ->
            Result = videodb:add_thumbnail(VideoId, ThumbnailData, Timestamp),
            gen_server:reply(From, Result),
            worker_loop();
        {add_preview, VideoId, PreviewData, From} ->
            Result = videodb:add_preview(VideoId, PreviewData),
            gen_server:reply(From, Result),
            worker_loop();
        {add_subtitle, VideoId, Language, SubtitleData, Format, From} ->
            Result = videodb:add_subtitle(VideoId, Language, SubtitleData, Format),
            gen_server:reply(From, Result),
            worker_loop();
        {add_audio_track, VideoId, Language, AudioData, TrackType, From} ->
            Result = videodb:add_audio_track(VideoId, Language, AudioData, TrackType),
            gen_server:reply(From, Result),
            worker_loop();
        {pin_video, VideoId, From} ->
            Result = videodb:pin_video(VideoId),
            gen_server:reply(From, Result),
            worker_loop();
        {unpin_video, VideoId, From} ->
            Result = videodb:unpin_video(VideoId),
            gen_server:reply(From, Result),
            worker_loop();
        {stop, From} ->
            From ! {stopped, self()};
        Other ->
            ?LOG_WARNING("Worker received unknown message: ~p", [Other]),
            worker_loop()
    end.

handle_call({create_video, CreatorId, Title, Description, VideoFile, Duration, Privacy, Tags, AllowComments, AllowDownloads, Monetized}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_video, CreatorId, Title, Description, VideoFile, Duration, Privacy, Tags, AllowComments, AllowDownloads, Monetized, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({create_video_from_file, CreatorId, Title, Description, FilePath, Duration, Privacy, Tags, AllowComments, Monetized}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_video_from_file, CreatorId, Title, Description, FilePath, Duration, Privacy, Tags, AllowComments, Monetized, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({upload_video_file, VideoId, FilePath}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {upload_video_file, VideoId, FilePath, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({create_video_version, VideoId, UserId, NewVideoFile, ChangeDescription}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_video_version, VideoId, UserId, NewVideoFile, ChangeDescription, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({add_thumbnail, VideoId, ThumbnailData, Timestamp}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {add_thumbnail, VideoId, ThumbnailData, Timestamp, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({add_preview, VideoId, PreviewData}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {add_preview, VideoId, PreviewData, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({add_subtitle, VideoId, Language, SubtitleData, Format}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {add_subtitle, VideoId, Language, SubtitleData, Format, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({add_audio_track, VideoId, Language, AudioData, TrackType}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {add_audio_track, VideoId, Language, AudioData, TrackType, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({pin_video, VideoId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {pin_video, VideoId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({unpin_video, VideoId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {unpin_video, VideoId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({update_video, VideoId, CreatorId, NewTitle, NewDescription, NewDuration, NewPrivacy, NewTags, NewAllowComments, NewAllowDownloads, NewAllowRemixes, NewMonetized}, _From, State) ->
    Res = videodb:update_video(VideoId, CreatorId, NewTitle, NewDescription, NewDuration, NewPrivacy, NewTags, NewAllowComments, NewAllowDownloads, NewAllowRemixes, NewMonetized),
    {reply, Res, State};
handle_call({delete_video, VideoId, UserId}, _From, State) ->
    Res = videodb:delete_video(VideoId, UserId),
    {reply, Res, State};
handle_call({get_video_by_id, VideoId}, _From, State) ->
    Res = videodb:get_video_by_id(VideoId),
    {reply, Res, State};
handle_call({get_videos_by_creator, CreatorId}, _From, State) ->
    Res = videodb:get_videos_by_creator(CreatorId),
    {reply, Res, State};
handle_call({get_videos_by_business, BusinessId}, _From, State) ->
    Res = videodb:get_videos_by_business(BusinessId),
    {reply, Res, State};
handle_call({get_public_videos}, _From, State) ->
    Res = videodb:get_public_videos(),
    {reply, Res, State};
handle_call({get_videos_by_tag, Tag}, _From, State) ->
    Res = videodb:get_videos_by_tag(Tag),
    {reply, Res, State};
handle_call({get_video_content, VideoId}, _From, State) ->
    Res = videodb:get_video_content(VideoId),
    {reply, Res, State};
handle_call({get_video_metadata, VideoId}, _From, State) ->
    Res = videodb:get_video_metadata(VideoId),
    {reply, Res, State};
handle_call({get_video_qualities, VideoId}, _From, State) ->
    Res = videodb:get_video_qualities(VideoId),
    {reply, Res, State};
handle_call({get_video_versions, VideoId}, _From, State) ->
    Res = videodb:get_video_versions(VideoId),
    {reply, Res, State};
handle_call({add_chapter, VideoId, Title, StartTime, EndTime}, _From, State) ->
    Res = videodb:add_chapter(VideoId, Title, StartTime, EndTime),
    {reply, Res, State};
handle_call({react_to_video, UserID, VideoId, ReactionType}, _From, State) ->
    Res = videodb:react_to_video(UserID, VideoId, ReactionType),
    {reply, Res, State};
handle_call({remove_reaction_from_video, LikeID, VideoId}, _From, State) ->
    Res = videodb:remove_reaction_from_video(LikeID, VideoId),
    {reply, Res, State};
handle_call({get_reactions_by_type, VideoId, ReactionType}, _From, State) ->
    Res = videodb:get_reactions_by_type(VideoId, ReactionType),
    {reply, Res, State};
handle_call({get_all_reactions, VideoId}, _From, State) ->
    Res = videodb:get_all_reactions(VideoId),
    {reply, Res, State};
handle_call({get_reaction_counts, VideoId}, _From, State) ->
    Res = videodb:get_reaction_counts(VideoId),
    {reply, Res, State};
handle_call({has_user_reacted_with_type, UserID, VideoId, ReactionType}, _From, State) ->
    Res = videodb:has_user_reacted_with_type(UserID, VideoId, ReactionType),
    {reply, Res, State};
handle_call({get_user_reaction_type, UserID, VideoId}, _From, State) ->
    Res = videodb:get_user_reaction_type(UserID, VideoId),
    {reply, Res, State};
handle_call({add_video_comment, Author, VideoId, Content}, _From, State) ->
    Res = videodb:add_video_comment(Author, VideoId, Content),
    {reply, Res, State};
handle_call({get_video_comments, VideoId}, _From, State) ->
    Res = videodb:get_video_comments(VideoId),
    {reply, Res, State};
handle_call({get_comment_count, VideoId}, _From, State) ->
    Res = videodb:get_comment_count(VideoId),
    {reply, Res, State};
handle_call({increment_view_count, VideoId, UserId}, _From, State) ->
    Res = videodb:increment_view_count(VideoId, UserId),
    {reply, Res, State};
handle_call({increment_unique_view, VideoId, UserId}, _From, State) ->
    Res = videodb:increment_unique_view(VideoId, UserId),
    {reply, Res, State};
handle_call({track_watch_time, VideoId, UserId, WatchSeconds}, _From, State) ->
    Res = videodb:track_watch_time(VideoId, UserId, WatchSeconds),
    {reply, Res, State};
handle_call({get_view_stats, VideoId}, _From, State) ->
    Res = videodb:get_view_stats(VideoId),
    {reply, Res, State};
handle_call({share_video, VideoId, UserId}, _From, State) ->
    Res = videodb:share_video(VideoId, UserId),
    {reply, Res, State};
handle_call({save_video, VideoId, UserId}, _From, State) ->
    Res = videodb:save_video(VideoId, UserId),
    {reply, Res, State};
handle_call({unsave_video, VideoId, UserId}, _From, State) ->
    Res = videodb:unsave_video(VideoId, UserId),
    {reply, Res, State};
handle_call({add_to_playlist, VideoId, PlaylistId}, _From, State) ->
    Res = videodb:add_to_playlist(VideoId, PlaylistId),
    {reply, Res, State};
handle_call({remove_from_playlist, VideoId, PlaylistId}, _From, State) ->
    Res = videodb:remove_from_playlist(VideoId, PlaylistId),
    {reply, Res, State};
handle_call({link_to_series, VideoId, SeriesId, EpisodeNumber}, _From, State) ->
    Res = videodb:link_to_series(VideoId, SeriesId, EpisodeNumber),
    {reply, Res, State};
handle_call({add_interactive_hotspot, VideoId, Timestamp, Duration, Position, Content}, _From, State) ->
    Res = videodb:add_interactive_hotspot(VideoId, Timestamp, Duration, Position, Content),
    {reply, Res, State};
handle_call({create_poll, VideoId, Timestamp, Question, Options}, _From, State) ->
    Res = videodb:create_poll(VideoId, Timestamp, Question, Options),
    {reply, Res, State};
handle_call({vote_on_poll, PollId, UserId, OptionIndex}, _From, State) ->
    Res = videodb:vote_on_poll(PollId, UserId, OptionIndex),
    {reply, Res, State};
handle_call({set_monetization, VideoId, RevenueModel, Price}, _From, State) ->
    Res = videodb:set_monetization(VideoId, RevenueModel, Price),
    {reply, Res, State};
handle_call({add_ad_break, VideoId, Timestamp, AdType}, _From, State) ->
    Res = videodb:add_ad_break(VideoId, Timestamp, AdType),
    {reply, Res, State};
handle_call({set_sponsor_info, VideoId, SponsorInfo}, _From, State) ->
    Res = videodb:set_sponsor_info(VideoId, SponsorInfo),
    {reply, Res, State};
handle_call({track_viewer_retention, VideoId, Timestamp, PercentageViewing}, _From, State) ->
    Res = videodb:track_viewer_retention(VideoId, Timestamp, PercentageViewing),
    {reply, Res, State};
handle_call({track_geographic_view, VideoId, Country, City}, _From, State) ->
    Res = videodb:track_geographic_view(VideoId, Country, City),
    {reply, Res, State};
handle_call({track_device_view, VideoId, DeviceType}, _From, State) ->
    Res = videodb:track_device_view(VideoId, DeviceType),
    {reply, Res, State};
handle_call({track_referral, VideoId, Source}, _From, State) ->
    Res = videodb:track_referral(VideoId, Source),
    {reply, Res, State};
handle_call({start_live_stream, VideoId, StreamUrl}, _From, State) ->
    Res = videodb:start_live_stream(VideoId, StreamUrl),
    {reply, Res, State};
handle_call({end_live_stream, VideoId}, _From, State) ->
    Res = videodb:end_live_stream(VideoId),
    {reply, Res, State};
handle_call({update_live_viewers, VideoId, ViewerCount}, _From, State) ->
    Res = videodb:update_live_viewers(VideoId, ViewerCount),
    {reply, Res, State};
handle_call({get_live_stream_info, VideoId}, _From, State) ->
    Res = videodb:get_live_stream_info(VideoId),
    {reply, Res, State};
handle_call({create_video_clip, SourceVideoId, CreatorId, Title, StartTimestamp, EndTimestamp}, _From, State) ->
    Res = videodb:create_video_clip(SourceVideoId, CreatorId, Title, StartTimestamp, EndTimestamp),
    {reply, Res, State};
handle_call({enable_duet, VideoId}, _From, State) ->
    Res = videodb:enable_duet(VideoId),
    {reply, Res, State};
handle_call({disable_duet, VideoId}, _From, State) ->
    Res = videodb:disable_duet(VideoId),
    {reply, Res, State};
handle_call({add_duet_video, VideoId, DuetVideoId}, _From, State) ->
    Res = videodb:add_duet_video(VideoId, DuetVideoId),
    {reply, Res, State};
handle_call({enable_stitch, VideoId}, _From, State) ->
    Res = videodb:enable_stitch(VideoId),
    {reply, Res, State};
handle_call({disable_stitch, VideoId}, _From, State) ->
    Res = videodb:disable_stitch(VideoId),
    {reply, Res, State};
handle_call({add_stitch_video, VideoId, StitchVideoId}, _From, State) ->
    Res = videodb:add_stitch_video(VideoId, StitchVideoId),
    {reply, Res, State};
handle_call({add_effect, VideoId, Effect}, _From, State) ->
    Res = videodb:add_effect(VideoId, Effect),
    {reply, Res, State};
handle_call({add_filter, VideoId, Filter}, _From, State) ->
    Res = videodb:add_filter(VideoId, Filter),
    {reply, Res, State};
handle_call({set_sound, VideoId, SoundId}, _From, State) ->
    Res = videodb:set_sound(VideoId, SoundId),
    {reply, Res, State};
handle_call({create_challenge, CreatorId, ChallengeName, Hashtag}, _From, State) ->
    Res = videodb:create_challenge(CreatorId, ChallengeName, Hashtag),
    {reply, Res, State};
handle_call({submit_to_challenge, VideoId, ChallengeId}, _From, State) ->
    Res = videodb:submit_to_challenge(VideoId, ChallengeId),
    {reply, Res, State};
handle_call({mint_video_nft, VideoId, CreatorId, Blockchain, TokenStandard, EditionNumber, TotalEditions}, _From, State) ->
    Res = videodb:mint_video_nft(VideoId, CreatorId, Blockchain, TokenStandard, EditionNumber, TotalEditions),
    {reply, Res, State};
handle_call({get_video_nft, VideoId}, _From, State) ->
    Res = videodb:get_video_nft(VideoId),
    {reply, Res, State};
handle_call({add_collaboration, VideoId, CollaboratorId}, _From, State) ->
    Res = videodb:add_collaboration(VideoId, CollaboratorId),
    {reply, Res, State};
handle_call({set_remix_permissions, VideoId, Permissions}, _From, State) ->
    Res = videodb:set_remix_permissions(VideoId, Permissions),
    {reply, Res, State};
handle_call({set_revenue_split, VideoId, RevenueSplit}, _From, State) ->
    Res = videodb:set_revenue_split(VideoId, RevenueSplit),
    {reply, Res, State};
handle_call({update_pin_status, VideoId, PinInfo}, _From, State) ->
    Res = videodb:update_pin_status(VideoId, PinInfo),
    {reply, Res, State};
handle_call({report_video, ReporterId, VideoId, Type, Description}, _From, State) ->
    Res = videodb:report_video(ReporterId, VideoId, Type, Description),
    {reply, Res, State};
handle_call({set_age_restriction, VideoId, AgeRestriction}, _From, State) ->
    Res = videodb:set_age_restriction(VideoId, AgeRestriction),
    {reply, Res, State};
handle_call({set_content_warnings, VideoId, Warnings}, _From, State) ->
    Res = videodb:set_content_warnings(VideoId, Warnings),
    {reply, Res, State};
handle_call({search_videos, Query}, _From, State) ->
    Res = videodb:search_videos(Query),
    {reply, Res, State};
handle_call({search_videos_advanced, SearchParams}, _From, State) ->
    Res = videodb:search_videos_advanced(SearchParams),
    {reply, Res, State};
handle_call({get_trending_videos, Limit}, _From, State) ->
    Res = videodb:get_trending_videos(Limit),
    {reply, Res, State};
handle_call({get_featured_videos}, _From, State) ->
    Res = videodb:get_featured_videos(),
    {reply, Res, State};
handle_call({increment_download_count, VideoId}, _From, State) ->
    Res = videodb:increment_download_count(VideoId),
    {reply, Res, State};
handle_call({start_raid, SourceStreamId, TargetStreamId, ViewerCount}, _From, State) ->
    Res = videodb:start_raid(SourceStreamId, TargetStreamId, ViewerCount),
    {reply, Res, State};
handle_call({host_channel, VideoId, HostedChannelId}, _From, State) ->
    Res = videodb:host_channel(VideoId, HostedChannelId),
    {reply, Res, State};
handle_call({add_moderator, VideoId, ModeratorId}, _From, State) ->
    Res = videodb:add_moderator(VideoId, ModeratorId),
    {reply, Res, State};
handle_call({remove_moderator, VideoId, ModeratorId}, _From, State) ->
    Res = videodb:remove_moderator(VideoId, ModeratorId),
    {reply, Res, State};
handle_call({add_vip, VideoId, VIPId}, _From, State) ->
    Res = videodb:add_vip(VideoId, VIPId),
    {reply, Res, State};
handle_call({remove_vip, VideoId, VIPId}, _From, State) ->
    Res = videodb:remove_vip(VideoId, VIPId),
    {reply, Res, State};
handle_call({ban_user, VideoId, UserId, Reason}, _From, State) ->
    Res = videodb:ban_user(VideoId, UserId, Reason),
    {reply, Res, State};
handle_call({enable_channel_points, VideoId}, _From, State) ->
    Res = videodb:enable_channel_points(VideoId),
    {reply, Res, State};
handle_call({create_channel_reward, VideoId, RewardName, Description, Cost, RewardType}, _From, State) ->
    Res = videodb:create_channel_reward(VideoId, RewardName, Description, Cost, RewardType),
    {reply, Res, State};
handle_call({redeem_reward, VideoId, UserId, RewardId}, _From, State) ->
    Res = videodb:redeem_reward(VideoId, UserId, RewardId),
    {reply, Res, State};
handle_call({enable_clip_creation, VideoId}, _From, State) ->
    Res = videodb:enable_clip_creation(VideoId),
    {reply, Res, State};
handle_call({disable_clip_creation, VideoId}, _From, State) ->
    Res = videodb:disable_clip_creation(VideoId),
    {reply, Res, State};
handle_call({add_simulcast_destination, VideoId, Destination}, _From, State) ->
    Res = videodb:add_simulcast_destination(VideoId, Destination),
    {reply, Res, State};
handle_call({remove_simulcast_destination, VideoId, Destination}, _From, State) ->
    Res = videodb:remove_simulcast_destination(VideoId, Destination),
    {reply, Res, State};
handle_call({calculate_trending_score, VideoId}, _From, State) ->
    Res = videodb:calculate_trending_score(VideoId),
    {reply, Res, State};
handle_call({update_analytics, VideoId}, _From, State) ->
    Res = videodb:update_analytics(VideoId),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.
handle_cast({create_video_concurrent, CreatorId, Title, Description, VideoFile, Duration, Privacy, Tags, AllowComments, AllowDownloads, Monetized, {Pid, RequestId}}, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_video_concurrent, CreatorId, Title, Description, VideoFile, Duration, Privacy, Tags, AllowComments, AllowDownloads, Monetized, {Pid, RequestId}},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info({'EXIT', Pid, Reason}, State = #state{worker_pool = Workers}) ->
    case lists:member(Pid, Workers) of
        true ->
            ?LOG_WARNING("Worker ~p crashed with reason: ~p. Replacing.", [Pid, Reason]),
            NewWorker = spawn_link(fun() -> worker_loop() end),
            NewWorkers = lists:delete(Pid, Workers) ++ [NewWorker],
            {noreply, State#state{worker_pool = NewWorkers}};
        false ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, #state{worker_pool = Workers}) ->
    [Worker ! {stop, self()} || Worker <- Workers],
    [receive {stopped, _W} -> ok after 1000 -> ok end || _ <- Workers],
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
