-module(musicdb).
-author("Zaryn Technologies").

-export([
    create_music/11,
    create_music_concurrent/11,
    create_music_from_file/10,
    upload_music_file/2,
    update_music/12,
    delete_music/2,

    get_music_by_id/1,
    get_music_by_creator/1,
    get_music_by_business/1,
    get_music_by_artist/1,
    get_public_music/0,
    get_music_by_genre/1,
    get_music_by_mood/1,
    get_music_content/1,
    get_music_metadata/1,
    get_music_qualities/1,

    create_music_version/4,
    get_music_versions/1,
    add_album_art/2,
    add_waveform/2,
    add_lyrics/3,
    add_synced_lyrics/2,

    react_to_music/3,
    remove_reaction_from_music/2,
    get_reactions_by_type/2,
    get_all_reactions/1,
    get_reaction_counts/1,
    has_user_reacted_with_type/3,
    get_user_reaction_type/2,

    add_music_comment/3,
    get_music_comments/1,
    get_music_comment_content/1,
    update_music_comment/2,
    delete_music_comment/2,
    react_to_music_comment/3,
    get_comment_reactions/1,
    get_user_comment_reaction/2,
    get_comment_count/1,

    increment_play_count/2,
    increment_unique_listener/2,
    track_listen_time/3,
    get_listen_stats/1,
    get_unique_listener_count/1,

    share_music/2,
    share_music/3,
    save_music/2,
    unsave_music/2,

    add_to_playlist/2,
    remove_from_playlist/2,
    link_to_album/2,

    set_monetization/3,
    set_streaming_royalties/2,

    track_geographic_play/3,
    track_device_play/2,
    track_referral/2,

    add_to_radio_seed/2,
    calculate_discovery_score/1,

    create_stem/4,
    get_stems/1,
    enable_remix/1,
    disable_remix/1,
    create_remix/4,

    add_collaborator/2,
    set_royalty_split/2,

    pin_music/1,
    unpin_music/1,
    update_pin_status/2,

    report_music/4,
    set_age_restriction/2,
    set_content_warnings/2,

    search_music/1,
    search_music_advanced/1,
    get_trending_music/1,
    get_featured_music/0,

    increment_download_count/1,
    increment_skip_count/1,
    increment_replay_count/1,

    add_to_listening_hours/2,
    track_completion_rate/2,

    calculate_trending_score/1,
    update_analytics/1,

    create_music_with_rust/11,
    upload_music_to_ipfs_simple/2,
    track_music_playback/3,
    update_playback_position/2,
    get_music_analytics/1,
    export_music_analytics/1,
    detect_music_format/1,
    get_music_info_from_rust/1,
    get_music_cid/1,
    update_music_ipns/2,
    get_music/1,

    add_timed_comment/4,
    get_timed_comments/1,
    get_comments_at_timestamp/2,

    submit_to_playlist/2,
    feature_on_editorial/2,

    create_spatial_audio/3,
    add_separated_stem/3,

    set_pre_release/2,
    schedule_premiere/2,

    add_listener_journey/3,
    track_skip_point/3,
    track_replay_section/4,
    create_artist_request/3,
    get_artist_request_status/1,
    approve_artist_request/2,
    reject_artist_request/3,
    get_artist_by_user_id/1,
    get_pending_artist_requests/0
]).

-include("../records.hrl").
-include("../media_records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("kernel/include/file.hrl").

-define(DEFAULT_CONCURRENCY, 5).
-define(DEFAULT_TIMEOUT, 30000).
-define(MAX_RETRIES, 10).
-define(BACKOFF_TIME, 20).
-define(MAX_FILE_SIZE, 5368709120).
-define(CHUNK_SIZE, 10485760).
-define(SUPPORTED_FORMATS, [
    ".mp3", ".wav", ".flac", ".aac", ".ogg",
    ".m4a", ".wma", ".opus", ".alac", ".aiff"
]).

create_music(CreatorId, Title, Artist, Album, Duration, Genre, Privacy, AllowComments, AllowDownloads, AllowRemixes, Monetized) ->
    case userdb:get_user_by_id(CreatorId) of
        user_not_exist ->
            error_logger:warning_msg("User ~p not found when creating music", [CreatorId]),
            {error, user_not_found};
        error ->
            {error, user_lookup_failed};
        _User ->
            Fun = fun() ->
                Id = nanoid:gen(),
                Now = calendar:universal_time(),
                AI_Music_ID = ai_musicdb:insert_in_transaction(Id),

                Music = #music{
                    id = Id,
                    ai_music_id = AI_Music_ID,
                    user_id = CreatorId,
                    file_url = {pending, Id},
                    ipfs_cid = {pending, Id},
                    status = processing,
                    title = Title,
                    artist = Artist,
                    album = Album,
                    duration_seconds = Duration,
                    genre = Genre,
                    privacy = Privacy,
                    allow_comments = AllowComments,
                    allow_downloads = AllowDownloads,
                    allow_remixes = AllowRemixes,
                    monetized = Monetized,
                    date_created = Now,
                    date_updated = Now,
                    plays = 0,
                    unique_listeners = 0,
                    likes = [],
                    reactions = #{
                        love => [],
                        fire => [],
                        sad => [],
                        party => [],
                        vibe => []
                    },
                    reaction_counts = #{
                        love => 0,
                        fire => 0,
                        sad => 0,
                        party => 0,
                        vibe => 0
                    },
                    comments = [],
                    shares = 0,
                    saves = 0
                },

                mnesia:write(Music),

                case mnesia:read({user, CreatorId}) of
                    [User] ->
                        mnesia:write(User#user{media = [Id | User#user.media]});
                    [] ->
                        ok
                end,

                {ok, Id}
            end,

            case mnesia:transaction(Fun) of
                {atomic, {ok, Id}} -> Id;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end
    end.

create_music_concurrent(CreatorId, Title, Artist, Album, Duration, Genre, Privacy, AllowComments, AllowDownloads, AllowRemixes, Monetized) ->
    case userdb:get_user_by_id(CreatorId) of
        user_not_exist ->
            error_logger:warning_msg("User ~p not found when creating music concurrently", [CreatorId]),
            {error, user_not_found};
        error ->
            {error, user_lookup_failed};
        _User ->
            IdFuture = spawn_monitor(fun() -> exit({result, nanoid:gen()}) end),
            Id = receive_result(IdFuture),

            Now = calendar:universal_time(),

            AIMusicIdFuture = spawn_monitor(fun() ->
                exit({result, ai_musicdb:insert(Id)})
            end),

            AI_Music_ID = receive_result(AIMusicIdFuture),

            Music = #music{
                id = Id,
                ai_music_id = AI_Music_ID,
                user_id = CreatorId,
                file_url = {pending, Id},
                ipfs_cid = {pending, Id},
                status = processing,
                title = Title,
                artist = Artist,
                album = Album,
                duration_seconds = Duration,
                genre = Genre,
                privacy = Privacy,
                allow_comments = AllowComments,
                allow_downloads = AllowDownloads,
                allow_remixes = AllowRemixes,
                monetized = Monetized,
                date_created = Now,
                date_updated = Now,
                plays = 0,
                unique_listeners = 0,
                likes = [],
                reactions = #{
                    love => [],
                    fire => [],
                    sad => [],
                    party => [],
                    vibe => []
                },
                reaction_counts = #{
                    love => 0,
                    fire => 0,
                    sad => 0,
                    party => 0,
                    vibe => 0
                },
                comments = [],
                shares = 0,
                saves = 0
            },

            case write_music_with_retry(Music, CreatorId, ?MAX_RETRIES) of
                ok -> Id;
                {error, Reason} -> {error, Reason}
            end
    end.

create_music_from_file(CreatorId, Title, Artist, Album, FilePath, Duration, Genre, Privacy, AllowComments, Monetized) ->
    case userdb:get_user_by_id(CreatorId) of
        user_not_exist ->
            {error, user_not_found};
        error ->
            {error, user_lookup_failed};
        _User ->
            case validate_music_file(FilePath) of
                {error, Reason} ->
                    {error, Reason};
                {ok, _FileInfo} ->
                    case file:read_file(FilePath) of
                        {ok, _FileContent} ->
                            create_music(CreatorId, Title, Artist, Album, Duration, Genre, Privacy, AllowComments, true, false, Monetized);
                        {error, Reason} ->
                            {error, {file_read_error, Reason}}
                    end
            end
    end.

upload_music_file(MusicId, FilePath) ->
    case validate_music_file(FilePath) of
        {error, Reason} ->
            {error, Reason};
        {ok, _FileInfo} ->
            case file:read_file(FilePath) of
                {ok, FileContent} ->
                    Fun = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [] ->
                                {error, music_not_found};
                            [Music] ->
                                Now = calendar:universal_time(),
                                SizeBytes = byte_size(FileContent),

                                ok = content_cache:set({music_file_update, MusicId}, FileContent),

                                UpdatedMusic = Music#music{
                                    file_url = {pending_update, MusicId},
                                    ipfs_cid = {pending_update, MusicId},
                                    status = processing,
                                    file_size_bytes = SizeBytes,
                                    date_updated = Now
                                },

                                mnesia:write(UpdatedMusic),

                                spawn(fun() ->
                                    upload_music_update(MusicId, FileContent)
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

update_music(MusicId, CreatorId, NewTitle, NewArtist, NewAlbum, NewDuration, NewGenre, NewPrivacy, NewAllowComments, NewAllowDownloads, NewAllowRemixes, NewMonetized) ->
    case userdb:get_user_by_id(CreatorId) of
        user_not_exist ->
            {error, user_not_found};
        error ->
            {error, user_lookup_failed};
        _User ->
            Fun = fun() ->
                case mnesia:read({music, MusicId}) of
                    [] ->
                        {error, music_not_found};
                    [Music] ->
                        case Music#music.user_id of
                            CreatorId ->
                                Now = calendar:universal_time(),

                                UpdatedMusic = Music#music{
                                    title = NewTitle,
                                    artist = NewArtist,
                                    album = NewAlbum,
                                    duration_seconds = NewDuration,
                                    genre = NewGenre,
                                    privacy = NewPrivacy,
                                    allow_comments = NewAllowComments,
                                    allow_downloads = NewAllowDownloads,
                                    allow_remixes = NewAllowRemixes,
                                    monetized = NewMonetized,
                                    date_updated = Now
                                },

                                mnesia:write(UpdatedMusic),
                                ok;
                            _ ->
                                {error, unauthorized}
                        end
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end
    end.

delete_music(MusicId, UserId) ->
    case userdb:get_user_by_id(UserId) of
        user_not_exist ->
            {error, user_not_found};
        error ->
            {error, user_lookup_failed};
        _User ->
            Fun = fun() ->
                case mnesia:read({music, MusicId}) of
                    [] ->
                        {error, music_not_found};
                    [Music] ->
                        case Music#music.user_id of
                            UserId ->
                                mnesia:delete({music, MusicId}),

                                case mnesia:read({user, UserId}) of
                                    [User] ->
                                        UpdatedMedia = lists:delete(MusicId, User#user.media),
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
            end
    end.

get_music_by_id(MusicId) ->
    Fun = fun() ->
        case mnesia:read({music, MusicId}) of
            [] -> {error, music_not_found};
            [Music] -> Music
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_music_by_creator(CreatorId) ->
    Fun = fun() ->
        mnesia:match_object(#music{user_id = CreatorId, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_music_by_business(BusinessId) ->
    Fun = fun() ->
        mnesia:match_object(#music{business_id = BusinessId, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_music_by_artist(ArtistId) ->
    Fun = fun() ->
        mnesia:match_object(#music{artist_id = ArtistId, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_public_music() ->
    Fun = fun() ->
        mnesia:match_object(#music{privacy = public, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_music_by_genre(Genre) ->
    Fun = fun() ->
        AllMusic = mnesia:match_object(#music{_ = '_'}),
        lists:filter(fun(Music) ->
            lists:member(Genre, Music#music.genre)
        end, AllMusic)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_music_by_mood(Mood) ->
    Fun = fun() ->
        AllMusic = mnesia:match_object(#music{_ = '_'}),
        lists:filter(fun(Music) ->
            lists:member(Mood, Music#music.mood)
        end, AllMusic)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_music_content(MusicId) ->
    Fun = fun() ->
        case mnesia:read({music, MusicId}) of
            [] -> {error, music_not_found};
            [Music] ->
                IPFSCID = Music#music.ipfs_cid,
                case IPFSCID of
                    {pending, Id} when Id =:= MusicId ->
                        case content_cache:get({music_file, Id}) of
                            undefined -> {error, content_not_ready};
                            CachedContent -> {ok, CachedContent}
                        end;
                    _ ->
                        try
                            ActualContent = ipfs_audio:get_audio_binary(IPFSCID),
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

get_music_metadata(MusicId) ->
    Fun = fun() ->
        case mnesia:read({music, MusicId}) of
            [] -> {error, music_not_found};
            [Music] -> {ok, Music#music.data}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_music_qualities(MusicId) ->
    Fun = fun() ->
        case mnesia:read({music, MusicId}) of
            [] -> {error, music_not_found};
            [Music] -> {ok, Music#music.available_qualities}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

create_music_version(MusicId, UserId, NewMusicFile, ChangeDescription) ->
    case userdb:get_user_by_id(UserId) of
        user_not_exist ->
            {error, user_not_found};
        error ->
            {error, user_lookup_failed};
        _User ->
            Fun = fun() ->
                case mnesia:read({music, MusicId}) of
                    [] ->
                        {error, music_not_found};
                    [Music] ->
                        case Music#music.user_id of
                            UserId ->
                                Now = calendar:universal_time(),

                                content_cache:set({music_version, MusicId}, NewMusicFile),

                                VersionNum = length(maps:get(version_history, Music#music.data, [])) + 1,
                                NewVersionEntry = {VersionNum, {pending_version, MusicId}, Now, ChangeDescription},

                                CurrentData = Music#music.data,
                                VersionHistory = maps:get(version_history, CurrentData, []),
                                UpdatedVersionHistory = [NewVersionEntry | VersionHistory],
                                UpdatedData = maps:put(version_history, UpdatedVersionHistory, CurrentData),

                                UpdatedMusic = Music#music{
                                    ipfs_cid = {pending_version, MusicId},
                                    status = processing,
                                    data = UpdatedData,
                                    date_updated = Now
                                },

                                mnesia:write(UpdatedMusic),

                                spawn(fun() ->
                                    upload_music_version(MusicId, NewMusicFile)
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
            end
    end.

    get_music_versions(MusicId) ->
        Fun = fun() ->
            case mnesia:read({music, MusicId}) of
                [] -> {error, music_not_found};
                [Music] ->
                    VersionHistory = maps:get(version_history, Music#music.data, []),
                    {ok, VersionHistory}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    add_album_art(MusicId, AlbumArtData) ->
        Fun = fun() ->
            case mnesia:read({music, MusicId}) of
                [] -> {error, music_not_found};
                [Music] ->
                    AlbumArtCID = ipfs_media:upload_media(AlbumArtData),

                    mnesia:write(Music#music{
                        album_art_cid = AlbumArtCID,
                        album_art_url = AlbumArtCID
                    }),

                    {ok, AlbumArtCID}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    add_waveform(MusicId, WaveformData) ->
        Fun = fun() ->
            case mnesia:read({music, MusicId}) of
                [] -> {error, music_not_found};
                [Music] ->
                    WaveformCID = ipfs_content:upload_text(WaveformData),

                    mnesia:write(Music#music{
                        waveform_cid = WaveformCID,
                        peaks_data = WaveformData
                    }),

                    {ok, WaveformCID}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    add_lyrics(MusicId, LyricsText, Language) ->
        Fun = fun() ->
            case mnesia:read({music, MusicId}) of
                [] -> {error, music_not_found};
                [Music] ->
                    LyricsCID = ipfs_content:upload_text(LyricsText),

                    mnesia:write(Music#music{
                        lyrics_cid = LyricsCID,
                        language = Language
                    }),

                    {ok, LyricsCID}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    add_synced_lyrics(MusicId, SyncedLyricsData) ->
        Fun = fun() ->
            case mnesia:read({music, MusicId}) of
                [] -> {error, music_not_found};
                [Music] ->
                    mnesia:write(Music#music{lyrics_synced = SyncedLyricsData}),
                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    react_to_music(UserID, MusicId, ReactionType) ->
        case userdb:get_user_by_id(UserID) of
            user_not_exist ->
                {error, user_not_found};
            error ->
                {error, user_lookup_failed};
            _User ->
                ValidReactions = [love, fire, sad, party, vibe],
                case lists:member(ReactionType, ValidReactions) of
                    false -> {error, invalid_reaction_type};
                    true ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    ExistingReaction = find_user_reaction_in_music(Music, UserID),

                                    case ExistingReaction of
                                        {found, OldType, OldLikeID} when OldType =:= ReactionType ->
                                            Reactions = Music#music.reactions,
                                            ReactionList = maps:get(OldType, Reactions, []),
                                            UpdatedList = lists:delete(OldLikeID, ReactionList),
                                            UpdatedReactions = maps:put(OldType, UpdatedList, Reactions),

                                            ReactionCounts = Music#music.reaction_counts,
                                            CurrentCount = maps:get(OldType, ReactionCounts, 0),
                                            UpdatedCount = max(0, CurrentCount - 1),
                                            UpdatedReactionCounts = maps:put(OldType, UpdatedCount, ReactionCounts),

                                            mnesia:delete({like, OldLikeID}),

                                            mnesia:write(Music#music{
                                                reactions = UpdatedReactions,
                                                reaction_counts = UpdatedReactionCounts
                                            }),
                                            {removed, OldType};
                                        {found, OldType, OldLikeID} ->
                                            Reactions = Music#music.reactions,
                                            OldReactionList = maps:get(OldType, Reactions, []),
                                            UpdatedOldList = lists:delete(OldLikeID, OldReactionList),
                                            IntermediateReactions = maps:put(OldType, UpdatedOldList, Reactions),

                                            ReactionCounts = Music#music.reaction_counts,
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

                                            mnesia:write(Music#music{
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

                                            Reactions = Music#music.reactions,
                                            ReactionList = maps:get(ReactionType, Reactions, []),
                                            UpdatedReactions = maps:put(ReactionType, [ID | ReactionList], Reactions),

                                            ReactionCounts = Music#music.reaction_counts,
                                            CurrentCount = maps:get(ReactionType, ReactionCounts, 0),
                                            UpdatedReactionCounts = maps:put(ReactionType, CurrentCount + 1, ReactionCounts),

                                            mnesia:write(Music#music{
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
                end
        end.

    find_user_reaction_in_music(Music, UserID) ->
        Reactions = Music#music.reactions,
        ReactionTypes = [love, fire, sad, party, vibe],
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

    remove_reaction_from_music(LikeID, MusicId) ->
        Fun = fun() ->
            case mnesia:read({like, LikeID}) of
                [] -> {error, reaction_not_found};
                [Like] ->
                    ReactionType = Like#like.reaction_type,

                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            Reactions = Music#music.reactions,
                            ReactionList = maps:get(ReactionType, Reactions, []),
                            UpdatedReactionList = lists:delete(LikeID, ReactionList),
                            UpdatedReactions = maps:put(ReactionType, UpdatedReactionList, Reactions),

                            ReactionCounts = Music#music.reaction_counts,
                            CurrentCount = maps:get(ReactionType, ReactionCounts, 0),
                            NewCount = max(0, CurrentCount - 1),
                            UpdatedReactionCounts = maps:put(ReactionType, NewCount, ReactionCounts),

                            mnesia:write(Music#music{
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

    get_reactions_by_type(MusicId, ReactionType) ->
        Fun = fun() ->
            case mnesia:read({music, MusicId}) of
                [] -> [];
                [Music] ->
                    Reactions = Music#music.reactions,
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

    get_all_reactions(MusicId) ->
        Fun = fun() ->
            case mnesia:read({music, MusicId}) of
                [] -> #{};
                [Music] ->
                    Reactions = Music#music.reactions,
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

    get_reaction_counts(MusicId) ->
        Fun = fun() ->
            case mnesia:read({music, MusicId}) of
                [] -> #{};
                [Music] -> Music#music.reaction_counts
            end
        end,
        {atomic, Res} = mnesia:transaction(Fun),
        Res.

    has_user_reacted_with_type(UserID, MusicId, ReactionType) ->
        case userdb:get_user_by_id(UserID) of
            user_not_exist ->
                false;
            error ->
                false;
            _User ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> false;
                        [Music] ->
                            Reactions = Music#music.reactions,
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
                end
        end.

    get_user_reaction_type(UserID, MusicId) ->
        case userdb:get_user_by_id(UserID) of
            user_not_exist ->
                undefined;
            error ->
                undefined;
            _User ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> undefined;
                        [Music] ->
                            Reactions = Music#music.reactions,
                            find_user_music_reaction_type(UserID, Reactions)
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, _Reason} -> undefined
                end
        end.

    find_user_music_reaction_type(UserID, Reactions) ->
        ReactionTypes = [love, fire, sad, party, vibe],
        find_user_music_reaction_type_in_types(UserID, ReactionTypes, Reactions).

    find_user_music_reaction_type_in_types(_UserID, [], _Reactions) ->
        undefined;
    find_user_music_reaction_type_in_types(UserID, [Type | Rest], Reactions) ->
        ReactionList = maps:get(Type, Reactions, []),
        case lists:any(fun(ID) ->
            case mnesia:read({like, ID}) of
                [Like] -> Like#like.userID =:= UserID;
                [] -> false
            end
        end, ReactionList) of
            true -> Type;
            false -> find_user_music_reaction_type_in_types(UserID, Rest, Reactions)
        end.

    add_music_comment(Author, MusicId, Content) ->
        Fun = fun() ->
            Id = nanoid:gen(),
            Date = calendar:universal_time(),
            UserID = userdb:get_user_id(Author),
            case UserID of
                {error, _} ->
                    error_logger:warning_msg("User ~p not found when adding music comment", [Author]),
                    {error, user_not_found};
                ValidUserID ->
                    ContentToCache = if
                        is_binary(Content) -> binary_to_list(Content);
                        true -> Content
                    end,
                    ok = content_cache:set(Id, ContentToCache),
                    PlaceholderContent = Id,
                    case mnesia:read({music, MusicId}) of
                        [] ->
                            {error, music_not_found};
                        [Music] ->
                            case Music#music.allow_comments of
                                false -> {error, comments_disabled};
                                true ->
                                    Comment = #comment{
                                        id = Id,
                                        user_id = ValidUserID,
                                        post = MusicId,
                                        author = Author,
                                        content = PlaceholderContent,
                                        date_created = Date,
                                        content_status = processing
                                    },
                                    mnesia:write(Comment),
                                    CurrentComments = case Music#music.comments of
                                        undefined -> [];
                                        L when is_list(L) -> L;
                                        _ -> []
                                    end,
                                    UpdatedComments = [Id | CurrentComments],
                                    mnesia:write(Music#music{comments = UpdatedComments}),
                                    case update_user_activity(Author, Date) of
                                        {error, Reason} ->
                                            error_logger:warning_msg("Failed to update activity for ~p: ~p", [Author, Reason]),
                                            ok;
                                        _ -> ok
                                    end,
                                    {ok, Id}
                            end
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
                                    {ok, #{id := _KeyID, name := _}} = ipfs_client_4:key_gen("music_comment_" ++ Id),
                                    PublishOptions = [
                                        {key, "music_comment_" ++ Id},
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

    get_music_comment_content(CommentID) ->
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

        update_music_comment(CommentID, NewContent) ->
            Fun = fun() ->
                case mnesia:read({comment, CommentID}) of
                    [] ->
                        {error, comment_not_found};
                    [Comment] ->
                        MusicId = Comment#comment.post,
                        case mnesia:read({music, MusicId}) of
                            [] ->
                                {error, invalid_comment_target};
                            [_Music] ->
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

        delete_music_comment(CommentID, MusicId) ->
            Fun = fun() ->
                case mnesia:read({music, MusicId}) of
                    [] ->
                        {error, music_not_found};
                    [Music] ->
                        case lists:member(CommentID, Music#music.comments) of
                            false ->
                                {error, comment_not_found};
                            true ->
                                UpdatedComments = lists:delete(CommentID, Music#music.comments),
                                mnesia:write(Music#music{comments = UpdatedComments}),

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

        react_to_music_comment(UserID, CommentId, ReactionType) ->
            case userdb:get_user_by_id(UserID) of
                user_not_exist ->
                    {error, user_not_found};
                error ->
                    {error, user_lookup_failed};
                _User ->
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
            case userdb:get_user_by_id(UserID) of
                user_not_exist ->
                    undefined;
                error ->
                    undefined;
                _User ->
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
                    end
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

        get_music_comments(MusicId) ->
            Fun = fun() ->
                case mnesia:read({music, MusicId}) of
                    [] -> [];
                    [Music] ->
                        CommentIds = Music#music.comments,
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

        get_comment_count(MusicId) ->
            Fun = fun() ->
                case mnesia:read({music, MusicId}) of
                    [] -> 0;
                    [Music] -> length(Music#music.comments)
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Count} -> Count;
                {aborted, _Reason} -> 0
            end.

        increment_play_count(MusicId, UserId) ->
            case userdb:get_user_by_id(UserId) of
                user_not_exist ->
                    {error, user_not_found};
                error ->
                    {error, user_lookup_failed};
                _User ->
                    Fun = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [] -> {error, music_not_found};
                            [Music] ->
                                mnesia:write(Music#music{plays = Music#music.plays + 1}),
                                ok
                        end
                    end,

                    case mnesia:transaction(Fun) of
                        {atomic, Result} -> Result;
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end
            end.

        increment_unique_listener(MusicId, UserId) when is_list(UserId) orelse is_binary(UserId) ->
            case userdb:get_user_by_id(UserId) of
                user_not_exist ->
                    {error, user_not_found};
                error ->
                    {error, user_lookup_failed};
                _User ->
                    UserIdStr = case is_binary(UserId) of
                        true -> binary_to_list(UserId);
                        false -> UserId
                    end,
                    Fun = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [] -> {error, music_not_found};
                            [Music] ->
                                CurrentListeners = case Music#music.data of
                                    #{unique_listener_ids := Listeners} -> Listeners;
                                    _ -> []
                                end,
                                case lists:member(UserIdStr, CurrentListeners) of
                                    true ->
                                        {error, already_counted};
                                    false ->
                                        NewListeners = [UserIdStr | CurrentListeners],
                                        NewCount = length(NewListeners),
                                        UpdatedData = maps:put(unique_listener_ids, NewListeners, Music#music.data),
                                        mnesia:write(Music#music{
                                            data = UpdatedData,
                                            unique_listeners = NewCount
                                        }),
                                        ok
                                end
                        end
                    end,
                    case mnesia:transaction(Fun) of
                        {atomic, Result} -> Result;
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end
            end.

        track_listen_time(MusicId, UserId, ListenSeconds) ->
            case userdb:get_user_by_id(UserId) of
                user_not_exist ->
                    {error, user_not_found};
                error ->
                    {error, user_lookup_failed};
                _User ->
                    Fun = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [] -> {error, music_not_found};
                            [Music] ->
                                TotalListenTime = Music#music.listen_time_total + ListenSeconds,
                                Duration = Music#music.duration_seconds,

                                CompletionRate = if
                                    Duration > 0 andalso Music#music.plays > 0 ->
                                        (TotalListenTime / (Duration * Music#music.plays)) * 100;
                                    true -> 0.0
                                end,

                                mnesia:write(Music#music{
                                    listen_time_total = TotalListenTime,
                                    completion_rate = CompletionRate
                                }),
                                ok
                        end
                    end,

                    case mnesia:transaction(Fun) of
                        {atomic, Result} -> Result;
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end
            end.

        get_listen_stats(MusicId) ->
            Fun = fun() ->
                case mnesia:read({music, MusicId}) of
                    [] -> {error, music_not_found};
                    [Music] ->
                        {ok, #{
                            plays => Music#music.plays,
                            unique_listeners => Music#music.unique_listeners,
                            listen_time_total => Music#music.listen_time_total,
                            completion_rate => Music#music.completion_rate
                        }}
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_unique_listener_count(MusicId) ->
            Fun = fun() ->
                case mnesia:read({music, MusicId}) of
                    [] -> {error, music_not_found};
                    [Music] -> {ok, Music#music.unique_listeners}
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        share_music(MusicId, UserId) ->
            share_music(MusicId, UserId, "").

        share_music(MusicId, UserId, Description) ->
            case userdb:get_user_by_id(UserId) of
                user_not_exist ->
                    {error, user_not_found};
                error ->
                    {error, user_lookup_failed};
                _User ->
                    Fun = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [] -> {error, music_not_found};
                            [Music] ->
                                mnesia:write(Music#music{shares = Music#music.shares + 1}),

                                MusicOwnerId = Music#music.user_id,
                                case mnesia:read({user, MusicOwnerId}) of
                                    [] ->
                                        {error, music_owner_not_found};
                                    [MusicOwner] ->
                                        MusicOwnerUsername = case MusicOwner#user.username of
                                            OwnerName when is_list(OwnerName) -> OwnerName;
                                            OwnerName when is_binary(OwnerName) -> binary_to_list(OwnerName);
                                            _ -> "Unknown"
                                        end,

                                        case mnesia:read({user, UserId}) of
                                            [] ->
                                                {error, user_not_found};
                                            [Sharer] ->
                                                SharerUsername = case Sharer#user.username of
                                                    SharerName when is_list(SharerName) -> SharerName;
                                                    SharerName when is_binary(SharerName) -> binary_to_list(SharerName);
                                                    _ -> "Unknown"
                                                end,

                                                MusicIdStr = if
                                                    is_list(MusicId) -> MusicId;
                                                    is_binary(MusicId) -> binary_to_list(MusicId);
                                                    true -> MusicId
                                                end,

                                                MusicTitle = if
                                                    is_binary(Music#music.title) -> Music#music.title;
                                                    is_list(Music#music.title) -> list_to_binary(Music#music.title);
                                                    true -> <<"Shared Music">>
                                                end,

                                                Artist = if
                                                    is_binary(Music#music.artist) -> Music#music.artist;
                                                    is_list(Music#music.artist) -> list_to_binary(Music#music.artist);
                                                    true -> <<"Unknown Artist">>
                                                end,

                                                MusicUrl = "https://mazaryn.io/en/music/" ++ MusicIdStr,

                                                Content = case Description of
                                                    "" ->
                                                        iolist_to_binary([
                                                            "MUSIC_SHARE:", MusicIdStr, "|", MusicUrl, "|", MusicTitle, "|",
                                                            Artist, "|", MusicOwnerUsername, "|", SharerUsername, "\n"
                                                        ]);
                                                    Desc when is_binary(Desc) andalso byte_size(Desc) > 0 ->
                                                        iolist_to_binary([
                                                            "MUSIC_SHARE:", MusicIdStr, "|", MusicUrl, "|", MusicTitle, "|",
                                                            Artist, "|", MusicOwnerUsername, "|", SharerUsername, "\n",
                                                            Desc
                                                        ]);
                                                    Desc when is_list(Desc) andalso length(Desc) > 0 ->
                                                        DescBin = list_to_binary(Desc),
                                                        iolist_to_binary([
                                                            "MUSIC_SHARE:", MusicIdStr, "|", MusicUrl, "|", MusicTitle, "|",
                                                            Artist, "|", MusicOwnerUsername, "|", SharerUsername, "\n",
                                                            DescBin
                                                        ]);
                                                    _ ->
                                                        iolist_to_binary([
                                                            "MUSIC_SHARE:", MusicIdStr, "|", MusicUrl, "|", MusicTitle, "|",
                                                            Artist, "|", MusicOwnerUsername, "|", SharerUsername, "\n"
                                                        ])
                                                end,

                                                AlbumArtUrl = case Music#music.album_art_url of
                                                    undefined -> undefined;
                                                    "" -> undefined;
                                                    Art when is_list(Art) -> Art;
                                                    Art when is_binary(Art) -> binary_to_list(Art);
                                                    _ -> undefined
                                                end,

                                                {ok, #{
                                                    username => SharerUsername,
                                                    content => binary_to_list(Content),
                                                    media => AlbumArtUrl,
                                                    music_id => MusicIdStr,
                                                    music_url => MusicUrl,
                                                    music_title => binary_to_list(MusicTitle),
                                                    artist => binary_to_list(Artist),
                                                    music_owner => MusicOwnerUsername,
                                                    hashtags => [<<"music">>, <<"shared">>],
                                                    emoji => [],
                                                    mention => []
                                                }}
                                        end
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
                    end
            end.

        save_music(MusicId, UserId) ->
            case userdb:get_user_by_id(UserId) of
                user_not_exist ->
                    {error, user_not_found};
                error ->
                    {error, user_lookup_failed};
                _User ->
                    Fun = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [] -> {error, music_not_found};
                            [Music] ->
                                mnesia:write(Music#music{saves = Music#music.saves + 1}),

                                case mnesia:read({user, UserId}) of
                                    [User] ->
                                        SavedPosts = User#user.saved_posts,
                                        mnesia:write(User#user{saved_posts = [MusicId | SavedPosts]});
                                    [] -> ok
                                end,

                                ok
                        end
                    end,

                    case mnesia:transaction(Fun) of
                        {atomic, Result} -> Result;
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end
            end.

        unsave_music(MusicId, UserId) ->
            case userdb:get_user_by_id(UserId) of
                user_not_exist ->
                    {error, user_not_found};
                error ->
                    {error, user_lookup_failed};
                _User ->
                    Fun = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [] -> {error, music_not_found};
                            [Music] ->
                                mnesia:write(Music#music{saves = max(0, Music#music.saves - 1)}),

                                case mnesia:read({user, UserId}) of
                                    [User] ->
                                        SavedPosts = User#user.saved_posts,
                                        UpdatedSavedPosts = lists:delete(MusicId, SavedPosts),
                                        mnesia:write(User#user{saved_posts = UpdatedSavedPosts});
                                    [] -> ok
                                end,

                                ok
                        end
                    end,

                    case mnesia:transaction(Fun) of
                        {atomic, Result} -> Result;
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end
            end.

            add_to_playlist(MusicId, PlaylistId) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            PlaylistIds = Music#music.playlist_ids,
                            UpdatedPlaylistIds = case lists:member(PlaylistId, PlaylistIds) of
                                true -> PlaylistIds;
                                false -> [PlaylistId | PlaylistIds]
                            end,

                            AddedToPlaylists = Music#music.added_to_playlists,

                            mnesia:write(Music#music{
                                playlist_ids = UpdatedPlaylistIds,
                                added_to_playlists = AddedToPlaylists + 1
                            }),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            remove_from_playlist(MusicId, PlaylistId) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            PlaylistIds = Music#music.playlist_ids,
                            UpdatedPlaylistIds = lists:delete(PlaylistId, PlaylistIds),

                            mnesia:write(Music#music{playlist_ids = UpdatedPlaylistIds}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            link_to_album(MusicId, AlbumId) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            mnesia:write(Music#music{album_id = AlbumId}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            set_monetization(MusicId, RevenueModel, Price) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            mnesia:write(Music#music{
                                monetized = true,
                                revenue_model = RevenueModel,
                                price_download = Price
                            }),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            set_streaming_royalties(MusicId, RoyaltyInfo) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            mnesia:write(Music#music{streaming_royalties = RoyaltyInfo}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            track_geographic_play(MusicId, Country, City) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            GeographicPlays = Music#music.geographic_plays,
                            Key = {Country, City},
                            CurrentCount = maps:get(Key, GeographicPlays, 0),
                            UpdatedGeographicPlays = maps:put(Key, CurrentCount + 1, GeographicPlays),

                            mnesia:write(Music#music{geographic_plays = UpdatedGeographicPlays}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            track_device_play(MusicId, DeviceType) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            DeviceBreakdown = Music#music.device_breakdown,
                            CurrentCount = maps:get(DeviceType, DeviceBreakdown, 0),
                            UpdatedDeviceBreakdown = maps:put(DeviceType, CurrentCount + 1, DeviceBreakdown),

                            mnesia:write(Music#music{device_breakdown = UpdatedDeviceBreakdown}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            track_referral(MusicId, Source) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            ReferralSources = Music#music.referral_sources,
                            CurrentCount = maps:get(Source, ReferralSources, 0),
                            UpdatedReferralSources = maps:put(Source, CurrentCount + 1, ReferralSources),

                            mnesia:write(Music#music{referral_sources = UpdatedReferralSources}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            add_to_radio_seed(MusicId, Weight) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            mnesia:write(Music#music{
                                radio_seed_weight = Weight,
                                radio_seed_enabled = true
                            }),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            calculate_discovery_score(MusicId) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            Plays = Music#music.plays,
                            Saves = Music#music.saves,
                            Shares = Music#music.shares,
                            PlaylistAdds = Music#music.added_to_playlists,

                            ReactionCounts = Music#music.reaction_counts,
                            TotalReactions = maps:fold(fun(_Type, Count, Acc) -> Count + Acc end, 0, ReactionCounts),

                            SkipRate = Music#music.skip_rate,
                            CompletionRate = Music#music.completion_rate,

                            Score = (Plays * 1.0 +
                                    Saves * 5.0 +
                                    Shares * 8.0 +
                                    PlaylistAdds * 10.0 +
                                    TotalReactions * 3.0 +
                                    CompletionRate * 2.0) -
                                    (SkipRate * 5.0),

                            mnesia:write(Music#music{discovery_score = Score}),
                            {ok, Score}
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            create_stem(MusicId, StemType, StemData, Quality) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            StemId = nanoid:gen(),
                            Now = calendar:universal_time(),

                            StemCID = ipfs_media:upload_media(StemData),
                            SizeBytes = calculate_content_size(StemData),

                            Stem = #audio_stem{
                                id = StemId,
                                music_id = MusicId,
                                stem_type = StemType,
                                ipfs_cid = StemCID,
                                quality = Quality,
                                file_size_bytes = SizeBytes,
                                date_created = Now
                            },
                            mnesia:write(Stem),

                            SeparatedStemsCids = Music#music.separated_stems_cids,
                            UpdatedStemsCids = maps:put(StemType, StemCID, SeparatedStemsCids),

                            mnesia:write(Music#music{separated_stems_cids = UpdatedStemsCids}),

                            {ok, StemId}
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            get_stems(MusicId) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] -> {ok, Music#music.separated_stems_cids}
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            enable_remix(MusicId) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            mnesia:write(Music#music{allow_remixes = true}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            disable_remix(MusicId) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            mnesia:write(Music#music{allow_remixes = false}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            create_remix(OriginalMusicId, RemixerId, RemixType, RemixData) ->
                case userdb:get_user_by_id(RemixerId) of
                    user_not_exist ->
                        {error, user_not_found};
                    error ->
                        {error, user_lookup_failed};
                    _User ->
                        Fun = fun() ->
                            case mnesia:read({music, OriginalMusicId}) of
                                [] -> {error, original_music_not_found};
                                [OriginalMusic] ->
                                    case OriginalMusic#music.allow_remixes of
                                        false -> {error, remixes_not_allowed};
                                        true ->
                                            RemixId = nanoid:gen(),
                                            Now = calendar:universal_time(),

                                            RemixProject = #remix_project{
                                                id = RemixId,
                                                original_track_id = OriginalMusicId,
                                                remixer_id = RemixerId,
                                                remix_type = RemixType,
                                                status = created,
                                                date_created = Now
                                            },
                                            mnesia:write(RemixProject),

                                            Remixes = OriginalMusic#music.remixes,
                                            UpdatedRemixes = [RemixId | Remixes],

                                            mnesia:write(OriginalMusic#music{remixes = UpdatedRemixes}),

                                            {ok, RemixId}
                                    end
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end
                end.

            add_collaborator(MusicId, CollaboratorId) ->
                case userdb:get_user_by_id(CollaboratorId) of
                    user_not_exist ->
                        {error, user_not_found};
                    error ->
                        {error, user_lookup_failed};
                    _User ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    Collaborators = Music#music.collaborators,
                                    UpdatedCollaborators = case lists:member(CollaboratorId, Collaborators) of
                                        true -> Collaborators;
                                        false -> [CollaboratorId | Collaborators]
                                    end,

                                    mnesia:write(Music#music{collaborators = UpdatedCollaborators}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end
                end.

            set_royalty_split(MusicId, RoyaltySplit) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            mnesia:write(Music#music{royalty_split_smart_contract = RoyaltySplit}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            pin_music(MusicId) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            IPFSCID = Music#music.ipfs_cid,
                            case is_cid_ready(IPFSCID) of
                                false -> {error, content_not_ready};
                                true ->
                                    spawn(fun() ->
                                        try
                                            ipfs_client_5:pin_add([{arg, IPFSCID}])
                                        catch
                                            _:Error ->
                                                error_logger:error_msg("Failed to pin music ~p: ~p", [MusicId, Error])
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

            unpin_music(MusicId) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            IPFSCID = Music#music.ipfs_cid,
                            case is_cid_ready(IPFSCID) of
                                false -> {error, content_not_ready};
                                true ->
                                    spawn(fun() ->
                                        try
                                            ipfs_client_5:pin_rm([{arg, IPFSCID}])
                                        catch
                                            _:Error ->
                                                error_logger:error_msg("Failed to unpin music ~p: ~p", [MusicId, Error])
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

            update_pin_status(MusicId, PinInfo) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            CurrentData = Music#music.data,
                            UpdatedData = maps:put(pin_info, PinInfo, CurrentData),
                            mnesia:write(Music#music{data = UpdatedData}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            report_music(ReporterId, MusicId, Type, Description) ->
                case userdb:get_user_by_id(ReporterId) of
                    user_not_exist ->
                        {error, user_not_found};
                    error ->
                        {error, user_lookup_failed};
                    _User ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    ReportId = nanoid:gen(),
                                    Now = calendar:universal_time(),

                                    Report = #report{
                                        id = ReportId,
                                        type = Type,
                                        description = Description,
                                        reporter = ReporterId,
                                        date_created = Now,
                                        data = #{music_id => MusicId}
                                    },
                                    mnesia:write(Report),

                                    UpdatedReports = [ReportId | Music#music.reported],
                                    mnesia:write(Music#music{reported = UpdatedReports}),

                                    {ok, ReportId}
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end
                end.

            set_age_restriction(MusicId, AgeRestriction) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            mnesia:write(Music#music{age_restriction = AgeRestriction}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            set_content_warnings(MusicId, Warnings) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            mnesia:write(Music#music{content_warnings = Warnings}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            search_music(Query) ->
                Fun = fun() ->
                    AllMusic = mnesia:match_object(#music{_ = '_'}),
                    QueryLower = string:to_lower(ensure_string(Query)),
                    lists:filter(fun(Music) ->
                        Title = ensure_string(Music#music.title),
                        TitleMatch = string:find(string:to_lower(Title), QueryLower) =/= nomatch,

                        Artist = ensure_string(Music#music.artist),
                        ArtistMatch = string:find(string:to_lower(Artist), QueryLower) =/= nomatch,

                        Album = ensure_string(Music#music.album),
                        AlbumMatch = string:find(string:to_lower(Album), QueryLower) =/= nomatch,

                        GenreMatch = lists:any(fun(Genre) ->
                            GenreStr = ensure_string(Genre),
                            string:find(string:to_lower(GenreStr), QueryLower) =/= nomatch
                        end, Music#music.genre),

                        TitleMatch orelse ArtistMatch orelse AlbumMatch orelse GenreMatch
                    end, AllMusic)
                end,

                {atomic, Res} = mnesia:transaction(Fun),
                Res.

            search_music_advanced(SearchParams) ->
                #{
                    query := Query,
                    genres := Genres,
                    moods := Moods,
                    min_plays := MinPlays,
                    privacy := Privacy
                } = SearchParams,

                Fun = fun() ->
                    AllMusic = mnesia:match_object(#music{_ = '_'}),
                    QueryLower = string:to_lower(ensure_string(Query)),

                    lists:filter(fun(Music) ->
                        QueryMatch = case Query of
                            "" -> true;
                            _ ->
                                Title = ensure_string(Music#music.title),
                                Artist = ensure_string(Music#music.artist),
                                TitleMatch = string:find(string:to_lower(Title), QueryLower) =/= nomatch,
                                ArtistMatch = string:find(string:to_lower(Artist), QueryLower) =/= nomatch,
                                TitleMatch orelse ArtistMatch
                        end,

                        GenreMatch = case Genres of
                            [] -> true;
                            _ -> lists:any(fun(Genre) ->
                                GenreStr = ensure_string(Genre),
                                lists:member(GenreStr, [ensure_string(G) || G <- Music#music.genre])
                            end, Genres)
                        end,

                        MoodMatch = case Moods of
                            [] -> true;
                            _ -> lists:any(fun(Mood) ->
                                MoodStr = ensure_string(Mood),
                                lists:member(MoodStr, [ensure_string(M) || M <- Music#music.mood])
                            end, Moods)
                        end,

                        PlaysMatch = Music#music.plays >= MinPlays,

                        PrivacyMatch = case Privacy of
                            any -> true;
                            _ -> Music#music.privacy =:= Privacy
                        end,

                        QueryMatch andalso GenreMatch andalso MoodMatch andalso PlaysMatch andalso PrivacyMatch
                    end, AllMusic)
                end,

                {atomic, Res} = mnesia:transaction(Fun),
                Res.

            get_trending_music(Limit) ->
                Fun = fun() ->
                    AllMusic = mnesia:match_object(#music{privacy = public, _ = '_'}),
                    Sorted = lists:sort(fun(A, B) ->
                        ScoreA = calculate_trending_score_value(A),
                        ScoreB = calculate_trending_score_value(B),
                        ScoreA > ScoreB
                    end, AllMusic),
                    lists:sublist(Sorted, Limit)
                end,

                {atomic, Res} = mnesia:transaction(Fun),
                Res.

            get_featured_music() ->
                Fun = fun() ->
                    AllMusic = mnesia:match_object(#music{privacy = public, _ = '_'}),
                    Sorted = lists:sort(fun(A, B) ->
                        A#music.discovery_score > B#music.discovery_score
                    end, AllMusic),
                    lists:sublist(Sorted, 20)
                end,

                {atomic, Res} = mnesia:transaction(Fun),
                Res.

            increment_download_count(MusicId) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            DownloadCount = Music#music.download_count,
                            mnesia:write(Music#music{download_count = DownloadCount + 1}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            increment_skip_count(MusicId) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            Plays = Music#music.plays,
                            SkipRate = if
                                Plays > 0 ->
                                    ((Music#music.skip_rate * Plays) + 1.0) / (Plays + 1);
                                true -> 1.0
                            end,
                            mnesia:write(Music#music{skip_rate = SkipRate}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            increment_replay_count(MusicId) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            ReplayCount = Music#music.replay_count,
                            mnesia:write(Music#music{replay_count = ReplayCount + 1}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            add_to_listening_hours(MusicId, Hour) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            ListeningHours = Music#music.listening_hours,
                            UpdatedHours = [Hour | ListeningHours],
                            mnesia:write(Music#music{listening_hours = UpdatedHours}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            track_completion_rate(MusicId, CompletionPercentage) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            Plays = Music#music.plays,
                            CurrentRate = Music#music.completion_rate,

                            NewRate = if
                                Plays > 0 ->
                                    ((CurrentRate * Plays) + CompletionPercentage) / (Plays + 1);
                                true -> CompletionPercentage
                            end,

                            mnesia:write(Music#music{completion_rate = NewRate}),
                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            calculate_trending_score(MusicId) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            Score = calculate_trending_score_value(Music),
                            mnesia:write(Music#music{discovery_score = Score}),
                            {ok, Score}
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

            calculate_trending_score_value(Music) ->
                Plays = Music#music.plays,
                Shares = Music#music.shares,
                Saves = Music#music.saves,
                PlaylistAdds = Music#music.added_to_playlists,

                ReactionCounts = Music#music.reaction_counts,
                TotalReactions = maps:fold(fun(_Type, Count, Acc) -> Count + Acc end, 0, ReactionCounts),

                Comments = length(Music#music.comments),

                Now = calendar:universal_time(),
                Created = Music#music.date_created,
                HoursOld = calendar:datetime_to_gregorian_seconds(Now) -
                           calendar:datetime_to_gregorian_seconds(Created),
                HoursOldFloat = HoursOld / 3600,

                TimeDecay = math:pow(HoursOldFloat + 2, 1.5),

                (Plays * 1.0 +
                 TotalReactions * 5.0 +
                 Comments * 3.0 +
                 Shares * 10.0 +
                 Saves * 7.0 +
                 PlaylistAdds * 12.0) / TimeDecay.

            update_analytics(MusicId) ->
                Fun = fun() ->
                    case mnesia:read({music, MusicId}) of
                        [] -> {error, music_not_found};
                        [Music] ->
                            Now = calendar:universal_time(),

                            Analytics = #media_analytics{
                                media_id = MusicId,
                                media_type = music,
                                date = Now,
                                views = Music#music.plays,
                                unique_viewers = Music#music.unique_listeners,
                                watch_time = Music#music.listen_time_total,
                                avg_watch_percentage = Music#music.completion_rate,
                                likes = maps:fold(fun(_Type, Count, Acc) -> Count + Acc end, 0, Music#music.reaction_counts),
                                comments = length(Music#music.comments),
                                shares = Music#music.shares,
                                geographic_breakdown = Music#music.geographic_plays,
                                device_breakdown = Music#music.device_breakdown
                            },
                            mnesia:write(Analytics),

                            ok
                    end
                end,

                case mnesia:transaction(Fun) of
                    {atomic, Result} -> Result;
                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                end.

                add_timed_comment(MusicId, UserId, CommentText, TimestampMs) ->
                    case userdb:get_user_by_id(UserId) of
                        user_not_exist ->
                            {error, user_not_found};
                        error ->
                            {error, user_lookup_failed};
                        _User ->
                            Fun = fun() ->
                                case mnesia:read({music, MusicId}) of
                                    [] -> {error, music_not_found};
                                    [Music] ->
                                        CommentId = nanoid:gen(),
                                        Now = calendar:universal_time(),

                                        TimedComment = #timed_comment{
                                            id = CommentId,
                                            music_id = MusicId,
                                            user_id = UserId,
                                            comment_text = CommentText,
                                            timestamp_ms = TimestampMs,
                                            date_created = Now
                                        },
                                        mnesia:write(TimedComment),

                                        CurrentData = Music#music.data,
                                        TimedComments = maps:get(timed_comments, CurrentData, []),
                                        UpdatedTimedComments = [CommentId | TimedComments],
                                        UpdatedData = maps:put(timed_comments, UpdatedTimedComments, CurrentData),

                                        mnesia:write(Music#music{data = UpdatedData}),

                                        {ok, CommentId}
                                end
                            end,

                            case mnesia:transaction(Fun) of
                                {atomic, Result} -> Result;
                                {aborted, Reason} -> {error, {transaction_failed, Reason}}
                            end
                    end.

                get_timed_comments(MusicId) ->
                    Fun = fun() ->
                        AllTimedComments = mnesia:match_object(#timed_comment{music_id = MusicId, _ = '_'}),
                        lists:sort(fun(A, B) ->
                            A#timed_comment.timestamp_ms =< B#timed_comment.timestamp_ms
                        end, AllTimedComments)
                    end,

                    {atomic, Res} = mnesia:transaction(Fun),
                    Res.

                get_comments_at_timestamp(MusicId, TimestampMs) ->
                    Fun = fun() ->
                        AllTimedComments = mnesia:match_object(#timed_comment{music_id = MusicId, _ = '_'}),
                        Range = 5000,
                        lists:filter(fun(Comment) ->
                            Ts = Comment#timed_comment.timestamp_ms,
                            abs(Ts - TimestampMs) =< Range
                        end, AllTimedComments)
                    end,

                    {atomic, Res} = mnesia:transaction(Fun),
                    Res.

                submit_to_playlist(MusicId, PlaylistId) ->
                    Fun = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [] -> {error, music_not_found};
                            [Music] ->
                                IncludedIn = Music#music.included_in_playlists,
                                UpdatedIncludedIn = case lists:member(PlaylistId, IncludedIn) of
                                    true -> IncludedIn;
                                    false -> [PlaylistId | IncludedIn]
                                end,

                                PlaylistReach = Music#music.playlist_reach + 1,

                                mnesia:write(Music#music{
                                    included_in_playlists = UpdatedIncludedIn,
                                    playlist_reach = PlaylistReach
                                }),
                                ok
                        end
                    end,

                    case mnesia:transaction(Fun) of
                        {atomic, Result} -> Result;
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end.

                feature_on_editorial(MusicId, EditorialPlaylistId) ->
                    Fun = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [] -> {error, music_not_found};
                            [Music] ->
                                EditorialPicks = Music#music.editorial_playlist_picks,
                                UpdatedEditorialPicks = case lists:member(EditorialPlaylistId, EditorialPicks) of
                                    true -> EditorialPicks;
                                    false -> [EditorialPlaylistId | EditorialPicks]
                                end,

                                mnesia:write(Music#music{editorial_playlist_picks = UpdatedEditorialPicks}),
                                ok
                        end
                    end,

                    case mnesia:transaction(Fun) of
                        {atomic, Result} -> Result;
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end.

                create_spatial_audio(MusicId, Format, AudioData) ->
                    Fun = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [] -> {error, music_not_found};
                            [Music] ->
                                SpatialId = nanoid:gen(),
                                Now = calendar:universal_time(),

                                SpatialCID = ipfs_media:upload_media(AudioData),

                                SpatialAudio = #spatial_audio_metadata{
                                    id = SpatialId,
                                    music_id = MusicId,
                                    format = Format,
                                    date_created = Now
                                },
                                mnesia:write(SpatialAudio),

                                mnesia:write(Music#music{spatial_audio_cid = SpatialCID}),

                                {ok, SpatialId}
                        end
                    end,

                    case mnesia:transaction(Fun) of
                        {atomic, Result} -> Result;
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end.

                add_separated_stem(MusicId, StemType, StemCID) ->
                    Fun = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [] -> {error, music_not_found};
                            [Music] ->
                                SeparatedStems = Music#music.separated_stems_cids,
                                UpdatedStems = maps:put(StemType, StemCID, SeparatedStems),

                                mnesia:write(Music#music{separated_stems_cids = UpdatedStems}),
                                ok
                        end
                    end,

                    case mnesia:transaction(Fun) of
                        {atomic, Result} -> Result;
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end.

                set_pre_release(MusicId, PreReleaseUntil) ->
                    Fun = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [] -> {error, music_not_found};
                            [Music] ->
                                mnesia:write(Music#music{
                                    pre_release = true,
                                    early_access_until = PreReleaseUntil
                                }),
                                ok
                        end
                    end,

                    case mnesia:transaction(Fun) of
                        {atomic, Result} -> Result;
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end.

                schedule_premiere(MusicId, PremiereTimestamp) ->
                    Fun = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [] -> {error, music_not_found};
                            [Music] ->
                                mnesia:write(Music#music{premiere_timestamp = PremiereTimestamp}),
                                ok
                        end
                    end,

                    case mnesia:transaction(Fun) of
                        {atomic, Result} -> Result;
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end.

                    add_listener_journey(MusicId, UserId, DiscoverySource) ->
                        case userdb:get_user_by_id(UserId) of
                            user_not_exist ->
                                {error, user_not_found};
                            error ->
                                {error, user_lookup_failed};
                            _User ->
                                Fun = fun() ->
                                    case mnesia:read({music, MusicId}) of
                                        [] -> {error, music_not_found};
                                        [_Music] ->
                                            JourneyId = nanoid:gen(),
                                            Now = calendar:universal_time(),

                                            Journey = #listener_journey{
                                                user_id = UserId,
                                                music_id = MusicId,
                                                discovery_source = DiscoverySource,
                                                discovery_timestamp = Now,
                                                total_plays = 0,
                                                skip_points = [],
                                                replay_sections = []
                                            },
                                            mnesia:write(Journey),

                                            {ok, JourneyId}
                                    end
                                end,

                                case mnesia:transaction(Fun) of
                                    {atomic, Result} -> Result;
                                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                                end
                        end.

                track_skip_point(MusicId, UserId, SkipTimestamp) ->
                    case userdb:get_user_by_id(UserId) of
                        user_not_exist ->
                            {error, user_not_found};
                        error ->
                            {error, user_lookup_failed};
                        _User ->
                            Fun = fun() ->
                                JourneyQuery = qlc:q([J || J <- mnesia:table(listener_journey),
                                                           J#listener_journey.music_id =:= MusicId,
                                                           J#listener_journey.user_id =:= UserId]),
                                case qlc:e(JourneyQuery) of
                                    [Journey] ->
                                        SkipPoints = Journey#listener_journey.skip_points,
                                        UpdatedSkipPoints = [SkipTimestamp | SkipPoints],
                                        mnesia:write(Journey#listener_journey{skip_points = UpdatedSkipPoints}),
                                        ok;
                                    [] ->
                                        {error, journey_not_found};
                                    _ ->
                                        {error, multiple_journeys_found}
                                end
                            end,

                            case mnesia:transaction(Fun) of
                                {atomic, Result} -> Result;
                                {aborted, Reason} -> {error, {transaction_failed, Reason}}
                            end
                    end.

                track_replay_section(MusicId, UserId, StartTimestamp, EndTimestamp) ->
                    case userdb:get_user_by_id(UserId) of
                        user_not_exist ->
                            {error, user_not_found};
                        error ->
                            {error, user_lookup_failed};
                        _User ->
                            Fun = fun() ->
                                JourneyQuery = qlc:q([J || J <- mnesia:table(listener_journey),
                                                           J#listener_journey.music_id =:= MusicId,
                                                           J#listener_journey.user_id =:= UserId]),
                                case qlc:e(JourneyQuery) of
                                    [Journey] ->
                                        ReplaySections = Journey#listener_journey.replay_sections,
                                        ReplayEntry = {StartTimestamp, EndTimestamp, calendar:universal_time()},
                                        UpdatedReplaySections = [ReplayEntry | ReplaySections],
                                        mnesia:write(Journey#listener_journey{replay_sections = UpdatedReplaySections}),
                                        ok;
                                    [] ->
                                        {error, journey_not_found};
                                    _ ->
                                        {error, multiple_journeys_found}
                                end
                            end,

                            case mnesia:transaction(Fun) of
                                {atomic, Result} -> Result;
                                {aborted, Reason} -> {error, {transaction_failed, Reason}}
                            end
                    end.

                create_music_with_rust(CreatorId, Title, Artist, Album, MusicFileOrPath, Duration, Genre, Privacy, AllowComments, AllowDownloads, Monetized) ->
                    case userdb:get_user_by_id(CreatorId) of
                        user_not_exist ->
                            error_logger:warning_msg("User ~p not found when creating music with rust", [CreatorId]),
                            {error, user_not_found};
                        error ->
                            {error, user_lookup_failed};
                        _User ->
                            Fun = fun() ->
                                Id = nanoid:gen(),
                                Now = calendar:universal_time(),
                                AI_Music_ID = ai_musicdb:insert_in_transaction(Id),

                                {SizeBytes, IsFilePath} = case MusicFileOrPath of
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
                                        ok = content_cache:set({music_file_path, Id}, MusicFileOrPath);
                                    true ->
                                        ok = content_cache:set({music_file, Id}, MusicFileOrPath)
                                end,

                                Music = #music{
                                    id = Id,
                                    ai_music_id = AI_Music_ID,
                                    user_id = CreatorId,
                                    file_url = {pending, Id},
                                    ipfs_cid = {pending, Id},
                                    status = processing,
                                    title = Title,
                                    artist = Artist,
                                    album = Album,
                                    duration_seconds = Duration,
                                    genre = Genre,
                                    privacy = Privacy,
                                    allow_comments = AllowComments,
                                    allow_downloads = AllowDownloads,
                                    monetized = Monetized,
                                    file_size_bytes = SizeBytes,
                                    date_created = Now,
                                    date_updated = Now,
                                    plays = 0,
                                    unique_listeners = 0,
                                    likes = [],
                                    reactions = #{
                                        love => [],
                                        fire => [],
                                        sad => [],
                                        party => [],
                                        vibe => []
                                    },
                                    reaction_counts = #{
                                        love => 0,
                                        fire => 0,
                                        sad => 0,
                                        party => 0,
                                        vibe => 0
                                    },
                                    comments = [],
                                    shares = 0,
                                    saves = 0,
                                    data = #{is_file_path => IsFilePath}
                                },

                                mnesia:write(Music),

                                case mnesia:read({user, CreatorId}) of
                                    [User] ->
                                        CurrentMedia = case User#user.media of
                                            undefined -> [Id];
                                            List when is_list(List) -> [Id | List];
                                            _ -> [Id]
                                        end,
                                        mnesia:write(User#user{media = CurrentMedia});
                                    [] ->
                                        ok
                                end,

                                {ok, Id}
                            end,

                            case mnesia:transaction(Fun) of
                                {atomic, {ok, Id}} ->
                                    spawn(fun() ->
                                        upload_music_to_ipfs_simple(Id, MusicFileOrPath)
                                    end),
                                    Id;
                                {atomic, {error, Reason}} ->
                                    {error, Reason};
                                {aborted, Reason} ->
                                    {error, {transaction_failed, Reason}}
                            end
                    end.

                upload_music_to_ipfs_simple(MusicId, MusicFileOrPath) ->
                    try
                        {InputType, Size, ActualPath} = case MusicFileOrPath of
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

                        error_logger:info_msg("Starting IPFS upload for music ~p, type: ~p, size: ~p bytes",
                                              [MusicId, InputType, Size]),

                        MusicCID = case InputType of
                            file_path ->
                                error_logger:info_msg("Uploading music from file path: ~p", [ActualPath]),
                                case ipfs_audio:upload_audio(ActualPath) of
                                    {error, Reason} ->
                                        error_logger:error_msg("IPFS upload failed: ~p", [Reason]),
                                        error;
                                    CID when is_list(CID) ->
                                        error_logger:info_msg("Successfully uploaded music to IPFS: ~p", [CID]),
                                        CID;
                                    CID when is_binary(CID) ->
                                        error_logger:info_msg("Successfully uploaded music to IPFS: ~p", [CID]),
                                        binary_to_list(CID);
                                    Other ->
                                        error_logger:error_msg("Unexpected IPFS upload result: ~p", [Other]),
                                        error
                                end;
                            binary ->
                                if Size > ?CHUNK_SIZE ->
                                    error_logger:info_msg("Large music binary detected, using chunked upload"),
                                    upload_large_file_chunked(MusicFileOrPath);
                                true ->
                                    error_logger:info_msg("Uploading music binary to IPFS"),
                                    case ipfs_audio:upload_audio(MusicFileOrPath) of
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

                        case MusicCID of
                            error ->
                                error_logger:error_msg("Failed to upload music content to IPFS for music ~p", [MusicId]),
                                UpdateF = fun() ->
                                    case mnesia:read({music, MusicId}) of
                                        [Music] ->
                                            mnesia:write(Music#music{
                                                ipfs_cid = {error, ipfs_upload_failed},
                                                file_url = {error, ipfs_upload_failed},
                                                status = failed
                                            });
                                        [] -> ok
                                    end
                                end,
                                mnesia:transaction(UpdateF);
                            ValidCID ->
                                error_logger:info_msg("Music content uploaded successfully: ~p", [ValidCID]),

                                UpdateF = fun() ->
                                    case mnesia:read({music, MusicId}) of
                                        [Music] ->
                                            UpdatedMusic = Music#music{
                                                ipfs_cid = ValidCID,
                                                file_url = ValidCID,
                                                status = ready
                                            },
                                            mnesia:write(UpdatedMusic);
                                        [] -> ok
                                    end
                                end,
                                mnesia:transaction(UpdateF),

                                content_cache:delete({music_file, MusicId}),
                                content_cache:delete({music_file_path, MusicId}),

                                case InputType of
                                    file_path when is_list(ActualPath) ->
                                        spawn(fun() ->
                                            timer:sleep(30000),
                                            try
                                                case filelib:is_file(ActualPath) of
                                                    true ->
                                                        file:delete(ActualPath),
                                                        error_logger:info_msg("Cleaned up temp music file: ~p", [ActualPath]);
                                                    false ->
                                                        ok
                                                end
                                            catch
                                                Exception:Error ->
                                                    error_logger:warning_msg("Failed to cleanup temp music ~p: ~p:~p",
                                                                           [ActualPath, Exception, Error])
                                            end
                                        end);
                                    _ ->
                                        ok
                                end,

                                spawn(fun() ->
                                    timer:sleep(10000),
                                    try
                                        KeyName = "music_" ++ MusicId,
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
                                                update_music_ipns(MusicId, IPNSKey);
                                            {error, IPNSReason} ->
                                                error_logger:error_msg("IPNS publish failed for music ~p: ~p", [MusicId, IPNSReason])
                                        end
                                    catch
                                        Exception:Error:Stacktrace ->
                                            error_logger:error_msg(
                                                "Exception while publishing to IPNS for music ~p: ~p:~p~n~p",
                                                [MusicId, Exception, Error, Stacktrace]
                                            )
                                    end
                                end),

                                ok
                        end
                    catch
                        Exception:Error:Stacktrace ->
                            error_logger:error_msg(
                                "Exception while uploading music ~p to IPFS: ~p:~p~n~p",
                                [MusicId, Exception, Error, Stacktrace]
                            ),
                            UpdateStatusF = fun() ->
                                case mnesia:read({music, MusicId}) of
                                    [Music] ->
                                        mnesia:write(Music#music{
                                            status = failed,
                                            ipfs_cid = {error, ipfs_upload_failed}
                                        });
                                    [] -> ok
                                end
                            end,
                            mnesia:transaction(UpdateStatusF)
                    end.

                upload_music_update(MusicId, MusicFile) ->
                    upload_music_to_ipfs_simple(MusicId, MusicFile).

                upload_music_version(MusicId, MusicFile) ->
                    try
                        MusicCID = ipfs_media:upload_media(MusicFile),

                        UpdateF = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [Music] ->
                                    CurrentData = Music#music.data,
                                    VersionHistory = maps:get(version_history, CurrentData, []),

                                    [{VersionNum, _PendingCID, Timestamp, Description} | RestVersions] = VersionHistory,
                                    UpdatedVersionEntry = {VersionNum, MusicCID, Timestamp, Description},
                                    UpdatedVersionHistory = [UpdatedVersionEntry | RestVersions],

                                    UpdatedData = maps:put(version_history, UpdatedVersionHistory, CurrentData),

                                    UpdatedMusic = Music#music{
                                        ipfs_cid = MusicCID,
                                        file_url = MusicCID,
                                        status = ready,
                                        data = UpdatedData
                                    },
                                    mnesia:write(UpdatedMusic);
                                [] -> ok
                            end
                        end,
                        mnesia:transaction(UpdateF),

                        content_cache:delete({music_version, MusicId}),

                        ok
                    catch
                        Exception:Error:Stacktrace ->
                            error_logger:error_msg(
                                "Exception while uploading music version ~p: ~p:~p~n~p",
                                [MusicId, Exception, Error, Stacktrace]
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

                track_music_playback(MusicId, UserId, Quality) ->
                    case userdb:get_user_by_id(UserId) of
                        user_not_exist ->
                            {error, user_not_found};
                        error ->
                            {error, user_lookup_failed};
                        _User ->
                            case media_audio_client:create_session(MusicId, UserId, Quality) of
                                {ok, SessionId, _, _} ->
                                    increment_play_count(MusicId, UserId),
                                    {ok, SessionId};
                                {error, Reason} ->
                                    {error, Reason}
                            end
                    end.

                update_playback_position(SessionId, Position) ->
                    media_audio_client:update_position(SessionId, Position).

                get_music_analytics(MusicId) ->
                    case media_audio_client:get_audio_metrics(MusicId) of
                        {ok, Metrics} ->
                            case get_listen_stats(MusicId) of
                                {ok, MnesiaStats} ->
                                    {ok, maps:merge(Metrics, MnesiaStats)};
                                _ ->
                                    {ok, Metrics}
                            end;
                        Error ->
                            Error
                    end.

                export_music_analytics(MusicId) ->
                    media_audio_client:get_analytics_report(MusicId).

                detect_music_format(FilePath) when is_binary(FilePath) ->
                    detect_music_format(binary_to_list(FilePath));
                detect_music_format(FilePath) when is_list(FilePath) ->
                    Extension = string:to_lower(filename:extension(FilePath)),
                    case Extension of
                        ".mp3" -> mp3;
                        ".wav" -> wav;
                        ".flac" -> flac;
                        ".aac" -> aac;
                        ".ogg" -> ogg;
                        ".m4a" -> m4a;
                        ".wma" -> wma;
                        ".opus" -> opus;
                        ".alac" -> alac;
                        ".aiff" -> aiff;
                        _ -> unknown
                    end.

                get_music_info_from_rust(FilePath) ->
                    media_audio_client:get_audio_info(FilePath).

                get_music_cid(MusicId) ->
                    Fun = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [] -> {error, music_not_found};
                            [Music] -> {ok, Music#music.ipfs_cid}
                        end
                    end,

                    case mnesia:transaction(Fun) of
                        {atomic, {ok, CID}} -> CID;
                        {atomic, {error, Reason}} -> {error, Reason};
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end.

                update_music_ipns(MusicId, IPNSKey) ->
                    UpdateF = fun() ->
                        case mnesia:read({music, MusicId}) of
                            [Music] ->
                                CurrentData = Music#music.data,
                                UpdatedData = maps:put(ipns, IPNSKey, CurrentData),
                                mnesia:write(Music#music{data = UpdatedData}),
                                ok;
                            [] ->
                                {error, not_found}
                        end
                    end,

                    case mnesia:transaction(UpdateF) of
                        {atomic, ok} ->
                            ok;
                        {atomic, {error, Reason}} ->
                            error_logger:error_msg("Failed to update music ~p with IPNS: ~p", [MusicId, Reason]),
                            {error, Reason};
                        {aborted, Reason} ->
                            error_logger:error_msg("Transaction aborted while updating music ~p with IPNS: ~p", [MusicId, Reason]),
                            {error, {transaction_failed, Reason}}
                    end.

                get_music(MusicId) ->
                    case mnesia:transaction(fun() ->
                        mnesia:read(music, MusicId)
                    end) of
                        {atomic, [Music]} -> {ok, Music};
                        {atomic, []} -> {error, not_found};
                        {aborted, Reason} -> {error, Reason}
                    end.

                    validate_music_file(FilePath) ->
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

                    write_music_with_retry(Music, UserId, RetriesLeft) when RetriesLeft > 0 ->
                        Fun = fun() ->
                            mnesia:write(Music),

                            case mnesia:read({user, UserId}) of
                                [User] ->
                                    UserMedia = User#user.media,
                                    UpdatedMedia = case UserMedia of
                                        undefined -> [Music#music.id];
                                        List when is_list(List) -> [Music#music.id | List];
                                        _ -> [Music#music.id]
                                    end,
                                    mnesia:write(User#user{media = UpdatedMedia});
                                [] ->
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, _} ->
                                ok;
                            {aborted, Reason} ->
                                error_logger:warning_msg("Music write failed (retries left: ~p): ~p",
                                                       [RetriesLeft, Reason]),
                                timer:sleep(?BACKOFF_TIME * ((?MAX_RETRIES - RetriesLeft) + 1)),
                                write_music_with_retry(Music, UserId, RetriesLeft - 1)
                        end;
                    write_music_with_retry(_Music, _UserId, 0) ->
                        {error, max_retries_exceeded}.

                    ensure_string(Value) when is_binary(Value) -> binary_to_list(Value);
                    ensure_string(Value) when is_list(Value) -> Value;
                    ensure_string(Value) when is_atom(Value) -> atom_to_list(Value);
                    ensure_string(_) -> "".

                    upload_music_to_ipfs_with_rust(MusicId, MusicFile) when is_binary(MusicFile) ->
                        try
                            Size = byte_size(MusicFile),

                            MusicCID = if
                                Size > ?CHUNK_SIZE ->
                                    upload_large_file_chunked(MusicFile);
                                true ->
                                    ipfs_media:upload_media(MusicFile)
                            end,

                            TempDir = "/tmp",
                            filelib:ensure_dir(TempDir ++ "/"),
                            TempFilePath = TempDir ++ "/mazaryn_music_" ++ MusicId ++ ".mp3",

                            case file:write_file(TempFilePath, MusicFile) of
                                ok ->
                                    MusicInfo = case media_audio_client:get_audio_info(TempFilePath) of
                                        {ok, Info} -> Info;
                                        {error, InfoReason} ->
                                            error_logger:warning_msg("Failed to get music info: ~p", [InfoReason]),
                                            #{}
                                    end,

                                    file:delete(TempFilePath),

                                    Duration = maps:get(duration, MusicInfo, 0.0),
                                    Format = maps:get(format, MusicInfo, "unknown"),
                                    Bitrate = maps:get(bitrate, MusicInfo, 0),
                                    SampleRate = maps:get(sample_rate, MusicInfo, 0),
                                    Channels = maps:get(channels, MusicInfo, 0),

                                    UpdateF = fun() ->
                                        case mnesia:read({music, MusicId}) of
                                            [Music] ->
                                                UpdatedMusic = Music#music{
                                                    ipfs_cid = MusicCID,
                                                    file_url = MusicCID,
                                                    status = ready,
                                                    duration_seconds = Duration,
                                                    format = Format,
                                                    bitrate = Bitrate,
                                                    sample_rate = SampleRate,
                                                    channels = Channels
                                                },
                                                mnesia:write(UpdatedMusic);
                                            [] -> ok
                                        end
                                    end,
                                    mnesia:transaction(UpdateF),

                                    content_cache:delete({music_file, MusicId}),

                                    spawn(fun() ->
                                        timer:sleep(20000),
                                        try
                                            KeyName = "music_" ++ MusicId,
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

                                            case ipfs_client_5:name_publish("/ipfs/" ++ MusicCID, PublishOptions) of
                                                {ok, #{name := IPNSKey}} ->
                                                    update_music_ipns(MusicId, IPNSKey);
                                                {error, IPNSReason} ->
                                                    error_logger:error_msg("IPNS publish failed for music ~p: ~p", [MusicId, IPNSReason])
                                            end
                                        catch
                                            IPNSException:IPNSError:IPNSStacktrace ->
                                                error_logger:error_msg(
                                                    "Exception while publishing to IPNS for music ~p: ~p:~p~n~p",
                                                    [MusicId, IPNSException, IPNSError, IPNSStacktrace]
                                                )
                                        end
                                    end),

                                    ok;
                                {error, WriteReason} ->
                                    error_logger:error_msg("Failed to write temp file for music ~p: ~p", [MusicId, WriteReason]),

                                    UpdateF = fun() ->
                                        case mnesia:read({music, MusicId}) of
                                            [Music] ->
                                                UpdatedMusic = Music#music{
                                                    ipfs_cid = MusicCID,
                                                    file_url = MusicCID,
                                                    status = ready
                                                },
                                                mnesia:write(UpdatedMusic);
                                            [] -> ok
                                        end
                                    end,
                                    mnesia:transaction(UpdateF),
                                    content_cache:delete({music_file, MusicId})
                            end
                        catch
                            Exception:Error:Stacktrace ->
                                error_logger:error_msg(
                                    "Exception while uploading music ~p to IPFS: ~p:~p~n~p",
                                    [MusicId, Exception, Error, Stacktrace]
                                ),

                                UpdateStatusF = fun() ->
                                    case mnesia:read({music, MusicId}) of
                                        [Music] ->
                                            mnesia:write(Music#music{status = failed});
                                        [] -> ok
                                    end
                                end,
                                mnesia:transaction(UpdateStatusF)
                        end.

                    get_music_by_isrc(ISRC) ->
                        Fun = fun() ->
                            mnesia:match_object(#music{isrc = ISRC, _ = '_'})
                        end,
                        {atomic, Res} = mnesia:transaction(Fun),
                        Res.

                    get_music_by_upc(UPC) ->
                        Fun = fun() ->
                            mnesia:match_object(#music{upc = UPC, _ = '_'})
                        end,
                        {atomic, Res} = mnesia:transaction(Fun),
                        Res.

                    set_explicit_content(MusicId, HasExplicitContent) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{has_explicit_content = HasExplicitContent}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    add_featured_artist(MusicId, ArtistId) ->
                        case userdb:get_user_by_id(ArtistId) of
                            user_not_exist ->
                                {error, user_not_found};
                            error ->
                                {error, user_lookup_failed};
                            _User ->
                                Fun = fun() ->
                                    case mnesia:read({music, MusicId}) of
                                        [] -> {error, music_not_found};
                                        [Music] ->
                                            FeaturedArtists = Music#music.featured_artists,
                                            UpdatedFeaturedArtists = case lists:member(ArtistId, FeaturedArtists) of
                                                true -> FeaturedArtists;
                                                false -> [ArtistId | FeaturedArtists]
                                            end,
                                            mnesia:write(Music#music{featured_artists = UpdatedFeaturedArtists}),
                                            ok
                                    end
                                end,

                                case mnesia:transaction(Fun) of
                                    {atomic, Result} -> Result;
                                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                                end
                        end.

                    remove_featured_artist(MusicId, ArtistId) ->
                        case userdb:get_user_by_id(ArtistId) of
                            user_not_exist ->
                                {error, user_not_found};
                            error ->
                                {error, user_lookup_failed};
                            _User ->
                                Fun = fun() ->
                                    case mnesia:read({music, MusicId}) of
                                        [] -> {error, music_not_found};
                                        [Music] ->
                                            FeaturedArtists = Music#music.featured_artists,
                                            UpdatedFeaturedArtists = lists:delete(ArtistId, FeaturedArtists),
                                            mnesia:write(Music#music{featured_artists = UpdatedFeaturedArtists}),
                                            ok
                                    end
                                end,

                                case mnesia:transaction(Fun) of
                                    {atomic, Result} -> Result;
                                    {aborted, Reason} -> {error, {transaction_failed, Reason}}
                                end
                        end.

                    set_audio_quality_tier(MusicId, QualityTier) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{audio_quality_tier = QualityTier}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    enable_lossless(MusicId, LosslessCID) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{
                                        lossless_available = true,
                                        lossless_cid = LosslessCID
                                    }),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    enable_hi_res(MusicId, HiResCID) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{
                                        hi_res_lossless = true,
                                        hi_res_cid = HiResCID
                                    }),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    enable_dolby_atmos(MusicId, DolbyAtmosCID) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{
                                        dolby_atmos_available = true,
                                        dolby_atmos_cid = DolbyAtmosCID
                                    }),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    set_apple_digital_master(MusicId, IsAppleDigitalMaster) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{apple_digital_master = IsAppleDigitalMaster}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    add_time_synced_lyrics(MusicId, TimeSyncedLyrics) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{time_synced_lyrics = TimeSyncedLyrics}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    add_lyrics_translation(MusicId, Language, TranslatedLyrics) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    LyricsTranslation = Music#music.lyrics_translation,
                                    UpdatedTranslation = maps:put(Language, TranslatedLyrics, LyricsTranslation),
                                    mnesia:write(Music#music{lyrics_translation = UpdatedTranslation}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    link_music_video(MusicId, VideoId) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{
                                        music_video_id = VideoId,
                                        video_available = true
                                    }),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    track_shazam(MusicId) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    ShazamCount = Music#music.shazam_count,
                                    mnesia:write(Music#music{shazam_count = ShazamCount + 1}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    set_shazam_trending(MusicId, IsTrending) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{shazam_trending = IsTrending}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    enable_gapless_playback(MusicId) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{gapless_playback = true}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    set_crossfade_duration(MusicId, DurationSeconds) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{crossfade_duration = DurationSeconds}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    generate_ai_extended_version(MusicId, ExtendedVersionCID) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{ai_extended_version_cid = ExtendedVersionCID}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    generate_ai_radio_edit(MusicId, RadioEditCID) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{ai_radio_edit_cid = RadioEditCID}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    generate_ai_instrumental(MusicId, InstrumentalCID) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{ai_instrumental_cid = InstrumentalCID}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    generate_ai_acapella(MusicId, AcapellaCID) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{ai_acapella_cid = AcapellaCID}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    track_tiktok_usage(MusicId) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    TikTokCount = Music#music.tiktok_usage_count,
                                    mnesia:write(Music#music{tiktok_usage_count = TikTokCount + 1}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    track_instagram_story_usage(MusicId) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    InstagramCount = Music#music.instagram_story_usage,
                                    mnesia:write(Music#music{instagram_story_usage = InstagramCount + 1}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    track_youtube_shorts_usage(MusicId) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    YouTubeCount = Music#music.youtube_shorts_usage,
                                    mnesia:write(Music#music{youtube_shorts_usage = YouTubeCount + 1}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    set_viral_moment(MusicId, ViralTimestamp) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{viral_moment_timestamp = ViralTimestamp}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    calculate_viral_coefficient(MusicId) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    Plays = Music#music.plays,
                                    Shares = Music#music.shares,
                                    TikTokUsage = Music#music.tiktok_usage_count,
                                    InstagramUsage = Music#music.instagram_story_usage,
                                    YouTubeUsage = Music#music.youtube_shorts_usage,

                                    ViralCoefficient = if
                                        Plays > 0 ->
                                            ((Shares + TikTokUsage + InstagramUsage + YouTubeUsage) / Plays) * 100;
                                        true -> 0.0
                                    end,

                                    mnesia:write(Music#music{viral_coefficient = ViralCoefficient}),
                                    {ok, ViralCoefficient}
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    get_music_by_tempo_range(MinBPM, MaxBPM) ->
                        Fun = fun() ->
                            AllMusic = mnesia:match_object(#music{_ = '_'}),
                            lists:filter(fun(Music) ->
                                case Music#music.tempo_bpm of
                                    undefined -> false;
                                    BPM -> BPM >= MinBPM andalso BPM =< MaxBPM
                                end
                            end, AllMusic)
                        end,

                        {atomic, Res} = mnesia:transaction(Fun),
                        Res.

                    get_music_by_key(MusicalKey) ->
                        Fun = fun() ->
                            mnesia:match_object(#music{key = MusicalKey, _ = '_'})
                        end,

                        {atomic, Res} = mnesia:transaction(Fun),
                        Res.

                    get_music_by_energy_level(MinEnergy, MaxEnergy) ->
                        Fun = fun() ->
                            AllMusic = mnesia:match_object(#music{_ = '_'}),
                            lists:filter(fun(Music) ->
                                case Music#music.energy_level of
                                    undefined -> false;
                                    Energy -> Energy >= MinEnergy andalso Energy =< MaxEnergy
                                end
                            end, AllMusic)
                        end,

                        {atomic, Res} = mnesia:transaction(Fun),
                        Res.

                    set_music_attributes(MusicId, Attributes) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    UpdatedMusic = Music#music{
                                        tempo_bpm = maps:get(tempo_bpm, Attributes, Music#music.tempo_bpm),
                                        key = maps:get(key, Attributes, Music#music.key),
                                        time_signature = maps:get(time_signature, Attributes, Music#music.time_signature),
                                        energy_level = maps:get(energy_level, Attributes, Music#music.energy_level),
                                        danceability = maps:get(danceability, Attributes, Music#music.danceability),
                                        acousticness = maps:get(acousticness, Attributes, Music#music.acousticness),
                                        instrumentalness = maps:get(instrumentalness, Attributes, Music#music.instrumentalness),
                                        vocal_presence = maps:get(vocal_presence, Attributes, Music#music.vocal_presence)
                                    },
                                    mnesia:write(UpdatedMusic),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    add_detected_instrument(MusicId, Instrument) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    DetectedInstruments = Music#music.detected_instruments,
                                    UpdatedInstruments = case lists:member(Instrument, DetectedInstruments) of
                                        true -> DetectedInstruments;
                                        false -> [Instrument | DetectedInstruments]
                                    end,
                                    mnesia:write(Music#music{detected_instruments = UpdatedInstruments}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    set_copyright_info(MusicId, CopyrightInfo) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{copyright_info = CopyrightInfo}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    set_license_type(MusicId, LicenseType) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{license_type = LicenseType}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    enable_sync_licensing(MusicId) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{sync_licensing_available = true}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    set_territorial_rights(MusicId, TerritorialRights) ->
                        Fun = fun() ->
                            case mnesia:read({music, MusicId}) of
                                [] -> {error, music_not_found};
                                [Music] ->
                                    mnesia:write(Music#music{territorial_rights = TerritorialRights}),
                                    ok
                            end
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, Result} -> Result;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    batch_update_music_status(MusicIds, NewStatus) ->
                        Fun = fun() ->
                            lists:foreach(fun(MusicId) ->
                                case mnesia:read({music, MusicId}) of
                                    [Music] ->
                                        mnesia:write(Music#music{status = NewStatus});
                                    [] ->
                                        ok
                                end
                            end, MusicIds)
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, _} -> ok;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                    get_music_count_by_status(Status) ->
                        Fun = fun() ->
                            AllMusic = mnesia:match_object(#music{status = Status, _ = '_'}),
                            length(AllMusic)
                        end,

                        {atomic, Count} = mnesia:transaction(Fun),
                        Count.

                    get_total_listen_time_by_user(UserId) ->
                        case userdb:get_user_by_id(UserId) of
                            user_not_exist ->
                                0;
                            error ->
                                0;
                            _User ->
                                Fun = fun() ->
                                    AllJourneys = mnesia:match_object(#listener_journey{user_id = UserId, _ = '_'}),
                                    lists:foldl(fun(Journey, Acc) ->
                                        case mnesia:read({music, Journey#listener_journey.music_id}) of
                                            [Music] ->
                                                Acc + Music#music.listen_time_total;
                                            [] ->
                                                Acc
                                        end
                                    end, 0, AllJourneys)
                                end,

                                {atomic, TotalTime} = mnesia:transaction(Fun),
                                TotalTime
                        end.

                    get_user_top_genres(UserId, Limit) ->
                        case userdb:get_user_by_id(UserId) of
                            user_not_exist ->
                                [];
                            error ->
                                [];
                            _User ->
                                Fun = fun() ->
                                    AllJourneys = mnesia:match_object(#listener_journey{user_id = UserId, _ = '_'}),
                                    GenreCounts = lists:foldl(fun(Journey, Acc) ->
                                        case mnesia:read({music, Journey#listener_journey.music_id}) of
                                            [Music] ->
                                                lists:foldl(fun(Genre, GenreAcc) ->
                                                    Count = maps:get(Genre, GenreAcc, 0),
                                                    maps:put(Genre, Count + 1, GenreAcc)
                                                end, Acc, Music#music.genre);
                                            [] ->
                                                Acc
                                        end
                                    end, #{}, AllJourneys),

                                    SortedGenres = lists:sort(fun({_, CountA}, {_, CountB}) ->
                                        CountA > CountB
                                    end, maps:to_list(GenreCounts)),

                                    lists:sublist(SortedGenres, Limit)
                                end,

                                {atomic, TopGenres} = mnesia:transaction(Fun),
                                TopGenres
                        end.

                    cleanup_old_music(DaysOld) ->
                        Fun = fun() ->
                            AllMusic = mnesia:match_object(#music{_ = '_'}),
                            Now = calendar:universal_time(),
                            NowSeconds = calendar:datetime_to_gregorian_seconds(Now),
                            CutoffSeconds = NowSeconds - (DaysOld * 86400),

                            lists:foreach(fun(Music) ->
                                CreatedSeconds = calendar:datetime_to_gregorian_seconds(Music#music.date_created),
                                if
                                    CreatedSeconds < CutoffSeconds andalso Music#music.status =:= failed ->
                                        mnesia:delete({music, Music#music.id});
                                    true ->
                                        ok
                                end
                            end, AllMusic)
                        end,

                        case mnesia:transaction(Fun) of
                            {atomic, _} -> ok;
                            {aborted, Reason} -> {error, {transaction_failed, Reason}}
                        end.

                        create_artist_request(UserId, Username, RequestData) ->
                            NormalizedUserId = normalize_id(UserId),
                            NormalizedUsername = normalize_id(Username),

                            error_logger:info_msg("[create_artist_request] userId=~p username=~p data=~p",
                                                  [NormalizedUserId, NormalizedUsername, RequestData]),

                            error_logger:info_msg("[create_artist_request] RequestData keys=~p",
                                                  [maps:keys(RequestData)]),

                            case userdb:get_user_by_id(NormalizedUserId) of
                                user_not_exist -> {error, user_not_found};
                                error          -> {error, user_lookup_failed};
                                _User ->
                                    case get_artist_request_status(NormalizedUserId) of
                                        {ok, _} -> {error, already_exists};
                                        _ ->
                                            RequestId = nanoid:gen(),
                                            Now = calendar:universal_time(),

                                            Request = #artist_request{
                                                id               = RequestId,
                                                user_id          = NormalizedUserId,
                                                username         = NormalizedUsername,
                                                status           = pending,
                                                requested_at     = Now,
                                                reviewed_at      = undefined,
                                                reviewed_by      = undefined,
                                                request_data     = RequestData,
                                                rejection_reason = undefined
                                            },

                                            error_logger:info_msg("[create_artist_request] Created record: ~p", [Request]),

                                            F = fun() -> mnesia:write(Request) end,

                                            case mnesia:transaction(F) of
                                                {atomic, ok}       -> {ok, RequestId};
                                                {aborted, Reason}  -> {error, {transaction_failed, Reason}}
                                            end
                                    end
                            end.

                        get_artist_request_status(UserId) ->
                            NormalizedUserId = normalize_id(UserId),

                            F = fun() ->
                                mnesia:match_object(#artist_request{user_id = NormalizedUserId, _ = '_'})
                            end,

                            case mnesia:transaction(F) of
                                {atomic, [Request | _]} -> {ok, Request#artist_request.status};
                                {atomic, []}            -> {error, not_found};
                                {aborted, _Reason}      -> {error, database_error}
                            end.

                        approve_artist_request(RequestId, AdminUsername) ->
                            NormalizedRequestId = normalize_id(RequestId),
                            NormalizedAdmin     = normalize_username(AdminUsername),

                            error_logger:info_msg("[approve_artist_request] requestId=~p admin=~p",
                                                  [NormalizedRequestId, NormalizedAdmin]),

                            AdminUsernames = ["arvand", "mazaryn", "zaryn"],

                            case lists:member(NormalizedAdmin, AdminUsernames) of
                                false ->
                                    error_logger:error_msg("[approve_artist_request] UNAUTHORIZED admin=~p not in ~p",
                                                           [NormalizedAdmin, AdminUsernames]),
                                    {error, unauthorized};
                                true ->
                                    F = fun() ->
                                        case mnesia:read(artist_request, NormalizedRequestId) of
                                            [Request] ->
                                                UserId      = Request#artist_request.user_id,
                                                Username    = Request#artist_request.username,
                                                RequestData = Request#artist_request.request_data,
                                                Now         = calendar:universal_time(),

                                                error_logger:info_msg("[approve_artist_request] RequestData=~p", [RequestData]),
                                                error_logger:info_msg("[approve_artist_request] RequestData keys=~p", [maps:keys(RequestData)]),

                                                UpdatedRequest = Request#artist_request{
                                                    status      = approved,
                                                    reviewed_at = Now,
                                                    reviewed_by = NormalizedAdmin
                                                },

                                                ArtistName = get_map_value(RequestData, artist_name, <<"artist_name">>, Username),
                                                Bio        = get_map_value(RequestData, bio,         <<"bio">>,         ""),
                                                GenresRaw  = get_map_value(RequestData, genres,      <<"genres">>,      ""),

                                                error_logger:info_msg("[approve_artist_request] Extracted: ArtistName=~p Bio=~p GenresRaw=~p",
                                                                       [ArtistName, Bio, GenresRaw]),

                                                Genres = case GenresRaw of
                                                    G when is_binary(G) -> string:split(binary_to_list(G), ",", all);
                                                    G when is_list(G)   -> string:split(G, ",", all);
                                                    _                   -> []
                                                end,

                                                ArtistId = nanoid:gen(),

                                                Artist = #artist{
                                                    id               = ArtistId,
                                                    user_id          = UserId,
                                                    stage_name       = ArtistName,
                                                    bio              = Bio,
                                                    profile_image_cid = undefined,
                                                    banner_image_cid  = undefined,
                                                    verified          = false,
                                                    genres            = Genres,
                                                    social_links      = #{},
                                                    monthly_listeners = 0,
                                                    total_plays       = 0,
                                                    followers         = [],
                                                    top_tracks        = [],
                                                    albums            = [],
                                                    singles           = [],
                                                    collaborations    = [],
                                                    date_created      = Now,
                                                    data              = #{}
                                                },

                                                mnesia:write(UpdatedRequest),
                                                mnesia:write(Artist),
                                                {ok, ArtistId};
                                            [] ->
                                                error_logger:error_msg("[approve_artist_request] NOT FOUND id=~p", [NormalizedRequestId]),
                                                {error, not_found}
                                        end
                                    end,

                                    case mnesia:transaction(F) of
                                        {atomic, {ok, ArtistId}} -> {ok, ArtistId};
                                        {atomic, {error, Reason}} -> {error, Reason};
                                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                                    end
                            end.

                        reject_artist_request(RequestId, AdminUsername, Reason) ->
                            NormalizedRequestId = normalize_id(RequestId),
                            NormalizedAdmin     = normalize_username(AdminUsername),
                            NormalizedReason    = normalize_id(Reason),

                            error_logger:info_msg("[reject_artist_request] requestId=~p admin=~p reason=~p",
                                                  [NormalizedRequestId, NormalizedAdmin, NormalizedReason]),

                            AdminUsernames = ["arvand", "mazaryn", "zaryn"],

                            case lists:member(NormalizedAdmin, AdminUsernames) of
                                false ->
                                    error_logger:error_msg("[reject_artist_request] UNAUTHORIZED admin=~p", [NormalizedAdmin]),
                                    {error, unauthorized};
                                true ->
                                    F = fun() ->
                                        case mnesia:read(artist_request, NormalizedRequestId) of
                                            [Request] ->
                                                Now = calendar:universal_time(),
                                                UpdatedRequest = Request#artist_request{
                                                    status           = rejected,
                                                    reviewed_at      = Now,
                                                    reviewed_by      = NormalizedAdmin,
                                                    rejection_reason = NormalizedReason
                                                },
                                                mnesia:write(UpdatedRequest),
                                                {ok, rejected};
                                            [] ->
                                                error_logger:error_msg("[reject_artist_request] NOT FOUND id=~p", [NormalizedRequestId]),
                                                {error, not_found}
                                        end
                                    end,

                                    case mnesia:transaction(F) of
                                        {atomic, {ok, rejected}} -> {ok, rejected};
                                        {atomic, {error, R}}     -> {error, R};
                                        {aborted, R}             -> {error, {transaction_failed, R}}
                                    end
                            end.

                        get_artist_by_user_id(UserId) ->
                            NormalizedUserId = normalize_id(UserId),

                            F = fun() ->
                                mnesia:match_object(#artist{user_id = NormalizedUserId, _ = '_'})
                            end,

                            case mnesia:transaction(F) of
                                {atomic, [Artist | _]} -> {ok, Artist};
                                {atomic, []}           -> {error, not_found};
                                {aborted, _Reason}     -> {error, database_error}
                            end.

                        get_pending_artist_requests() ->
                            F = fun() ->
                                mnesia:match_object(#artist_request{status = pending, _ = '_'})
                            end,

                            case mnesia:transaction(F) of
                                {atomic, Requests} -> {ok, Requests};
                                {aborted, _Reason} -> {error, database_error}
                            end.

                        get_map_value(Map, AtomKey, BinaryKey, Default) ->
                            case maps:find(AtomKey, Map) of
                                {ok, V} -> V;
                                error   ->
                                    case maps:find(BinaryKey, Map) of
                                        {ok, V} -> V;
                                        error   -> Default
                                    end
                            end.

                        normalize_id(Id) when is_binary(Id) -> binary_to_list(Id);
                        normalize_id(Id) when is_list(Id)   -> Id;
                        normalize_id(Id) when is_atom(Id)   -> atom_to_list(Id);
                        normalize_id(Id)                    -> lists:flatten(io_lib:format("~p", [Id])).

                        normalize_username(U) when is_binary(U) -> string:to_lower(string:trim(binary_to_list(U)));
                        normalize_username(U) when is_list(U)   -> string:to_lower(string:trim(U));
                        normalize_username(_)                   -> "".
