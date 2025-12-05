-module(media_video_client).
-author("Zaryn Technologies").

-export([
    create_session/3,
    get_session/1,
    close_session/1,
    update_position/2,
    update_quality/2,
    set_playback_rate/2,
    set_volume/2,

    create_stream/4,
    get_stream/1,
    start_stream/1,
    end_stream/1,
    join_stream/2,
    leave_stream/2,
    list_live_streams/0,

    get_video_metrics/1,
    get_analytics_report/1,

    create_transcoding_job/3,
    get_transcoding_job/1,

    upload_video/2,
    get_video_info/1
]).

-define(RUST_API_BASE, "http://localhost:2020").
-define(DEFAULT_TIMEOUT, 120000).

create_session(VideoId, UserId, Quality) ->
    RequestBody = #{
        video_id => ensure_binary(VideoId),
        user_id => ensure_binary(UserId),
        quality => case Quality of
            undefined -> null;
            _ -> ensure_binary(Quality)
        end
    },

    Json = jsx:encode(RequestBody),
    ApiUrl = ?RUST_API_BASE ++ "/media/sessions",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"session_id">> := SessionId, <<"video_id">> := VidId, <<"quality">> := Qual} ->
                    {ok, binary_to_list(SessionId), binary_to_list(VidId), binary_to_list(Qual)};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_session(SessionId) ->
    ApiUrl = ?RUST_API_BASE ++ "/media/sessions/" ++ ensure_string(SessionId),

    case httpc:request(get, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Session when is_map(Session) ->
                    {ok, parse_session(Session)};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

close_session(SessionId) ->
    ApiUrl = ?RUST_API_BASE ++ "/media/sessions/" ++ ensure_string(SessionId),

    case httpc:request(delete, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

update_position(SessionId, Position) ->
    RequestBody = #{
        session_id => ensure_binary(SessionId),
        position => Position
    },

    Json = jsx:encode(RequestBody),
    ApiUrl = ?RUST_API_BASE ++ "/media/sessions/position",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

update_quality(SessionId, Quality) ->
    RequestBody = #{
        session_id => ensure_binary(SessionId),
        quality => ensure_binary(Quality)
    },

    Json = jsx:encode(RequestBody),
    ApiUrl = ?RUST_API_BASE ++ "/media/sessions/quality",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

set_playback_rate(SessionId, Rate) ->
    RequestBody = #{
        session_id => ensure_binary(SessionId),
        playback_rate => Rate
    },

    Json = jsx:encode(RequestBody),
    ApiUrl = ?RUST_API_BASE ++ "/media/sessions/playback_rate",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

set_volume(SessionId, Volume) ->
    RequestBody = #{
        session_id => ensure_binary(SessionId),
        volume => Volume
    },

    Json = jsx:encode(RequestBody),
    ApiUrl = ?RUST_API_BASE ++ "/media/sessions/volume",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

create_stream(VideoId, StreamerId, Title, LatencyMode) ->
    RequestBody = #{
        video_id => ensure_binary(VideoId),
        streamer_id => ensure_binary(StreamerId),
        title => ensure_binary(Title),
        latency_mode => ensure_binary(LatencyMode)
    },

    Json = jsx:encode(RequestBody),
    ApiUrl = ?RUST_API_BASE ++ "/media/streams",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Stream when is_map(Stream) ->
                    {ok, parse_stream(Stream)};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_stream(StreamId) ->
    ApiUrl = ?RUST_API_BASE ++ "/media/streams/" ++ ensure_string(StreamId),

    case httpc:request(get, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Stream when is_map(Stream) ->
                    {ok, parse_stream(Stream)};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

start_stream(StreamId) ->
    ApiUrl = ?RUST_API_BASE ++ "/media/streams/" ++ ensure_string(StreamId) ++ "/start",

    case httpc:request(post, {ApiUrl, [], "application/json", "{}"},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

end_stream(StreamId) ->
    ApiUrl = ?RUST_API_BASE ++ "/media/streams/" ++ ensure_string(StreamId) ++ "/end",

    case httpc:request(post, {ApiUrl, [], "application/json", "{}"},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

join_stream(StreamId, UserId) ->
    RequestBody = #{
        stream_id => ensure_binary(StreamId),
        user_id => ensure_binary(UserId)
    },

    Json = jsx:encode(RequestBody),
    ApiUrl = ?RUST_API_BASE ++ "/media/streams/join",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

leave_stream(StreamId, UserId) ->
    RequestBody = #{
        stream_id => ensure_binary(StreamId),
        user_id => ensure_binary(UserId)
    },

    Json = jsx:encode(RequestBody),
    ApiUrl = ?RUST_API_BASE ++ "/media/streams/leave",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

list_live_streams() ->
    ApiUrl = ?RUST_API_BASE ++ "/media/streams/live",

    case httpc:request(get, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Streams when is_list(Streams) ->
                    {ok, [parse_stream(S) || S <- Streams]};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_video_metrics(VideoId) ->
    ApiUrl = ?RUST_API_BASE ++ "/media/analytics/" ++ ensure_string(VideoId),

    case httpc:request(get, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Metrics when is_map(Metrics) ->
                    {ok, parse_metrics(Metrics)};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_analytics_report(VideoId) ->
    ApiUrl = ?RUST_API_BASE ++ "/media/analytics/" ++ ensure_string(VideoId) ++ "/report",

    case httpc:request(get, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Report when is_map(Report) ->
                    {ok, Report};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

create_transcoding_job(VideoId, SourceCid, Qualities) ->
    RequestBody = #{
        video_id => ensure_binary(VideoId),
        source_cid => ensure_binary(SourceCid),
        qualities => [ensure_binary(Q) || Q <- Qualities]
    },

    Json = jsx:encode(RequestBody),
    ApiUrl = ?RUST_API_BASE ++ "/media/transcoding",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"job_id">> := JobId} ->
                    {ok, binary_to_list(JobId)};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_transcoding_job(JobId) ->
    ApiUrl = ?RUST_API_BASE ++ "/media/transcoding/" ++ ensure_string(JobId),

    case httpc:request(get, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Job when is_map(Job) ->
                    {ok, parse_transcoding_job(Job)};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

upload_video(VideoId, FilePath) ->
    case file:read_file(FilePath) of
        {ok, FileContent} ->
            ApiUrl = ?RUST_API_BASE ++ "/media/upload",

            Boundary = "----WebKitFormBoundary" ++ nanoid:gen(),
            ContentType = "multipart/form-data; boundary=" ++ Boundary,

            Body = create_multipart_body(Boundary, VideoId, FilePath, FileContent),

            case httpc:request(post, {ApiUrl, [], ContentType, Body},
                              [{timeout, 300000}], []) of
                {ok, {{_, 200, _}, _, ResponseBody}} ->
                    case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                        #{<<"status">> := <<"ok">>} ->
                            ok;
                        _ ->
                            {error, invalid_response}
                    end;
                {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
                    {error, {http_error, StatusCode, ErrorBody}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

    get_video_info(FilePath) ->
        RequestBody = #{
            file_path => ensure_binary(FilePath)
        },

        Json = jsx:encode(RequestBody),
        ApiUrl = ?RUST_API_BASE ++ "/media/info",

        case httpc:request(post, {ApiUrl, [], "application/json", Json},
                          [{timeout, 60000}], []) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                    Info when is_map(Info) ->
                        {ok, parse_video_info(Info)};
                    _ ->
                        {error, invalid_response}
                end;
            {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
                {error, {http_error, StatusCode, ErrorBody}};
            {error, Reason} ->
                {error, Reason}
        end.

create_multipart_body(Boundary, VideoId, FilePath, FileContent) ->
    FileName = filename:basename(FilePath),

    Part1 = "--" ++ Boundary ++ "\r\n" ++
            "Content-Disposition: form-data; name=\"video_id\"\r\n\r\n" ++
            VideoId ++ "\r\n",

    Part2 = "--" ++ Boundary ++ "\r\n" ++
            "Content-Disposition: form-data; name=\"file\"; filename=\"" ++ FileName ++ "\"\r\n" ++
            "Content-Type: application/octet-stream\r\n\r\n",

    Part3 = "\r\n--" ++ Boundary ++ "--\r\n",

    list_to_binary([Part1, Part2, FileContent, Part3]).

parse_session(Session) ->
    #{
        session_id => binary_to_list(maps:get(<<"session_id">>, Session)),
        video_id => binary_to_list(maps:get(<<"video_id">>, Session)),
        user_id => binary_to_list(maps:get(<<"user_id">>, Session)),
        quality => binary_to_list(maps:get(<<"quality">>, Session)),
        position => maps:get(<<"position">>, Session),
        playback_rate => maps:get(<<"playback_rate">>, Session),
        volume => maps:get(<<"volume">>, Session),
        buffering => maps:get(<<"buffering">>, Session)
    }.

parse_stream(Stream) ->
    #{
        stream_id => binary_to_list(maps:get(<<"stream_id">>, Stream)),
        video_id => binary_to_list(maps:get(<<"video_id">>, Stream)),
        streamer_id => binary_to_list(maps:get(<<"streamer_id">>, Stream)),
        title => binary_to_list(maps:get(<<"title">>, Stream)),
        status => parse_stream_status(maps:get(<<"status">>, Stream)),
        ingest_url => binary_to_list(maps:get(<<"ingest_url">>, Stream)),
        playback_url => binary_to_list(maps:get(<<"playback_url">>, Stream)),
        stream_key => binary_to_list(maps:get(<<"stream_key">>, Stream)),
        current_viewers => maps:get(<<"current_viewers">>, Stream),
        peak_viewers => maps:get(<<"peak_viewers">>, Stream)
    }.

parse_stream_status(<<"Scheduled">>) -> scheduled;
parse_stream_status(<<"Live">>) -> live;
parse_stream_status(<<"Ended">>) -> ended;
parse_stream_status(<<"Failed">>) -> failed;
parse_stream_status(_) -> unknown.

parse_metrics(Metrics) ->
    #{
        total_views => maps:get(<<"total_views">>, Metrics),
        unique_viewers => maps:get(<<"unique_viewers">>, Metrics, 0),
        total_watch_time => maps:get(<<"total_watch_time">>, Metrics),
        completed_views => maps:get(<<"completed_views">>, Metrics),
        buffer_events => maps:get(<<"buffer_events">>, Metrics),
        quality_changes => maps:get(<<"quality_changes">>, Metrics)
    }.

parse_transcoding_job(Job) ->
    #{
        job_id => binary_to_list(maps:get(<<"job_id">>, Job)),
        video_id => binary_to_list(maps:get(<<"video_id">>, Job)),
        status => parse_job_status(maps:get(<<"status">>, Job)),
        progress => maps:get(<<"progress">>, Job),
        error => case maps:get(<<"error">>, Job, null) of
            null -> undefined;
            Error -> binary_to_list(Error)
        end
    }.

parse_job_status(<<"Pending">>) -> pending;
parse_job_status(<<"Processing">>) -> processing;
parse_job_status(<<"Completed">>) -> completed;
parse_job_status(<<"Failed">>) -> failed;
parse_job_status(_) -> unknown.

parse_video_info(Info) ->
    #{
        duration => maps:get(<<"duration">>, Info),
        resolution => parse_resolution(maps:get(<<"resolution">>, Info)),
        codec => binary_to_list(maps:get(<<"codec">>, Info)),
        bitrate => maps:get(<<"bitrate">>, Info),
        frame_rate => maps:get(<<"frame_rate">>, Info),
        format => binary_to_list(maps:get(<<"format">>, Info, <<"unknown">>))
    }.

parse_resolution(Res) ->
    #{
        width => maps:get(<<"width">>, Res),
        height => maps:get(<<"height">>, Res)
    }.

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8).

ensure_string(Value) when is_list(Value) -> Value;
ensure_string(Value) when is_binary(Value) -> binary_to_list(Value);
ensure_string(Value) when is_atom(Value) -> atom_to_list(Value).
