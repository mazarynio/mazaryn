-module(download_manager_client).
-author("Zaryn Technologies").

-export([
    start_download/6,
    start_download/7,
    get_download_status/1,
    pause_download/1,
    resume_download/1,
    cancel_download/1,
    delete_download/1,
    list_user_downloads/1,
    download_dataset/3,
    download_dataset/4,
    download_competition_dataset/4,
    download_competition_dataset/5,
    bulk_download_datasets/2,
    monitor_download/1,
    wait_for_completion/1,
    wait_for_completion/2
]).

-include("../records.hrl").
-include("../ml_records.hrl").

-define(RUST_API_BASE, "http://localhost:2020").
-define(DEFAULT_TIMEOUT, 30000).
-define(POLL_INTERVAL, 2000).

start_download(Url, Destination, UserId, DatasetId, CompetitionId, Priority) ->
    start_download(Url, Destination, UserId, DatasetId, CompetitionId, Priority, #{}).

    start_download(Url, Destination, UserId, DatasetId, CompetitionId, Priority, Options) ->
        error_logger:info_msg("download_manager_client:start_download called with URL: ~p", [Url]),
        error_logger:info_msg("Destination: ~p", [Destination]),
        error_logger:info_msg("UserId: ~p", [UserId]),

        DatasetIdValue = case DatasetId of
            undefined -> null;
            _ -> ensure_binary(DatasetId)
        end,

        CompetitionIdValue = case CompetitionId of
            undefined -> null;
            _ -> ensure_binary(CompetitionId)
        end,

        ChecksumValue = maps:get(checksum, Options, null),
        ExpectedSizeValue = maps:get(expected_size, Options, null),

        RequestBody = #{
            url => ensure_binary(Url),
            destination => ensure_binary(Destination),
            user_id => ensure_binary(UserId),
            dataset_id => DatasetIdValue,
            competition_id => CompetitionIdValue,
            priority => priority_to_string(Priority),
            chunk_size_mb => maps:get(chunk_size_mb, Options, 10),
            max_connections => maps:get(max_connections, Options, 8),
            checksum => ChecksumValue,
            expected_size => ExpectedSizeValue
        },

        error_logger:info_msg("Request body: ~p", [RequestBody]),

        Json = jsx:encode(RequestBody),
        ApiUrl = ?RUST_API_BASE ++ "/downloads/start",

        error_logger:info_msg("Sending POST request to: ~p", [ApiUrl]),
        error_logger:info_msg("Request JSON: ~p", [Json]),

        case httpc:request(post, {ApiUrl, [], "application/json", Json},
                          [{timeout, ?DEFAULT_TIMEOUT}], []) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                error_logger:info_msg("Received response: ~p", [ResponseBody]),
                case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                    #{<<"download_id">> := DownloadId} ->
                        error_logger:info_msg("Download ID: ~p", [DownloadId]),
                        {ok, binary_to_list(DownloadId)};
                    _ ->
                        error_logger:error_msg("Invalid response format"),
                        {error, invalid_response}
                end;
            {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
                error_logger:error_msg("HTTP error ~p: ~p", [StatusCode, ErrorBody]),
                {error, {http_error, StatusCode, ErrorBody}};
            {error, Reason} ->
                error_logger:error_msg("Request failed: ~p", [Reason]),
                {error, Reason}
        end.

get_download_status(DownloadId) ->
    ApiUrl = ?RUST_API_BASE ++ "/downloads/" ++ ensure_string(DownloadId),

    case httpc:request(get, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Info when is_map(Info) ->
                    {ok, parse_download_info(Info)};
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

pause_download(DownloadId) ->
    ApiUrl = ?RUST_API_BASE ++ "/downloads/" ++ ensure_string(DownloadId) ++ "/pause",

    case httpc:request(post, {ApiUrl, [], "application/json", "{}"},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

resume_download(DownloadId) ->
    ApiUrl = ?RUST_API_BASE ++ "/downloads/" ++ ensure_string(DownloadId) ++ "/resume",

    case httpc:request(post, {ApiUrl, [], "application/json", "{}"},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

cancel_download(DownloadId) ->
    ApiUrl = ?RUST_API_BASE ++ "/downloads/" ++ ensure_string(DownloadId) ++ "/cancel",

    case httpc:request(post, {ApiUrl, [], "application/json", "{}"},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

delete_download(DownloadId) ->
    ApiUrl = ?RUST_API_BASE ++ "/downloads/" ++ ensure_string(DownloadId),

    case httpc:request(delete, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

list_user_downloads(UserId) ->
    ApiUrl = ?RUST_API_BASE ++ "/downloads/user/" ++ ensure_string(UserId),

    case httpc:request(get, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Downloads when is_list(Downloads) ->
                    {ok, [parse_download_info(Info) || Info <- Downloads]};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

download_dataset(DatasetId, UserId, DestinationDir) ->
    download_dataset(DatasetId, UserId, DestinationDir, #{}).

download_dataset(DatasetId, UserId, DestinationDir, Options) ->
    case datasetdb:get_dataset_by_id(DatasetId) of
        {error, dataset_not_found} ->
            {error, dataset_not_found};
        #dataset{} = Dataset ->
            ContentCID = Dataset#dataset.content_cid,
            Title = Dataset#dataset.title,

            Filename = sanitize_filename(Title) ++ ".dat",
            Destination = filename:join(DestinationDir, Filename),

            Url = "https://gateway.pinata.cloud/ipfs/" ++ ensure_string(ContentCID),

            Checksum = case Dataset#dataset.metadata of
                #{checksum := CS} -> CS;
                _ -> undefined
            end,

            OptionsWithChecksum = case Checksum of
                undefined -> Options;
                _ -> maps:put(checksum, Checksum, Options)
            end,

            start_download(
                Url,
                Destination,
                UserId,
                DatasetId,
                undefined,
                high,
                OptionsWithChecksum
            );
        Error ->
            Error
    end.

download_competition_dataset(CompetitionId, DatasetId, UserId, DestinationDir) ->
    download_competition_dataset(CompetitionId, DatasetId, UserId, DestinationDir, #{}).

download_competition_dataset(CompetitionId, DatasetId, UserId, DestinationDir, Options) ->
    case competitiondb:is_participant(CompetitionId, UserId) of
        false ->
            {error, not_a_participant};
        true ->
            case competitiondb:get_competition_by_id(CompetitionId) of
                {error, Reason} ->
                    {error, Reason};
                #competition{} = Competition ->
                    DatasetIds = Competition#competition.dataset_ids,
                    case lists:member(DatasetId, DatasetIds) of
                        false ->
                            {error, dataset_not_in_competition};
                        true ->
                            download_dataset(DatasetId, UserId, DestinationDir, Options)
                    end;
                Error ->
                    Error
            end
    end.

bulk_download_datasets(DatasetIds, UserId) ->
    DestinationDir = "/tmp/mazaryn_downloads/" ++ UserId,
    ok = filelib:ensure_dir(DestinationDir ++ "/"),

    Results = lists:map(fun(DatasetId) ->
        case download_dataset(DatasetId, UserId, DestinationDir) of
            {ok, DownloadId} ->
                {DatasetId, {ok, DownloadId}};
            {error, Reason} ->
                {DatasetId, {error, Reason}}
        end
    end, DatasetIds),

    {ok, Results}.

monitor_download(DownloadId) ->
    case get_download_status(DownloadId) of
        {ok, Info} ->
            Status = maps:get(status, Info),
            Progress = maps:get(progress_percentage, Info),
            Speed = maps:get(speed_bps, Info),
            ETA = maps:get(eta_seconds, Info),

            io:format("Download ~s: ~s (~.2f%) - Speed: ~s/s - ETA: ~s~n",
                     [DownloadId, Status, Progress, format_bytes(Speed), format_time(ETA)]),

            case Status of
                <<"Completed">> ->
                    {completed, Info};
                <<"Failed">> ->
                    {failed, maps:get(error, Info, <<"Unknown error">>)};
                _ ->
                    timer:sleep(?POLL_INTERVAL),
                    monitor_download(DownloadId)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

wait_for_completion(DownloadId) ->
    wait_for_completion(DownloadId, infinity).

wait_for_completion(DownloadId, Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_completion_loop(DownloadId, Timeout, StartTime).

wait_for_completion_loop(DownloadId, Timeout, StartTime) ->
    case get_download_status(DownloadId) of
        {ok, Info} ->
            Status = maps:get(status, Info),
            case Status of
                <<"Completed">> ->
                    {ok, completed};
                <<"Failed">> ->
                    {error, maps:get(error, Info, <<"Unknown error">>)};
                _ ->
                    case Timeout of
                        infinity ->
                            timer:sleep(?POLL_INTERVAL),
                            wait_for_completion_loop(DownloadId, Timeout, StartTime);
                        _ ->
                            Elapsed = erlang:monotonic_time(millisecond) - StartTime,
                            case Elapsed >= Timeout of
                                true ->
                                    {error, timeout};
                                false ->
                                    timer:sleep(?POLL_INTERVAL),
                                    wait_for_completion_loop(DownloadId, Timeout, StartTime)
                            end
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

priority_to_string(low) -> <<"low">>;
priority_to_string(normal) -> <<"normal">>;
priority_to_string(high) -> <<"high">>;
priority_to_string(critical) -> <<"critical">>;
priority_to_string(_) -> <<"normal">>.

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
ensure_binary(Value) -> list_to_binary(io_lib:format("~p", [Value])).

ensure_string(Value) when is_list(Value) -> Value;
ensure_string(Value) when is_binary(Value) -> binary_to_list(Value);
ensure_string(Value) when is_atom(Value) -> atom_to_list(Value);
ensure_string(Value) -> lists:flatten(io_lib:format("~p", [Value])).

parse_download_info(Info) ->
    #{
        id => maps:get(<<"id">>, Info),
        url => maps:get(<<"url">>, Info),
        destination => maps:get(<<"destination">>, Info),
        status => maps:get(<<"status">>, Info),
        progress_percentage => maps:get(<<"progress_percentage">>, Info, 0.0),
        speed_bps => maps:get(<<"speed_bps">>, Info, 0),
        eta_seconds => maps:get(<<"eta_seconds">>, Info, null),
        downloaded_size => maps:get(<<"downloaded_size">>, Info, 0),
        total_size => maps:get(<<"total_size">>, Info, null),
        user_id => maps:get(<<"user_id">>, Info),
        dataset_id => maps:get(<<"dataset_id">>, Info, null),
        competition_id => maps:get(<<"competition_id">>, Info, null),
        error => maps:get(<<"error">>, Info, null)
    }.

sanitize_filename(Filename) when is_binary(Filename) ->
    sanitize_filename(binary_to_list(Filename));
sanitize_filename(Filename) when is_list(Filename) ->
    Re = "[^a-zA-Z0-9._-]",
    re:replace(Filename, Re, "_", [global, {return, list}]);
sanitize_filename(Filename) ->
    sanitize_filename(io_lib:format("~p", [Filename])).

format_bytes(Bytes) when is_integer(Bytes) ->
    if
        Bytes >= 1073741824 ->
            io_lib:format("~.2f GB", [Bytes / 1073741824]);
        Bytes >= 1048576 ->
            io_lib:format("~.2f MB", [Bytes / 1048576]);
        Bytes >= 1024 ->
            io_lib:format("~.2f KB", [Bytes / 1024]);
        true ->
            io_lib:format("~p B", [Bytes])
    end;
format_bytes(_) ->
    "N/A".

format_time(null) ->
    "N/A";
format_time(Seconds) when is_integer(Seconds) ->
    Hours = Seconds div 3600,
    Minutes = (Seconds rem 3600) div 60,
    Secs = Seconds rem 60,

    if
        Hours > 0 ->
            io_lib:format("~ph ~pm ~ps", [Hours, Minutes, Secs]);
        Minutes > 0 ->
            io_lib:format("~pm ~ps", [Minutes, Secs]);
        true ->
            io_lib:format("~ps", [Secs])
    end;
format_time(_) ->
    "N/A".
