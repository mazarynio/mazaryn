-module(post_emoji).
-author("Zaryn Technologies").
-export([get_top_emojis/3, get_top_emojis_by_author/4, get_all_emojis/0, get_monthly_emojis/0, get_daily_emojis/0, get_all_emojis_by_author/1,
 get_monthly_emojis_by_author/1, get_daily_emojis_by_author/1]).

-include_lib("kernel/include/logger.hrl").

%% Get top emojis for a specific period
get_top_emojis(Period, TargetMonth, TargetYear) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            {ok, ConnPid} = gun:open("localhost", 8000),
            {ok, _Protocol} = gun:await_up(ConnPid),

            QueryParams = case Period of
                "monthly" ->
                    io_lib:format("period=~s&target_month=~p&target_year=~p", [Period, TargetMonth, TargetYear]);
                "daily" ->
                    io_lib:format("period=~s&target_day=~s", [Period, TargetMonth]); 
                _ ->
                    io_lib:format("period=~s", [Period])
            end,

            Url = "/api/top_emojis?" ++ QueryParams,
            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:get(ConnPid, Url, Headers),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    #{<<"top_emojis">> := Emojis} = jsone:decode(RespBody),
                    Emojis;  
                {response, nofin, StatusCode, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    ?LOG_ERROR("Error from Python service: ~p ~p", [StatusCode, RespBody]),
                    {error, RespBody};
                {error, Reason} ->
                    ?LOG_ERROR("Error calling Python service: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG_ERROR("Failed to start gun: ~p", [Reason]),
            {error, Reason}
    end.

%% Helper function to call the Python API
get_top_emojis_by_author(Author, Period, TargetMonth, TargetYear) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            {ok, ConnPid} = gun:open("localhost", 8000),
            {ok, _Protocol} = gun:await_up(ConnPid),

            QueryParams = case Period of
                "monthly" ->
                    io_lib:format("author=~s&period=~s&target_month=~p&target_year=~p", [Author, Period, TargetMonth, TargetYear]);
                "daily" ->
                    io_lib:format("author=~s&period=~s&target_day=~s", [Author, Period, TargetMonth]); 
                _ ->
                    io_lib:format("author=~s&period=~s", [Author, Period])
            end,

            Url = "/api/top_emojis_by_author?" ++ QueryParams,
            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:get(ConnPid, Url, Headers),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    #{<<"top_emojis">> := Emojis} = jsone:decode(RespBody),
                    Emojis;  
                {response, nofin, StatusCode, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    ?LOG_ERROR("Error from Python service: ~p ~p", [StatusCode, RespBody]),
                    {error, RespBody};
                {error, Reason} ->
                    ?LOG_ERROR("Error calling Python service: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG_ERROR("Failed to start gun: ~p", [Reason]),
            {error, Reason}
    end.

%% Get top emojis for all time
get_all_emojis() ->
    get_top_emojis("all_time", undefined, undefined).

%% Get top emojis for the current month
get_monthly_emojis() ->
    {{Year, Month, _Day}, _Time} = calendar:local_time(),
    get_top_emojis("monthly", Month, Year).

%% Get top emojis for the current day
get_daily_emojis() ->
    {{Year, Month, Day}, _Time} = calendar:local_time(),
    DateStr = io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]),
    get_top_emojis("daily", DateStr, undefined).

%% Get top emojis for all time by author
get_all_emojis_by_author(Author) ->
    get_top_emojis_by_author(Author, "all_time", undefined, undefined).

%% Get top emojis for the current month by author
get_monthly_emojis_by_author(Author) ->
    {{Year, Month, _Day}, _Time} = calendar:local_time(),
    get_top_emojis_by_author(Author, "monthly", Month, Year).

%% Get top emojis for the current day by author
get_daily_emojis_by_author(Author) ->
    {{Year, Month, Day}, _Time} = calendar:local_time(),
    DateStr = io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]),
    get_top_emojis_by_author(Author, "daily", DateStr, undefined).
