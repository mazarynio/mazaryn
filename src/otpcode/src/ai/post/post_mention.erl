-module(post_mention).
-author("Zaryn Technologies").
-export([get_top_mentions/3, get_top_mentions_by_author/4, get_all_mentions/0, get_monthly_mentions/0, get_daily_mentions/0, get_all_mentions_by_author/1,
get_monthly_mentions_by_author/1, get_daily_mentions_by_author/1]).

-include_lib("kernel/include/logger.hrl").

%% Get top mentions for a specific period
get_top_mentions(Period, TargetMonth, TargetYear) ->
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

            Url = "/api/top_mentions?" ++ QueryParams,
            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:get(ConnPid, Url, Headers),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    #{<<"top_mentions">> := Mentions} = jsone:decode(RespBody),
                    Mentions;  
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
get_top_mentions_by_author(Author, Period, TargetMonth, TargetYear) ->
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

            Url = "/api/top_mentions_by_author?" ++ QueryParams,
            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:get(ConnPid, Url, Headers),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    #{<<"top_mentions">> := Mentions} = jsone:decode(RespBody),
                    Mentions;  
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

%% Get top mentions for all time
get_all_mentions() ->
    get_top_mentions("all_time", undefined, undefined).

%% Get top mentions for the current month
get_monthly_mentions() ->
    {{Year, Month, _Day}, _Time} = calendar:local_time(),
    get_top_mentions("monthly", Month, Year).

%% Get top mentions for the current day
get_daily_mentions() ->
    {{Year, Month, Day}, _Time} = calendar:local_time(),
    DateStr = io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]),
    get_top_mentions("daily", DateStr, undefined).

%% Get top mentions for all time by author
get_all_mentions_by_author(Author) ->
    get_top_mentions_by_author(Author, "all_time", undefined, undefined).

%% Get top mentions for the current month by author
get_monthly_mentions_by_author(Author) ->
    {{Year, Month, _Day}, _Time} = calendar:local_time(),
    get_top_mentions_by_author(Author, "monthly", Month, Year).

%% Get top mentions for the current day by author
get_daily_mentions_by_author(Author) ->
    {{Year, Month, Day}, _Time} = calendar:local_time(),
    DateStr = io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]),
    get_top_mentions_by_author(Author, "daily", DateStr, undefined).