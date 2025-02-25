-module(post_hashtag).
-author("Zaryn Technologies").
-export([get_top_hashtags/3, get_top_hashtags_by_author/4, get_all_hashtags/0, get_monthly_hashtags/0, get_daily_hashtags/0, get_all_hashtags_by_author/1,
 get_monthly_hashtags_by_author/1, get_daily_hashtags_by_author/1]).

-include_lib("kernel/include/logger.hrl").


%% post_hashtag:get_top_hashtags("monthly", 2, 2025).
%% post_hashtag:get_top_hashtags("daily", "2025-02-24", undefined).
%% post_hashtag:get_top_hashtags("all_time", undefined, undefined).
get_top_hashtags(Period, TargetMonth, TargetYear) ->
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

            Url = "/api/top_hashtags?" ++ QueryParams,
            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:get(ConnPid, Url, Headers),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    #{<<"top_hashtags">> := Hashtags} = jsone:decode(RespBody),
                    Hashtags;  
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
get_top_hashtags_by_author(Author, Period, TargetMonth, TargetYear) ->
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

            Url = "/api/top_hashtags_by_author?" ++ QueryParams,
            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:get(ConnPid, Url, Headers),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    #{<<"top_hashtags">> := Hashtags} = jsone:decode(RespBody),
                    Hashtags;  
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

%% Get top hashtags for all time
get_all_hashtags() ->
    get_top_hashtags("all_time", undefined, undefined).

%% Get top hashtags for the current month
get_monthly_hashtags() ->
    {{Year, Month, _Day}, _Time} = calendar:local_time(),
    get_top_hashtags("monthly", Month, Year).

%% Get top hashtags for the current day
get_daily_hashtags() ->
    {{Year, Month, Day}, _Time} = calendar:local_time(),
    DateStr = io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]),
    get_top_hashtags("daily", DateStr, undefined).

%% Get top hashtags for all time by author
get_all_hashtags_by_author(Author) ->
    get_top_hashtags_by_author(Author, "all_time", undefined, undefined).

%% Get top hashtags for the current month by author
get_monthly_hashtags_by_author(Author) ->
    {{Year, Month, _Day}, _Time} = calendar:local_time(),
    get_top_hashtags_by_author(Author, "monthly", Month, Year).

%% Get top hashtags for the current day by author
get_daily_hashtags_by_author(Author) ->
    {{Year, Month, Day}, _Time} = calendar:local_time(),
    DateStr = io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]),
    get_top_hashtags_by_author(Author, "daily", DateStr, undefined).