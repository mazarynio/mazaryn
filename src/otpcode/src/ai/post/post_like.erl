-module(post_like).
-author("Zaryn Technologies").
-export([get_top_likes/3, get_likes_by_postID/1, get_likes_by_ai_post_id/1, get_all_likes/0, get_monthly_likes/0, get_daily_likes/0]).

-include_lib("kernel/include/logger.hrl").

%% Get top likes for a specific period
get_top_likes(Period, TargetMonth, TargetYear) ->
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

            Url = "/api/top_likes?" ++ QueryParams,
            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:get(ConnPid, Url, Headers),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    #{<<"top_likes">> := Likes} = jsone:decode(RespBody),
                    Likes;  
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

get_likes_by_postID(PostID) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            {ok, ConnPid} = gun:open("localhost", 8000),
            {ok, _Protocol} = gun:await_up(ConnPid),

            QueryParams = io_lib:format("postID=~s", [PostID]),

            Url = "/api/likes_by_postID?" ++ QueryParams,
            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:get(ConnPid, Url, Headers),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    #{<<"top_likes">> := Likes} = jsone:decode(RespBody),
                    Likes;  
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

%% Get likes by ai_post_id
get_likes_by_ai_post_id(AIPostID) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            {ok, ConnPid} = gun:open("localhost", 8000),
            {ok, _Protocol} = gun:await_up(ConnPid),

            QueryParams = io_lib:format("ai_post_id=~s", [AIPostID]),

            Url = "/api/likes_by_ai_post_id?" ++ QueryParams,
            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:get(ConnPid, Url, Headers),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    #{<<"top_likes">> := Likes} = jsone:decode(RespBody),
                    Likes;  
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

%% Get top likes for all time
get_all_likes() ->
    get_top_likes("all_time", undefined, undefined).

%% Get top likes for the current month
get_monthly_likes() ->
    {{Year, Month, _Day}, _Time} = calendar:local_time(),
    get_top_likes("monthly", Month, Year).

%% Get top likes for the current day
get_daily_likes() ->
    {{Year, Month, Day}, _Time} = calendar:local_time(),
    DateStr = io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]),
    get_top_likes("daily", DateStr, undefined).