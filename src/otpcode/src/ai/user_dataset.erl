-module(user_dataset).
-author("Zaryn Technologies").
-export([serialize_user/1, save_to_tfrecord/2, save_to_csv/2, save_to_json/2, save_to_parquet/2, append_to_tfrecord/2]).

-include_lib("kernel/include/logger.hrl").
-include("../records.hrl").

serialize_user({user, Id, AiUserId, BusinessId, AdsId, QuantumId, Username, Password, Email, Address, Knode, Media, Post, BlogPost, Notif, Following, Follower, Blocked, SavedPosts, OtherInfo, Private, DateCreated, DateUpdated, AvatarUrl, BannerUrl, TokenId, Chat, Verified, Report, Level, LastActivity, Suspend, Data}) ->

    {{Year, Month, Day}, {Hour, Minute, Second}} = DateCreated,
    TimestampStr = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Year, Month, Day, Hour, Minute, Second]),

    % Convert all fields to plain strings or binaries
    ToString = fun
        (Value) when is_binary(Value) ->
            case is_valid_utf8(Value) of
                true -> binary_to_list(Value);
                false -> binary_to_list(base64:encode(Value))
            end;
        (Value) when is_list(Value) -> Value;
        (Value) when is_atom(Value) -> atom_to_list(Value);
        (Value) when is_integer(Value) -> integer_to_list(Value);
        (undefined) -> "";
        (Value) -> lists:flatten(io_lib:format("~p", [Value]))
    end,

    % Create a map with all fields as plain strings
    #{
        <<"id">> => list_to_binary(ToString(Id)),
        <<"ai_user_id">> => list_to_binary(ToString(AiUserId)),
        <<"business_id">> => list_to_binary(ToString(BusinessId)),
        <<"ads_id">> => list_to_binary(ToString(AdsId)),
        <<"quantum_id">> => list_to_binary(ToString(QuantumId)),
        <<"username">> => list_to_binary(ToString(Username)),
        <<"password">> => list_to_binary(ToString(Password)),
        <<"email">> => list_to_binary(ToString(Email)),
        <<"address">> => list_to_binary(ToString(Address)),
        <<"knode">> => list_to_binary(ToString(Knode)),
        <<"media">> => list_to_binary(ToString(Media)),
        <<"post">> => list_to_binary(ToString(Post)),
        <<"blog_post">> => list_to_binary(ToString(BlogPost)),
        <<"notif">> => list_to_binary(ToString(Notif)),
        <<"following">> => list_to_binary(ToString(Following)),
        <<"follower">> => list_to_binary(ToString(Follower)),
        <<"blocked">> => list_to_binary(ToString(Blocked)),
        <<"saved_posts">> => list_to_binary(ToString(SavedPosts)),
        <<"other_info">> => list_to_binary(ToString(OtherInfo)),
        <<"private">> => list_to_binary(ToString(Private)),
        <<"date_created">> => list_to_binary(ToString(TimestampStr)),
        <<"date_updated">> => list_to_binary(ToString(DateUpdated)),
        <<"avatar_url">> => list_to_binary(ToString(AvatarUrl)),
        <<"banner_url">> => list_to_binary(ToString(BannerUrl)),
        <<"token_id">> => list_to_binary(ToString(TokenId)),
        <<"chat">> => list_to_binary(ToString(Chat)),
        <<"verified">> => list_to_binary(ToString(Verified)),
        <<"report">> => list_to_binary(ToString(Report)),
        <<"level">> => list_to_binary(ToString(Level)),
        <<"last_activity">> => list_to_binary(ToString(LastActivity)),
        <<"suspend">> => list_to_binary(ToString(Suspend))
    }.

is_valid_utf8(Binary) ->
    case unicode:characters_to_list(Binary, utf8) of
        {incomplete, _, _} -> false;
        {error, _, _} -> false;
        _ -> true
    end.

save_to_csv(User, Filename) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            JsonData = serialize_user(User),
            JsonDataWithFilename = JsonData#{<<"filename">> => list_to_binary(Filename)}, 

            {ok, ConnPid} = gun:open("localhost", 8000), 
            {ok, _Protocol} = gun:await_up(ConnPid),

            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:post(ConnPid, "/api/convert_user_to_csv", Headers, jsone:encode(JsonDataWithFilename)),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    case file:write_file(Filename, RespBody) of
                        ok ->
                            io:format("User saved to ~s~n", [Filename]),
                            ok;
                        {error, Reason} ->
                            io:format("Error writing to file ~s: ~p~n", [Filename, Reason]),
                            {error, Reason}
                    end;
                {response, nofin, StatusCode, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    io:format("Error from Python service: ~p ~p~n", [StatusCode, RespBody]),
                    {error, RespBody};
                {error, Reason} ->
                    io:format("Error calling Python service: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Failed to start gun: ~p~n", [Reason]),
            {error, Reason}
    end.

save_to_tfrecord(User, Filename) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            JsonData = serialize_user(User),
            JsonDataWithFilename = JsonData#{<<"filename">> => list_to_binary(Filename)}, 

            {ok, ConnPid} = gun:open("localhost", 8000), 
            {ok, _Protocol} = gun:await_up(ConnPid),

            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:post(ConnPid, "/api/convert_user_to_tfrecord", Headers, jsone:encode(JsonDataWithFilename)),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    case file:write_file(Filename, RespBody, [append]) of
                        ok ->
                            io:format("User saved to ~s~n", [Filename]),
                            ok;
                        {error, Reason} ->
                            io:format("Error writing to file ~s: ~p~n", [Filename, Reason]),
                            {error, Reason}
                    end;
                {response, nofin, StatusCode, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    io:format("Error from Python service: ~p ~p~n", [StatusCode, RespBody]),
                    {error, RespBody};
                {error, Reason} ->
                    io:format("Error calling Python service: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Failed to start gun: ~p~n", [Reason]),
            {error, Reason}
    end.

save_to_json(User, Filename) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            JsonData = serialize_user(User),
            JsonDataWithFilename = JsonData#{<<"filename">> => list_to_binary(Filename)}, 

            {ok, ConnPid} = gun:open("localhost", 8000), 
            {ok, _Protocol} = gun:await_up(ConnPid),

            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:post(ConnPid, "/api/convert_user_to_json", Headers, jsone:encode(JsonDataWithFilename)),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    case file:write_file(Filename, RespBody) of
                        ok ->
                            io:format("User saved to ~s~n", [Filename]),
                            ok;
                        {error, Reason} ->
                            io:format("Error writing to file ~s: ~p~n", [Filename, Reason]),
                            {error, Reason}
                    end;
                {response, nofin, StatusCode, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    io:format("Error from Python service: ~p ~p~n", [StatusCode, RespBody]),
                    {error, RespBody};
                {error, Reason} ->
                    io:format("Error calling Python service: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Failed to start gun: ~p~n", [Reason]),
            {error, Reason}
    end.

save_to_parquet(User, Filename) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            JsonData = serialize_user(User),
            JsonDataWithFilename = JsonData#{<<"filename">> => list_to_binary(Filename)}, 

            {ok, ConnPid} = gun:open("localhost", 8000), 
            {ok, _Protocol} = gun:await_up(ConnPid),

            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:post(ConnPid, "/api/convert_user_to_parquet", Headers, jsone:encode(JsonDataWithFilename)),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    case file:write_file(Filename, RespBody) of
                        ok ->
                            io:format("User saved to ~s~n", [Filename]),
                            ok;
                        {error, Reason} ->
                            io:format("Error writing to file ~s: ~p~n", [Filename, Reason]),
                            {error, Reason}
                    end;
                {response, nofin, StatusCode, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    io:format("Error from Python service: ~p ~p~n", [StatusCode, RespBody]),
                    {error, RespBody};
                {error, Reason} ->
                    io:format("Error calling Python service: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Failed to start gun: ~p~n", [Reason]),
            {error, Reason}
    end.

append_to_tfrecord(User, Filename) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            JsonData = serialize_user(User),
            JsonDataWithFilename = JsonData#{<<"filename">> => list_to_binary(Filename)}, 

            {ok, ConnPid} = gun:open("localhost", 8000), 
            {ok, _Protocol} = gun:await_up(ConnPid),

            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:post(ConnPid, "/api/append_user_to_tfrecord", Headers, jsone:encode(JsonDataWithFilename)),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    case file:write_file(Filename, RespBody, [append]) of
                        ok ->
                            io:format("User saved to ~s~n", [Filename]),
                            ok;
                        {error, Reason} ->
                            io:format("Error writing to file ~s: ~p~n", [Filename, Reason]),
                            {error, Reason}
                    end;
                {response, nofin, StatusCode, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    io:format("Error from Python service: ~p ~p~n", [StatusCode, RespBody]),
                    {error, RespBody};
                {error, Reason} ->
                    io:format("Error calling Python service: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Failed to start gun: ~p~n", [Reason]),
            {error, Reason}
    end.