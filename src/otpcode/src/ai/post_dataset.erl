-module(post_dataset).
-author("Zaryn Technologies").
-export([serialize_post/1, save_to_tfrecord/2, save_to_csv/2, save_to_json/2, save_to_parquet/2]).

-include_lib("kernel/include/logger.hrl").
-include("../records.hrl").

serialize_post({post, Id, AiPostId, Content, Emoji, Comments, Likes, Media, Hashtag, Mention, LinkUrl, Author, _Other, DateCreated, _DateUpdated, _Report, _DeviceInfo, _Data}) ->
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
        <<"ai_post_id">> => list_to_binary(ToString(AiPostId)),
        <<"content">> => list_to_binary(ToString(Content)),
        <<"emoji">> => list_to_binary(ToString(Emoji)),
        <<"comments">> => list_to_binary(ToString(Comments)),
        <<"likes">> => list_to_binary(ToString(Likes)),
        <<"media">> => list_to_binary(ToString(Media)),
        <<"hashtag">> => list_to_binary(ToString(Hashtag)),
        <<"mention">> => list_to_binary(ToString(Mention)),
        <<"link_url">> => list_to_binary(ToString(LinkUrl)),
        <<"author">> => list_to_binary(ToString(Author)),
        <<"date_created">> => list_to_binary(ToString(TimestampStr))
    }.

is_valid_utf8(Binary) ->
    case unicode:characters_to_list(Binary, utf8) of
        {incomplete, _, _} -> false;
        {error, _, _} -> false;
        _ -> true
    end.

save_to_csv(Post, Filename) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            JsonData = serialize_post(Post),
            JsonDataWithFilename = JsonData#{<<"filename">> => list_to_binary(Filename)}, 

            {ok, ConnPid} = gun:open("localhost", 8000), 
            {ok, _Protocol} = gun:await_up(ConnPid),

            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:post(ConnPid, "/api/convert_post_to_csv", Headers, jsone:encode(JsonDataWithFilename)),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    case file:write_file(Filename, RespBody) of
                        ok ->
                            io:format("Post saved to ~s~n", [Filename]),
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

save_to_tfrecord(Post, Filename) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            JsonData = serialize_post(Post),
            JsonDataWithFilename = JsonData#{<<"filename">> => list_to_binary(Filename)}, 

            {ok, ConnPid} = gun:open("localhost", 8000), 
            {ok, _Protocol} = gun:await_up(ConnPid),

            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:post(ConnPid, "/api/convert_post_to_tfrecord", Headers, jsone:encode(JsonDataWithFilename)),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    case file:write_file(Filename, RespBody, [append]) of
                        ok ->
                            io:format("Post saved to ~s~n", [Filename]),
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

save_to_json(Post, Filename) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            JsonData = serialize_post(Post),
            JsonDataWithFilename = JsonData#{<<"filename">> => list_to_binary(Filename)}, 

            {ok, ConnPid} = gun:open("localhost", 8000), 
            {ok, _Protocol} = gun:await_up(ConnPid),

            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:post(ConnPid, "/api/convert_post_to_json", Headers, jsone:encode(JsonDataWithFilename)),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    case file:write_file(Filename, RespBody) of
                        ok ->
                            io:format("Post saved to ~s~n", [Filename]),
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

save_to_parquet(Post, Filename) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            JsonData = serialize_post(Post),
            JsonDataWithFilename = JsonData#{<<"filename">> => list_to_binary(Filename)}, 

            {ok, ConnPid} = gun:open("localhost", 8000), 
            {ok, _Protocol} = gun:await_up(ConnPid),

            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:post(ConnPid, "/api/convert_post_to_parquet", Headers, jsone:encode(JsonDataWithFilename)),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    case file:write_file(Filename, RespBody) of
                        ok ->
                            io:format("Post saved to ~s~n", [Filename]),
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