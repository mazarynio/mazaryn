-module(chat_dataset).
-author("Zaryn Technologies").
-export([serialize_chat/1, save_to_tfrecord/2]).

-include_lib("kernel/include/logger.hrl").
-include("../records.hrl").

serialize_chat({chat, Id, AiChatId, UserId, RecipientId, Body, Media, Bot, DateCreated, DateUpdated, Data}) ->

    {{Year, Month, Day}, {Hour, Minute, Second}} = DateCreated,
    TimestampStr = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Year, Month, Day, Hour, Minute, Second]),

    ToBinary = fun
        (Value) when is_binary(Value) ->
            case is_valid_utf8(Value) of
                true -> Value; 
                false -> base64:encode(Value) 
            end;
        (Value) when is_list(Value) -> list_to_binary(Value); 
        (Value) when is_atom(Value) -> list_to_binary(atom_to_list(Value)); 
        (Value) when is_integer(Value) -> list_to_binary(integer_to_list(Value)); 
        (Value) -> list_to_binary(lists:flatten(io_lib:format("~p", [Value]))) 
    end,

    #{
        <<"id">> => ToBinary(Id),
        <<"ai_chat_id">> => ToBinary(AiChatId),
        <<"user_id">> => ToBinary(UserId),
        <<"recipient_id">> => ToBinary(RecipientId),
        <<"body">> => ToBinary(Body),
        <<"media">> => ToBinary(Media),
        <<"bot">> => ToBinary(Bot),
        <<"date_created">> => list_to_binary(TimestampStr),
        <<"date_updated">> => ToBinary(DateUpdated)
    }.

is_valid_utf8(Binary) ->
    case unicode:characters_to_list(Binary, utf8) of
        {incomplete, _, _} -> false;
        {error, _, _} -> false;
        _ -> true
    end.

save_to_tfrecord(Chat, Filename) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            JsonData = serialize_chat(Chat),
            JsonDataWithFilename = JsonData#{<<"filename">> => list_to_binary(Filename)}, 

            {ok, ConnPid} = gun:open("localhost", 8000), 
            {ok, _Protocol} = gun:await_up(ConnPid),

            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:post(ConnPid, "/api/convert_chat_to_tfrecord", Headers, jsone:encode(JsonDataWithFilename)),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    case file:write_file(Filename, RespBody, [append]) of
                        ok ->
                            io:format("Chat saved to ~s~n", [Filename]),
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