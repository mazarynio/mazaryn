-module(libp2pdb).
-author("Zaryn Technologies").
-export([serialize_user/1, save_to_parquet/2, create_node/1]).
-include("../records.hrl").

create_node(Address) ->
    Date = calendar:universal_time(),
    P2PNode = #p2p_node{
            address = Address,
            date_created = Date 
        },
    Serialized = serialize_user(P2PNode),
    Saved = save_to_parquet(Serialized, "amin.parquet"),
    Saved.


serialize_user({p2p_node, Address, DateCreated, _Data}) ->

    {{Year, Month, Day}, {Hour, Minute, Second}} = DateCreated,
    TimestampStr = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Year, Month, Day, Hour, Minute, Second]),

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

    #{
        <<"address">> => list_to_binary(ToString(Address)),
        <<"date_created">> => list_to_binary(ToString(TimestampStr))
    }.

is_valid_utf8(Binary) ->
    case unicode:characters_to_list(Binary, utf8) of
        {incomplete, _, _} -> false;
        {error, _, _} -> false;
        _ -> true
    end.

save_to_parquet(Libp2p, Filename) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            JsonData = serialize_user(Libp2p),
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
