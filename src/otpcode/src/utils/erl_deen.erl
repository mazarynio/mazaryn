-module(erl_deen).
-export([main/1, main_string/1]).

main(Data) ->
    X = base64:encode(Data),
    base64:decode(X).

main_string(Data) ->
    X = base64:encode_to_string(Data),
    base64:decode_to_string(X).