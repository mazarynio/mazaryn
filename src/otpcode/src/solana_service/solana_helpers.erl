-module(solana_helpers).
-author("Zaryn Technologies").

-export([
    unwrap/1,
    unwrap/2,
    get_token/1,
    get_user_id/1,
    print_result/1,
    print_result/2
]).

unwrap({ok, Value}) ->
    Value;
unwrap({error, Reason}) ->
    erlang:error({unwrap_failed, Reason}).

unwrap({ok, Value}, _Default) ->
    Value;
unwrap({error, _Reason}, Default) ->
    Default.

get_token({ok, AuthResult}) ->
    maps:get(token, AuthResult);
get_token(AuthResult) when is_map(AuthResult) ->
    maps:get(token, AuthResult);
get_token({error, Reason}) ->
    erlang:error({get_token_failed, Reason}).

get_user_id({ok, AuthResult}) ->
    maps:get(user_id, AuthResult);
get_user_id(AuthResult) when is_map(AuthResult) ->
    maps:get(user_id, AuthResult);
get_user_id({error, Reason}) ->
    erlang:error({get_user_id_failed, Reason}).

print_result({ok, Result}) ->
    io:format("✅ Success: ~p~n", [Result]),
    {ok, Result};
print_result({error, Reason}) ->
    io:format("❌ Error: ~p~n", [Reason]),
    {error, Reason}.

print_result(Label, {ok, Result}) ->
    io:format("✅ ~s: ~p~n", [Label, Result]),
    {ok, Result};
print_result(Label, {error, Reason}) ->
    io:format("❌ ~s failed: ~p~n", [Label, Reason]),
    {error, Reason}.
