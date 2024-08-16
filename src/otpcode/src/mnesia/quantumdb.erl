-module(quantumdb).
-author("Zaryn Technologies").
-include("../records.hrl"). 
-export([insert/1, get_quantum_by_id/1, get_quantum_by_user_id/1, get_quantum_by_username/1]).

insert(UserID) ->
    Fun = fun() ->
        ID = key_guardian:gen_address(80),
        Quantum = #quantum{
            id = ID,
            user_id = UserID
        },
        mnesia:write(Quantum),
        ID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_quantum_by_id(ID) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:match_object(#quantum{id = ID, _= '_'})
        end),
    case Res of
      {atomic, []} -> quantum_not_exist;
      {atomic, [Quantum]} -> Quantum;
      _ -> error
end.

get_quantum_by_user_id(ID) ->
    User = userdb:get_user_by_id(ID),
    Quantum_ID = User#user.quantum_id,
    Quantum = get_quantum_by_id(Quantum_ID),
    Quantum.

get_quantum_by_username(Username) ->
    User = userdb:get_user(Username),
    Quantum_ID = User#user.quantum_id,
    Quantum = get_quantum_by_id(Quantum_ID),
    Quantum.