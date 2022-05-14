-module(msgdb).
-compile([export_all, nowarn_export_all]).
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-import(userdb, [get_password/1]).

reset_db() ->
    mnesia:clear_table(msg).

insert_message(Sender, Receiver, Content) ->
    ReceiverExists = case get_password(Receiver) of 
        undefined -> false;
        _         -> true
    end,
    SenderExists = case get_password(Sender) of 
        undefined -> false;
        _         -> true 
    end,
    Msg = #msg{receiver = Receiver, sender = Sender,
                content = Content, timestamp = erlang:system_time()},
    Fun = fun() ->
        mnesia:write(Msg)
    end,
    case ReceiverExists and SenderExists of
        true  -> {atomic, Status} = mnesia:transaction(Fun),
                 Status;
        false -> {error, unknown_user}
    end.
