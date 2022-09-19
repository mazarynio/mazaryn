-module(msgdb).
-compile([export_all, nowarn_export_all]).
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").


get_message_by_id(Id) ->
  Res = mnesia:transaction(
          fun() ->
              mnesia:match_object(#message{id = Id, _= '_'})
          end),
  case Res of
    {atomic, []} -> message_not_exist;
    {atomic, [Message]} -> Message;
    _ -> error
  end.
