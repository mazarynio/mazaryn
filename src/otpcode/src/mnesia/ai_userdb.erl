-module(ai_userdb).
-author("Zaryn Technologies").
-include("../records.hrl"). 
-export([insert/1, get_ai_user_by_ai_id/1, get_ai_user_by_user_id/1, get_ai_user_by_username/1]).

insert(UserID) ->
    Fun = fun() ->
        ID = key_guardian:gen_address(80),
        AI_User = #ai_user{
            id = ID,
            user_id = UserID
        },
        mnesia:write(AI_User),
        ID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_ai_user_by_ai_id(ID) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:match_object(#ai_user{id = ID, _= '_'})
        end),
    case Res of
      {atomic, []} -> ai_user_not_exist;
      {atomic, [User]} -> User;
      _ -> error
end.

get_ai_user_by_user_id(ID) ->
    User = userdb:get_user_by_id(ID),
    AI_User_ID = User#user.ai_user_id,
    AI_User = get_ai_user_by_ai_id(AI_User_ID),
    AI_User.

get_ai_user_by_username(Username) ->
    User = userdb:get_user(Username),
    AI_User_ID = User#user.ai_user_id,
    AI_User = get_ai_user_by_ai_id(AI_User_ID),
    AI_User.

