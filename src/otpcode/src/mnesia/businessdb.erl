-module(businessdb).
-author("Zaryn Technologies").
-include("../records.hrl").
-export([insert/1, get_business_account_by_business_id/1, get_business_account_by_user_id/1, get_business_account_by_username/1]).

insert(UserID) ->
    Fun = fun() ->
        ID = nanoid:gen(),
        AI_Business_ID = ai_businessdb:insert(ID),
        Business = #business{
            id = ID,
            user_id = UserID,
            ai_business_id = AI_Business_ID
        },
        mnesia:write(Business),
        ID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_business_account_by_business_id(ID) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:match_object(#business{id = ID, _= '_'})
        end),
    case Res of
      {atomic, []} -> business_account_not_exist;
      {atomic, [BusinessAccount]} -> BusinessAccount;
      _ -> error
end.

get_business_account_by_user_id(ID) ->
    User = userdb:get_user_by_id(ID),
    Business_ID = User#user.business_id,
    Business_Account = get_business_account_by_business_id(Business_ID),
    Business_Account.

get_business_account_by_username(Username) ->
    User = userdb:get_user(Username),
    Business_ID = User#user.business_id,
    Business_Account = get_business_account_by_business_id(Business_ID),
    Business_Account.