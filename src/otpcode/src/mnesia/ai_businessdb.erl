-module(ai_businessdb).
-author("Zaryn Technologies").
-include("../records.hrl"). 
-export([insert/1, get_ai_business_by_ai_id/1, get_ai_business_by_business_id/1]).

insert(BusinessID) ->
    Fun = fun() ->
        ID = key_guardian:gen_address(80),
        AI_Business = #ai_business{
            id = ID,
            business_id = BusinessID
        },
        mnesia:write(AI_Business),
        ID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_ai_business_by_ai_id(ID) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:match_object(#ai_business{id = ID, _= '_'})
        end),
    case Res of
      {atomic, []} -> ai_business_not_exist;
      {atomic, [Business]} -> Business;
      _ -> error
end.

get_ai_business_by_business_id(ID) ->
    Business = businessdb:get_business_account_by_business_id(ID),
    AI_Business_ID = Business#business.ai_business_id,
    AI_Business = get_ai_business_by_ai_id(AI_Business_ID),
    AI_Business.