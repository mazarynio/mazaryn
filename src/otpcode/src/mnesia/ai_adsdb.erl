-module(ai_adsdb).
-author("Zaryn Technologies").
-include("../records.hrl").
-export([insert/1, get_ai_ads_by_ai_id/1, get_ai_ads_by_ads_id/1]).

insert(Ads_ID) ->
    Fun = fun() ->
        ID = key_guardian:gen_address(80),
        AI_Ads = #ai_ads{
            id = ID,
            ads_id = Ads_ID
        },
        mnesia:write(AI_Ads),
        ID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_ai_ads_by_ai_id(ID) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:match_object(#ai_ads{id = ID, _= '_'})
        end),
    case Res of
      {atomic, []} -> ai_ads_not_exist;
      {atomic, [Ads]} -> Ads;
      _ -> error
end.

get_ai_ads_by_ads_id(ID) ->
    Ads = adsdb:get_ads_by_ads_id(ID),
    AI_Ads_ID = Ads#ads.ai_ads_id,
    AI_Ads = get_ai_ads_by_ai_id(AI_Ads_ID),
    AI_Ads.