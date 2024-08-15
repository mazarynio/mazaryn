-module(adsdb).
-author("Zaryn Technologies").
-include("../records.hrl").
-export([insert/2, get_ads_by_ads_id/1, get_ads_by_user_id/1, get_ads_by_username/1]).

insert(UserID, Ad_type) ->
    Fun = fun() ->
        ID = nanoid:gen(),
        AI_Ads_ID = ai_adsdb:insert(ID),
        Now = calendar:universal_time(),
        Ads = #ads{
            id = ID,
            user_id = UserID,
            ai_ads_id = AI_Ads_ID,
            ad_type = Ad_type,
            date_created = Now
        },
        mnesia:write(Ads),
        ID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_ads_by_ads_id(ID) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:match_object(#ads{id = ID, _= '_'})
        end),
    case Res of
      {atomic, []} -> ads_not_exist;
      {atomic, [Ads]} -> Ads;
      _ -> error
end.

get_ads_by_user_id(ID) ->
    User = userdb:get_user_by_id(ID),
    Ads_ID = User#user.ads_id,
    Ads = get_ads_by_ads_id(Ads_ID),
    Ads.

get_ads_by_username(Username) ->
    User = userdb:get_user(Username),
    Ads_ID = User#user.ads_id,
    Ads = get_ads_by_ads_id(Ads_ID),
    Ads.
