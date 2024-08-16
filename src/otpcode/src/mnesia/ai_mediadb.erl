-module(ai_mediadb).
-author("Zaryn Technologies").
-include("../records.hrl"). 
-export([insert/1, get_ai_media_by_ai_id/1, get_ai_media_by_media_id/1]).

insert(MediaID) ->
    Fun = fun() ->
        ID = key_guardian:gen_address(80),
        AI_Media = #ai_media{
            id = ID,
            media_id = MediaID
        },
        mnesia:write(AI_Media),
        ID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_ai_media_by_ai_id(ID) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:match_object(#ai_media{id = ID, _= '_'})
        end),
    case Res of
      {atomic, []} -> ai_media_not_exist;
      {atomic, [Media]} -> Media;
      _ -> error
end.

get_ai_media_by_media_id(ID) ->
    Media = mediadb:get_media_by_id(ID),
    AI_Media_ID = Media#media.ai_media_id,
    AI_Media = get_ai_media_by_ai_id(AI_Media_ID),
    AI_Media.