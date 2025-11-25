-module(ai_videodb).
-author("Zaryn Technologies").
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("kernel/include/file.hrl").

-export([insert/1, insert_in_transaction/1]).

insert(VideoID) ->
    Fun = fun() ->
        insert_in_transaction(VideoID)
    end,

    case mnesia:transaction(Fun) of
        {atomic, ID} -> ID;
        {aborted, Reason} ->
            error_logger:error_msg("Failed to insert AI video: ~p", [Reason]),
            {error, Reason}
    end.

insert_in_transaction(VideoID) ->
    ID = nanoid:gen(),
    Now = calendar:universal_time(),
    AI_Video = #ai_video{
        id = ID,
        video_id = VideoID,
        date_created = Now
    },
    mnesia:write(AI_Video),
    ID.
