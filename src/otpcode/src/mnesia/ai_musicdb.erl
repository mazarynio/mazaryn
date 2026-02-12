-module(ai_musicdb).
-author("Zaryn Technologies").

-include("../records.hrl").
-include("../media_records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("kernel/include/file.hrl").

-export([insert/1, insert_in_transaction/1]).

insert(MusicID) ->
    Fun = fun() ->
        insert_in_transaction(MusicID)
    end,

    case mnesia:transaction(Fun) of
        {atomic, ID} ->
            ID;
        {aborted, Reason} ->
            error_logger:error_msg(
                "Failed to insert AI music: ~p",
                [Reason]
            ),
            {error, Reason}
    end.

insert_in_transaction(MusicID) ->
    ID = nanoid:gen(),
    Now = calendar:universal_time(),
    AI_Music = #ai_music{
        id = ID,
        music_id = MusicID,
        date_created = Now
    },
    mnesia:write(AI_Music),
    ID.
