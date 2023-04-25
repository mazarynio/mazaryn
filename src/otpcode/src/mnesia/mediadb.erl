-module(mediadb).
-author("Zaryn Technologies").
-export([insert_music/2, insert_video/2]).

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

insert_music(Username, Single) ->
    Fun = fun() ->
            Id = nanoid:gen(),
            mnesia:write(#media{music = #music{id = Id,
                                               single = Single,
                                               date_created = calendar:universal_time()}}),
            [User] = mnesia:index_read(user, Username, username),
            Music = User#user.media,
            mnesia:write(User#user{media = [Id, Music]}),
            Id
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

insert_video(Username, Single) ->
    Fun = fun() ->
            Id = nanoid:gen(),
            mnesia:write(#media{video = #video{id = Id,
                                               single = Single,
                                               date_created = calendar:universal_time()}}),
            [User] = mnesia:index_read(user, Username, username),
            Video = User#user.media,
            mnesia:write(User#user{media = [Id, Video]}),
            Id 
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.
