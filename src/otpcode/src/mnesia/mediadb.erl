-module(mediadb).
-author("Zaryn Technologies").
-export([insert_media/2, delete_file/1, get_media/1, get_all_media/1, report_media/4]).

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

insert_media(UserID, File) -> 
    Fun = fun() ->
            Id = nanoid:gen(),
            mnesia:write(#media{id = Id, 
                                user_id = UserID,
                                file = File,
                                date_created = calendar:universal_time()}),
            [User] = mnesia:read({user, UserID}),
            Music = User#user.media,
            mnesia:write(User#user{media = [Id, Music]}),
            Id
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

delete_file(MediaID) -> 
    Fun = fun() ->
      mnesia:delete({media, MediaID})
    end,
    mnesia:activity(transaction, Fun).

get_media(MediaID) -> 
    Fun = fun() ->
            [Media] = mnesia:read({media, MediaID}),
            Media 
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_all_media(UserID) ->
    Fun = fun() ->
            mnesia:match_object(#media{user_id = UserID,
                                                _ = '_'}),
            [User] = mnesia:read({user, UserID}),
            lists:foldl(fun(ID, Acc) ->
                          [Media] = mnesia:read({media, ID}),
                          [Media|Acc]
                        end,
                        [], User#user.media)
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

report_media(MyID, MediaID, Type, Description) ->
  Fun = fun() ->
    ID = nanoid:gen(),
    mnesia:read(media, MediaID),
        Report = #report{
          id = ID,
          type = Type,
          description = Description,
          reporter = MyID,
          media = MediaID,
          date_created = calendar:universal_time()},
        mnesia:write(Report),
        ID
  end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.