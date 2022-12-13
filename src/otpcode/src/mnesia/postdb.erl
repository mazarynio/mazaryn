-module(postdb).
-export([insert/4, get_post_by_id/1,
         modify_post/4, get_posts_by_author/1, 
         get_posts_by_hashtag/1, update_post/2,
         delete_post/1, get_posts/0,
         get_all_posts_from_date/4, get_all_posts_from_month/3,
         add_comment/3, update_comment/2,
         get_all_comments/1, get_single_comment/1]).

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% if post or comment do not have media,
%% their value in record are nil

insert(Author, Content, Media, Hashtag) ->
    F = fun() ->
          Id = id_gen:generate(),
          mnesia:write(#post{id=Id,
                             content=Content,
                             author=Author,
                             media = Media,
                             hashtag = Hashtag,
                             date_created = calendar:universal_time()}),
          [User] = mnesia:index_read(user, Author, username),
          Posts = User#user.post,
          mnesia:write(User#user{post = [Id | Posts]}),
          Id
    end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

modify_post(Author, NewContent, NewMedia, NewHashtag) ->
  Fun = fun() ->
          [Post] = mnesia:read({post, Author}),
          mnesia:write(Post#post{content = NewContent,
                                 media = NewMedia,
                                 hashtag = NewHashtag,
                                 date_updated = calendar:universal_time()})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_post_by_id(Id) ->
  Fun = fun() ->
          mnesia:match_object(#comment{post = Id,
                                              _ = '_'}),
          [Post] = mnesia:read({post, Id}),
          Comments = lists:foldl(fun(Id, Acc) ->
                                  [Comment] = mnesia:read({comment, Id}),
                                  [Comment|Acc]
                                 end,[], Post#post.comments),
          Post#post{comments = Comments}
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_posts_by_author(Author) ->
    Fun = fun() ->
            mnesia:match_object(#post{author = Author,
                                       _ = '_'})
            end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_posts_by_hashtag(Hashtag) ->
    Fun = fun() ->
            mnesia:match_object(#post{hashtag = Hashtag,
                                      _ = '_'})
            end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

update_post(PostId, NewContent) ->
  Fun = fun() ->
          [Post] = mnesia:read({post, PostId}),
          Content = Post#post.content,
          UpdatedContent =
                lists:foldl(fun({Key, Value}, Acc) ->
                                case lists:keymember(Key, 1, Acc) of
                                  true ->
                                    lists:keyreplace(Key, 1, Acc, {Key, Value});
                                  false ->
                                    [{Key, Value}|Acc]
                                end
                            end, Content, NewContent),
          mnesia:write(Post#post{content = UpdatedContent})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.


delete_post(Id) ->
    F = fun() ->
            mnesia:delete({post, Id})
        end,
    mnesia:activity(transaction, F).

get_posts() ->
    Fun = fun() ->
            mnesia:all_keys(post)
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.


%%% Date in tuple format
%%% {2022,05,01}
%%% User = [], select all users
%%% USer = dat for particular
get_all_posts_from_date(Year, Month, Date, Author) ->
    DateTime = {Year, Month, Date},
    Object =
      case Author of
        [] -> #post{date_created = {DateTime, '_'},
                                        _ = '_'};
        Author -> #post{date_created = {DateTime, '_'},
                                    author = Author,
                                    _ = '_'}
      end,
    {atomic, Res} = mnesia:transaction(fun() -> mnesia:match_object(Object) end),
    Res.

get_all_posts_from_month(Year, Month, Author) ->
    Object =
      case Author of
        [] -> #post{date_created = {{Year, Month, '_'}, '_'},
                                        _ = '_'};
        Author -> #post{date_created = {{Year, Month, '_'}, '_'},
                                    author = Author,
                                    _ = '_'}
      end,
    {atomic, Res} = mnesia:transaction(fun() -> mnesia:match_object(Object) end),
    Res.

%% Content = [{text, Text}, {media, Media}, {mention, Name}, {like, Like}]
add_comment(Author, PostID, Content) ->
  Fun = fun() ->
          Id =id_gen:generate(),
          mnesia:write(#comment{id = Id,
                                post = PostID,
                                author = Author,
                                content = Content,
                                date_created = calendar:universal_time()}),
          [Post] = mnesia:read({post, PostID}),
          Comments = Post#post.comments,
          mnesia:write(Post#post{comments = [Id|Comments]}),
          Id
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

update_comment(CommentID, NewContent) ->
  Fun = fun() ->
          [Comment] = mnesia:read({comment, CommentID}),
          Content = Comment#comment.content,
          UpdatedContent =
                lists:foldl(fun({Key, Value}, Acc) ->
                                case lists:keymember(Key, 1, Acc) of
                                  true ->
                                    lists:keyreplace(Key, 1, Acc, {Key, Value});
                                  false ->
                                    [{Key, Value}|Acc]
                                end
                            end, Content, NewContent),
          mnesia:write(Comment#comment{content = UpdatedContent})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_single_comment(CommentId) ->
  Fun = fun() ->
          [Comment] = mnesia:read({comment, CommentId}),
          Comment
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_all_comments(PostId) ->
  Fun = fun() ->
          mnesia:match_object(#comment{post = PostId,
                                              _ = '_'}),
          [Post] = mnesia:read({post, PostId}),
          lists:foldl(fun(Id, Acc) ->
                        [Comment] = mnesia:read({comment, Id}),
                        [Comment|Acc]
                      end,
                      [], Post#post.comments)

        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_media(Media) ->
  case Media of
    nil -> nil;
    Media -> [Media]  %% media link here
  end.
