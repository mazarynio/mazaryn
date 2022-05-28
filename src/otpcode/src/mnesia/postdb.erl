-module(postdb).
-export([insert/2, get_post_by_id/1,
         get_posts_by_author/1,delete_post/1,add_comment/3, get_posts/0,
         get_all_posts_from_date/4, get_all_posts_from_month/3,
         get_comments/1]).
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

insert(Author, Content) ->
    Id = id_gen:generate(),
    Post = #post{id = Id, content = Content,
                 author=Author, date_created = calendar:universal_time()},
    F = fun() ->
        mnesia:write(Post)
    end,
    {atomic, _} = mnesia:transaction(F),
    Id.

get_post_by_id(Id) ->
    {atomic, [Post]} = mnesia:transaction(fun() -> mnesia:read({post, Id}) end),
    Post.

get_posts_by_author(Author) ->
    Fun = fun() ->
            mnesia:match_object(#post{author = Author,
                                       _ = '_'})
            end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

delete_post(Id) ->
    F = fun() ->
            mnesia:delete({post, Id})
        end,
    mnesia:activity(transaction, F).

%%% comments are store in descending order
add_comment(Id, Username, Comment) ->
    Fun = fun() ->
            [Post] = mnesia:read(post, Id),
            Comments = Post#post.comments,
            mnesia:write(Post#post{comments = [{Username, Comment, calendar:universal_time()}| Comments]})
          end,
    mnesia:transaction(Fun).

get_posts() ->
    Fun = fun() ->
            mnesia:foldl(fun(Post, Acc) ->
                            [Post#post.id|Acc]
                         end, [], post)
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

get_comments(Id) ->
    Post = get_post_by_id(Id),
    lists:reverse(Post#post.comments).