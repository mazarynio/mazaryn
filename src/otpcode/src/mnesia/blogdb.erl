-module(blogdb).
-author("Zaryn Technologies").
-export([insert/3, delete_post/1, get_post/1, add_comment/3, update_comment/2,
get_single_comment/1, get_all_comments/1, delete_comment/2]).

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

insert(Author, Content, _Media) -> 
    F = fun() ->
        Id = nanoid:gen(),
        mnesia:write(#blog_post{id = Id,
                                content = Content,
                                author = Author,
                                media = _Media,
                                date_created = calendar:universal_time()}),
        [User] = mnesia:index_read(user, Author, username),
        Posts = User#user.blog_post,
        mnesia:write(User#user{blog_post = [Id | Posts]}),
        Id
    end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

%% delete_post(Blog_post_id)
delete_post(PostID) ->
    Fun = fun() ->
        mnesia:delete({blog_post, PostID})
    end,
    mnesia:activity(transaction, Fun).

%% get_post(Blog_post_id)
get_post(PostID) ->
    Fun = fun() ->
            [Post] = mnesia:read({blog_post, PostID}),
            Post 
      end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

add_comment(Author, PostID, Content) -> 
    Fun = fun() ->
                Id = nanoid:gen(),
                mnesia:write(#blog_comment{id = Id,
                                    blog_post = PostID,
                                    author = Author,
                                    content = Content,
                                    date_created = calendar:universal_time()}),
                [BlogPost] = mnesia:read({blog_post, PostID}),
                Comments = BlogPost#blog_post.comments,
                mnesia:write(BlogPost#blog_post{comments = [Id|Comments]}),
                Id
            end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

update_comment(CommentID, NewContent) ->
    Fun = fun() ->
              [Comment] = mnesia:read({blog_comment, CommentID}),
              mnesia:write(Comment#blog_comment{content = NewContent}),
              CommentID
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_single_comment(CommentId) ->
    Fun = fun() ->
              [Comment] = mnesia:read({blog_comment, CommentId}),
              Comment
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_all_comments(PostId) ->
    Fun = fun() ->
                mnesia:match_object(#blog_comment{blog_post = PostId,
                                              _ = '_'}),
                [BlogPost] = mnesia:read({blog_post, PostId}),
                lists:foldl(fun(Id, Acc) ->
                                [Comment] = mnesia:read({blog_comment, Id}),
                                [Comment|Acc]
                            end,
                            [], BlogPost#blog_post.comments)
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

delete_comment(CommentID, PostId) ->
    Fun = fun() -> 
                [BlogPost] = mnesia:read(blog_post, PostId),
                Update = lists:delete(CommentID, BlogPost#blog_post.comments),
                mnesia:write(BlogPost#blog_post{comments = Update,
                                       date_created = calendar:universal_time()})
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.