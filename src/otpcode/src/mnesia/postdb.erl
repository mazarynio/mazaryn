-module(postdb).
-compile([export_all, nowarn_export_all]).
-include("../include/records.hrl").
-include_lib("stdlib/include/qlc.hrl").

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(post, [{attributes, record_info(fields, post)},
            {disc_copies, [node()]}]).

reset_db() ->
    mnesia:clear_table(post).

insert(Content) ->
    Id = id_gen:generate(),
    Post = #post{id = Id, content = Content},
    F = fun() ->
        mnesia:write(Post)
    end,
    mnesia:transaction(F).


get_post(Id) ->
    {atomic, [Post]} = mnesia:transaction(fun() -> mnesia:read({post, Id}) end),
    Post.

get_posts() ->
    F = fun() ->
        Q = qlc:q([{P#post.content}
                || P <- mnesia:table(post)]),
        qlc:e(Q)
        end,
    mnesia:transaction(F).



delete_post(Content) ->
    mnesia:delete_table(Content),
    F = fun() ->
        mnesia:delete({post, Content})
    end,
    mnesia:activity(transaction, F).

add_comment(Id, Username, Content) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        [Post] = mnesia:wread({post, Id}),
        NewComment = #comment{username = Username, content = Content},
        NewComments = [NewComment | Post#post.comments],
        mnesia:write(Post#post{comments = NewComments})
    end).
    
search_user(Content) ->
    mnesia:dirty_read(post, Content).