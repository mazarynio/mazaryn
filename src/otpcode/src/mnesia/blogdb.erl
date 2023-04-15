-module(blogdb).
-author("Zaryn Technologies").
-export([insert/3]).

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