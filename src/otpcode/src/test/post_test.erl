-module(post_test).
-export([insert_test/0, insert_test2/0, get_post_by_id_test/1, get_posts_by_author_test/1,
    get_posts_by_hashtag_test/1, update_post_test/2, delete_post_test/1, get_posts_test/0]).

-include_lib("eunit/include/eunit.hrl"). 

insert_test() ->
	postdb:insert("hello", "My_first_post", "#mazaryn").

insert_test2() ->
	postdb:insert("hello", "my_second_post", "#mazaryn").

get_post_by_id_test(Id) ->
	postdb:get_post_by_id(Id).

get_posts_by_author_test(Author) ->
	postdb:get_posts_by_author(Author).

get_posts_by_hashtag_test(Hashtag) ->
	postdb:get_posts_by_hashtag(Hashtag).

update_post_test(PostId, NewContent) ->
	postdb:update_post(PostId, NewContent).

delete_post_test(Id) ->
	postdb:delete_post(Id).

get_posts_test() ->
	postdb:get_posts().