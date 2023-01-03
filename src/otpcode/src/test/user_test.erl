-module(user_test).
-export([insert_test/0, insert_test2/0, login_test/0, get_user_test/0, get_users_test/0, get_password_test/1,
	get_user_by_email_test/0, get_user_by_id_test/1, change_password_test/0, change_email_test/0,
	change_username_test/0, delete_user_test/0, follow_test/2, unfollow_test/2, get_following_test/1,
	get_follower_test/1, block_test/2, unblock_test/2, get_blocked_test/1]).

-include_lib("eunit/include/eunit.hrl").

insert_test() ->
	userdb:insert("hello", "pass", "hello@gmail.com").

insert_test2() ->
	userdb:insert("bye", "pass", "bye@gmail.com").

login_test() ->
	userdb:login("hello@gmail.com", "pass").

get_user_test() ->
	userdb:get_user("hello").

get_users_test() ->
	userdb:get_users().

get_password_test(Id) ->
	userdb:get_password(Id).

get_user_by_email_test() ->
	userdb:get_user_by_email("hello@gmail.com").

get_user_by_id_test(Id) ->
	userdb:get_user_by_id(Id).

change_password_test() ->
	userdb:change_password("hello", "pass", "new_pass").

change_email_test() ->
	userdb:change_email("hello", "pass", "hellooo@gmail.com").

change_username_test() ->
	userdb:change_username("hello", "pass", "hellooo").

delete_user_test() ->
	userdb:delete_user("hello").

follow_test(Id, Following) ->
	userdb:follow(Id, Following).

unfollow_test(Id, Following) ->
	userdb:unfollow(Id, Following).

get_following_test(Id) ->
	userdb:get_following(Id).

get_follower_test(Id) ->
	userdb:get_follower(Id).

block_test(Id, Blocked) ->
	userdb:block(Id, Blocked).

unblock_test(Id, Unblocked) ->
	userdb:unblock(Id, Unblocked).

get_blocked_test(Id) ->
	userdb:get_blocked(Id).