-module(user_test).
-export([insert_test/0, login_test/0]).

-include_lib("eunit/include/eunit.hrl").

insert_test() ->
	userdb:insert("hello", "pass", "hello@gmail.com").

login_test() ->
	userdb:login("hello@gmail.com", "pass").