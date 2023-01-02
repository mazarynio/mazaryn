-module(post_test).
-export([insert_test/0]).

-include_lib("eunit/include/eunit.hrl").

insert_test() ->
	postdb:insert("hello", "My_first_post", "#mazaryn").