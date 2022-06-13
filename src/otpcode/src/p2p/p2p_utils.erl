-module(p2p_utils).
-export([generate_id/0]).

generate_id() ->
    crypto:strong_rand_bytes(20).