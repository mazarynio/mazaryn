-module(kad_server).
-author("Zaryn Technologies").
-export([start_link/0]).
-export([init/1]).
-include_lib("kernel/include/logger.hrl").
-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?LOG_NOTICE("Kademlia server has been started - ~p", [self()]),
    {ok, #state{}}.