-module(user_statem).
-author("Zaryn Technologies").
-export([start_link/0, user_status/1]).
-export([init/1, terminate/3, code_change/4]).
-record(state, {}).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

user_status(UserID) ->
    gen_statem:call(?MODULE, {user_status, UserID}).

init([]) ->
    State = #state{},
    {ok, initial, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
