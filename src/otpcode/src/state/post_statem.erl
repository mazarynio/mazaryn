-module(post_statem).
-author("Zaryn Technologies").
-export([start_link/0, archive_post/1, edit_post/2]).
-export([init/1, terminate/3, code_change/4]).
-record(state, {}).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

archive_post(PostID) ->
    gen_statem:call(?MODULE, {archive_post, PostID}).

edit_post(PostID, NewContent) ->
    gen_statem:call(?MODULE, {edit_post, PostID, NewContent}).

init([]) ->
    State = #state{},
    {ok, initial, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.