-module(user_level_server).
-author("Zaryn Technologies").
-export([start_link/0, remove_level_one_users/0, downgrade_level_two_users/0, downgrade_level_three_users/0, downgrade_level_four_users/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).
-include_lib("kernel/include/logger.hrl").

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

remove_level_one_users() ->
    gen_server:call({global, ?MODULE}, {remove_level_one_users}).

downgrade_level_two_users() ->
    gen_server:call({global, ?MODULE}, {downgrade_level_two_users}).

downgrade_level_three_users() ->
    gen_server:call({global, ?MODULE}, {downgrade_level_three_users}).

downgrade_level_four_users() ->
    gen_server:call({global, ?MODULE}, {downgrade_level_four_users}).

init([]) ->
    ?LOG_NOTICE("User auto level management has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({remove_level_one_users}, _From, State) ->
    Res = user_level_db:remove_level_one_users(),
    {reply, Res, State};

handle_call({downgrade_level_two_users}, _From, State) ->
    Res = user_level_db:downgrade_level_two_users(),
    {reply, Res, State};

handle_call({downgrade_level_three_users}, _From, State) ->
    Res = user_level_db:downgrade_level_three_users(),
    {reply, Res, State};

handle_call({downgrade_level_four_users}, _From, State) ->
    Res = user_level_db:downgrade_level_four_users(),
    {reply, Res, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.