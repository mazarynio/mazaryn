-module(sui_wallet_server).
-author("Zaryn Technologies").
-behaviour(gen_server).
-include("../records.hrl").
-include("../wallet_records.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(POOL_NAME, sui_wallet_pool).
-define(POOL_SIZE, 50).
-define(POOL_MAX_OVERFLOW, 20).
-define(HTTP_TIMEOUT, 30000).
-define(DEFAULT_NETWORK, <<"mainnet">>).

-record(state, {
    pool_pid,
    base_url,
    network
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    BaseUrl = application:get_env(otpcode, sui_api_url, "http://localhost:3001"),
    Network = application:get_env(otpcode, sui_network, "mainnet"),
    PoolArgs = [
        {name, {local, ?POOL_NAME}},
        {worker_module, sui_wallet_worker},
        {size, ?POOL_SIZE},
        {max_overflow, ?POOL_MAX_OVERFLOW}
    ],
    WorkerArgs = [{base_url, BaseUrl}, {network, Network}],
    case poolboy:start_link(PoolArgs, WorkerArgs) of
        {ok, PoolPid} ->
            error_logger:info_msg("Sui wallet server has been started - ~p~n", [self()]),
            {ok, #state{pool_pid = PoolPid, base_url = BaseUrl, network = Network}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
