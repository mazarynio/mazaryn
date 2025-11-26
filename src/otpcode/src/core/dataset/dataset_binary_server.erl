-module(dataset_binary_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_dataset_binary/2, cleanup_old_binaries/0]).

-record(state, {
    binaries = #{}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:send_after(300000, self(), cleanup),
    {ok, #state{}}.

get_dataset_binary(DatasetId, UserId) ->
    gen_server:call(?MODULE, {get_binary, DatasetId, UserId}, 60000).

cleanup_old_binaries() ->
    gen_server:cast(?MODULE, cleanup_old).

handle_call({get_binary, DatasetId, UserId}, _From, State) ->
    case datasetdb:get_dataset_zip_by_id(DatasetId, UserId) of
        {ok, Binary} when is_binary(Binary) ->
            {reply, {ok, Binary}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State};
        Other ->
            {reply, {error, {unexpected_result, Other}}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(cleanup_old, State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    erlang:send_after(300000, self(), cleanup),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
