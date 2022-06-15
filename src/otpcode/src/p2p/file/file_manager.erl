-module(file_manager).
-export([start_link/0, new_file/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

-include("../p2p.hrl").

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

new_file(Name, Size, Loc) ->
    gen_server:call({global, ?MODULE}, {new_file, {Name, Size, Loc}}).

init([]) ->
    {ok, #state{}}.

handle_call({new_file, Name, Size, Loc}, _From, State = #state{}) ->
    Res = generate_file(Name, Size, Loc),
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

generate_file(Name, Size, Loc) ->
    ok = file:write_file(Loc ++"/"++ Name, <<>>),
    {ok, IoDevice} = file:open(Loc ++ Name, [raw, write]),
    {ok, _} = file:position(IoDevice, Size-1),
    ok = file:write(IoDevice, <<0>>),
    ok = file:close(IoDevice),
    ok.


