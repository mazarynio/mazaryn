-module(blog_server).
-author("Zaryn Technologies").

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

%%API
-export([start_link/0, insert/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

insert(Author, Content, _Media) ->
    gen_server:call({global, ?MODULE}, {insert, Author, Content, _Media}).

init([]) ->
    ?LOG_NOTICE("Blog server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({insert, Author, Content, _Media}, _From, State) ->
    Id = blogdb:insert(Author, Content, _Media),
    {reply, Id, State};

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