-module(business_server).
-author("Zaryn Technologies").
-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).
-define(SERVER, ?MODULE). 
-record(state, {}).

-export([start_link/0, insert/4, get_business_account_by_business_id/1, get_business_account_by_user_id/1, get_business_account_by_username/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

insert(UserID, CompanyName, Industry, BusinessEmail) ->
    gen_server:call({global, ?MODULE}, {insert, UserID, CompanyName, Industry, BusinessEmail}).

get_business_account_by_business_id(ID) ->
    gen_server:call({global, ?MODULE}, {get_business_account_by_business_id, ID}).

get_business_account_by_user_id(ID) ->
    gen_server:call({global, ?MODULE}, {get_business_account_by_user_id, ID}).

get_business_account_by_username(Username) ->
    gen_server:call({global, ?MODULE}, {get_business_account_by_username, Username}).

init([]) ->
    ?LOG_NOTICE("Business server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({insert, UserID, CompanyName, Industry, BusinessEmail}, _From, State) ->
    Res = businessdb:insert(UserID, CompanyName, Industry, BusinessEmail),
    {reply, Res, State};

handle_call({get_business_account_by_business_id, ID}, _From, State) ->
    Res = businessdb:get_business_account_by_business_id(ID),
    {reply, Res, State};

handle_call({get_business_account_by_user_id, ID}, _From, State) ->
    Res = businessdb:get_business_account_by_user_id(ID),
    {reply, Res, State};

handle_call({get_business_account_by_username, Username}, _From, State) ->
    Res = businessdb:get_business_account_by_username(Username),
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