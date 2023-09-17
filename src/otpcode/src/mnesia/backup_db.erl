%%%-------------------------------------------------------------------
%%% @author dathuynh
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2023 10:28 PM
%%%-------------------------------------------------------------------
-module(backup_db).
-compile(export_all).
-author("dathuynh").

-behaviour(gen_server).

-define(BACKUP_DIR, <<"/tmp/mnesia/">>).
-define(BACKUP_TIME, {23, 59, 59}).
-define(BACKUP_EXTENSION, <<".bak">>).
-define(BACKUP_RETRIES, 3).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(backup_state, {timer, date}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #backup_state{}} | {ok, State :: #backup_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {Date, _} = calendar:universal_time(),
  {ok, #backup_state{date = Date,
                     timer = trigger_backup()}
  }.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #backup_state{}) ->
  {reply, Reply :: term(), NewState :: #backup_state{}} |
  {reply, Reply :: term(), NewState :: #backup_state{}, timeout() | hibernate} |
  {noreply, NewState :: #backup_state{}} |
  {noreply, NewState :: #backup_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #backup_state{}} |
  {stop, Reason :: term(), NewState :: #backup_state{}}).
handle_call(_Request, _From, State = #backup_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #backup_state{}) ->
  {noreply, NewState :: #backup_state{}} |
  {noreply, NewState :: #backup_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #backup_state{}}).
handle_cast(_Request, State = #backup_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #backup_state{}) ->
  {noreply, NewState :: #backup_state{}} |
  {noreply, NewState :: #backup_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #backup_state{}}).
handle_info({timeout, TimeRef, trigger_backup},
            #backup_state{
              date = Date,
              timer = TimeRef} = State) ->
   case check_backup_dir(?BACKUP_DIR) of
        true ->
          Path = create_backup_name(Date),
          run_backup_retires(Path);
       false ->
          ok = maybe_create_dir(?BACKUP_DIR),
          Path = create_backup_name(Date),
          run_backup_retires(Path)
   end,

  {NewDate, _} = calendar:universal_time(),
  {noreply, #backup_state{date = NewDate,
                          timer = trigger_backup()}}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #backup_state{}) -> term()).
terminate(_Reason, _State = #backup_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #backup_state{},
    Extra :: term()) ->
  {ok, NewState :: #backup_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #backup_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_backup_dir(Dir) ->
  case filelib:is_dir(Dir) of
    {ok, _} -> true;
    _ -> false
  end.

trigger_backup() ->
  {_, Time} = calendar:universal_time(),
  AwaitingTime = calendar:time_to_seconds(?BACKUP_TIME) - calendar:time_to_seconds(Time),
  erlang:start_timer(AwaitingTime, self(), trigger_backup).

maybe_create_dir(Dir) ->
  case file:make_dir(Dir) of
    ok -> ok;
    Reason -> {error, Reason}
  end.

create_backup_name(Date) ->
  [Year, Month, Day] = [integer_to_binary(X) || X <- erlang:tuple_to_list(Date)],
  <<?BACKUP_DIR/binary, Year/binary, "_", Month/binary, "_", Day/binary, ?BACKUP_EXTENSION/binary>>.

run_backup_retires(Path) ->
  run_backup_retries(Path, ?BACKUP_RETRIES).

run_backup_retries(Path, 1) ->
  mnesia:backup(Path);
run_backup_retries(Path, Retries) ->
  case mnesia:backup(Path) of
    {error, _Reason} ->
      run_backup_retries(Path, Retries - 1);
    ok -> ok
  end.

