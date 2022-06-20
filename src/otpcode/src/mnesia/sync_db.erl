%%%-------------------------------------------------------------------
%%% @author dhuynh
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(sync_db).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(syncdb_state, {conn, attributes}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Table) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Table], []).

init([Table]) ->
  io:format("Start db_sync"),
  %%TODO: the database credentials should be dynamic configured
  {ok, Conn} = epgsql:connect(#{
      host => "localhost",
      username => "postgres",
      password => "postgres",
      database => "mazaryn_dev",
      timeout => 4000
  }),
  %%  create table if it does not exist yet
  QueryStr = make_table_query(Table),
  {ok, _, _} = epgsql:squery(Conn, QueryStr),

  Attrs = mnesia:table_info(Table, attributes),
  {ok, _} = mnesia:subscribe({table, Table, detailed}),
  {ok, #syncdb_state{conn=Conn, attributes = Attrs}}.

handle_call(_Request, _From, _State) ->
  {reply, ok, _State}.

handle_cast(_Request, _State) ->
  {noreply, _State}.

%% handle mnesia activities
handle_info({mnesia_table_event, Event}, #syncdb_state{conn = Conn}=State) ->
  case Event of
    {Operation, schema, Table, _Old, _Activity} -> % schema activity
      ok;

    %% record and table have same name
    {delete, Table, {Record, Key}, _OldRecord, _ActivityId} ->
      QueryStr = make_delete_query(Record, Key),
      {ok, _} = epgsql:squery(Conn, QueryStr);

    {_Operation, Table, NewRec, OldRecs, _ActivityId} ->
          case OldRecs of
            [] -> %% new one is inserted
              %% run insert query here
              QueryStr = make_insert_query(NewRec),
              {ok, _} = epgsql:squery(Conn, QueryStr);
            [OldRec] ->
              QueryStr = make_update_query(NewRec, OldRec), %% run update query
              {ok, _} = epgsql:squery(Conn, QueryStr)
          end
  end,
  {noreply, State};

handle_info(_Info, _State) ->
  {noreply, _State}.

terminate(_Reason, _State = #syncdb_state{}) ->
  ok.

code_change(_OldVsn, State = #syncdb_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
make_table_query(user) ->
%%  set all columns in text type
%%  for easy synchronize data type
  "CREATE TABLE IF NOT EXISTS users (
     username TEXT PRIMARY KEY,
	   password TEXT NOT NULL,
	   email TEXT UNIQUE NOT NULL,
	   following TEXT,
	   follower TEXT ,
	   blocking TEXT,
	   saved_posts TEXT,
	   other_info TEXT,
	   private TEXT,
	   last_login TIMESTAMP WITH TIME ZONE,
	   date_crated TIMESTAMP WITH TIME ZONE,
	   date_updated TIMESTAMP WITH TIME ZONE)".

generate_partial_query({Type, Values}) ->
    MapValue =
      case Type of
        fields ->
            string:join(Values, ",");
        values ->
          map_postgres_value(Values)
      end,
  add_trim(["(", ")"], MapValue).

make_insert_query(Record) ->
  [Table|Values] = tuple_to_list(Record),
  TextValues = map_postgres_value(Values),
  "INSERT INTO " ++ get_table_name(Table) ++ " VALUES " ++ add_trim(["(", ")"], TextValues).

make_delete_query(Table, Username) ->
  "DELETE FROM " ++ get_table_name(Table) ++
  " WHERE " ++ get_table_key(Table) ++
  "=" ++ make_postges_text(standardize_helper(Username)).

make_update_query(NewRecord, OldRecord) ->
  [Table | NewValue ] = tuple_to_list(NewRecord),
  [Username|_] = NewValue,
  [_ | OldValue ] = tuple_to_list(OldRecord),
  Attrs = mnesia:table_info(user, attributes),
  Lists = lists:zip3(NewValue, OldValue, Attrs),
  UpdatedThings = [{standardize_helper(New), standardize_helper(Name)} || {New, Old, Name} <- Lists, New =/= Old],
  UpdatedFields = [X || {_, X} <- UpdatedThings],
  UpdatedValues = [X || {X, _} <- UpdatedThings],
  "UPDATE " ++ get_table_name(Table) ++ " SET " ++ generate_partial_query({fields, UpdatedFields}) ++
    " = " ++ generate_partial_query({values, UpdatedValues}) ++
      "WHERE username = " ++ make_postges_text(standardize_helper(Username)).

standardize_values(Values) ->
  [standardize_helper(X) || X <- Values].

standardize_helper([]) -> "null";
standardize_helper(undefined) -> "null";
standardize_helper({{_, _, _}, { _, _, _}} = DateTime) ->
  core_util:parse_utc_str_time(DateTime);
standardize_helper(X) when is_binary(X) -> erlang:binary_to_list(X);
standardize_helper(X) when is_atom(X) -> erlang:atom_to_list(X);
standardize_helper(X) when is_list(X) ->
  %% TODO: need to check elements type of X in elixr vs erlang
  X1 = lists:map(fun(Elem) when is_binary(Elem) -> binary_to_list(Elem);
                    (Elem) -> Elem
                 end, X),
  try
      X2 = string:join(X1, ","),  % X1 is a list of string
      X2
  catch
      error:_Error  ->
        X1
  end;
standardize_helper(X) -> X.

map_postgres_value(List) ->
  StandardValues = standardize_values(List),
  MapValues = [case X of
                 "null" -> "null";
                 X -> make_postges_text(X)
               end || X <- StandardValues],
  string:join(MapValues, ",").

make_postges_text(X) -> add_trim(["'","'"], X).

%% add_trim(["'", "'"], "12345") -> '12345'.
add_trim(X, Trim) -> string:join(X, Trim).

get_table_name(user) -> "users".
get_table_key(user) -> "username".
