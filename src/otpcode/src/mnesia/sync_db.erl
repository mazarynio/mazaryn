%%%-------------------------------------------------------------------
%%% @author dhuynh
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2022 8:55 PM
%%%-------------------------------------------------------------------
-module(sync_postgres_db).
-author("dhuynh").

%% API
-export([init/0, my_test/0]).

init() ->
  {ok, Conn} = epgsql:connect(#{
      host => "localhost",
      username => "postgres",
      password => "docker",
      database => "mazaryn_dev",
      timeout => 4000
  }),

  spawn_link(fun() ->
    io:format("Start the subscribe fun~n"),
    subscribe_activities(user, Conn) end).


subscribe_activities(Table, Conn) ->
  Attributes = mnesia:table_info(Table, attributes),
  io:format("Going to create table"),
  {ok, _, _} = epgsql:squery(Conn,
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
	   date_updated TIMESTAMP WITH TIME ZONE );"),
  {ok, _} = mnesia:subscribe({table, Table, detailed}),
  subscribe_handler(Conn).

subscribe_handler(Conn) ->
  %% only subscribe user table for testing
  receive
    {mnesia_table_event, Event} ->
      io:format("Event: ~p ~n", [Event]),
      case Event of
        {Oper, schema, Table, _Old, _Activity} -> % schema activity
          io:format("~p was on ~p ~n", [Oper, Table]);
        {delete, Table, {Record, Key}, OldRecord, ActivityId} ->
          QueryStr = make_delete_query(Record, Key),
          Res = epgsql:squery(Conn, QueryStr),
          io:format("Query result: ~p~n",[Res]);
        {Oper, Table, NewRec, OldRecs, ActivityId} ->
          case OldRecs of
            [] -> %% new one is inserted
              %% run insert query here
              QueryStr = make_insert_query(NewRec),
              Res = epgsql:squery(Conn, QueryStr),
              io:format("Query result: ~p~n",[Res]);
            [OldRec] ->
              QueryStr = make_update_query(NewRec, OldRec), %% run update query
              Res = epgsql:squery(Conn, QueryStr),
              io:format("Query result: ~p~n",[Res])
          end
      end,
      subscribe_handler(Conn)

  end.

my_test() ->
  Table = user,
  Username = "dathuynh148",
  QueryStr = "DELETE FROM " ++ get_table_name(Table) ++
             " WHERE  " ++ get_table_key(Table) ++
             "='" ++ standardize_helper(Username) ++ "'".
%%    {ok, Conn} = epgsql:connect(#{
%%      host => "localhost",
%%      username => "postgres",
%%      password => "docker",
%%      database => "mazaryn_dev",
%%      timeout => 4000
%%  }),
%%  epgsql:squery(Conn, QueryStr).
generate_partial_query({Type, Values}) ->
    MapValue =
      case Type of
        fields ->
            Values;
        values ->
          [ case X of
                 "null" -> "null";
                 X -> "'" ++ X ++ "'"
               end || X <- Values, X =/="nulls"]
      end,

  " (" ++ lists:flatten(lists:join(",", MapValue)) ++ ") ".

make_insert_query(Record) ->
  [Table|Values] = tuple_to_list(Record),
  StandardValues = standardize_values(Values),
  MapValue = [ case X of
                 "null" -> "null";
                 X -> "'" ++ X ++ "'"
               end || X <- StandardValues, X =/="nulls"],
  StrValues = lists:flatten(lists:join(",", MapValue)),
  lists:flatten(string:replace("INSERT INTO " ++ get_table_name(Table) ++ " VALUES (%s)","%s", StrValues)).

make_delete_query(Table, Username) ->
  QueryStr = "DELETE FROM " ++ get_table_name(Table) ++
             " WHERE  " ++ get_table_key(Table) ++
             "='" ++ standardize_helper(Username) ++ "'".

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
      "WHERE username = '" ++ standardize_helper(Username) ++ "'".

standardize_values(Values) ->
  [standardize_helper(X) || X <- Values].

standardize_helper([]) -> "null";
standardize_helper(undefined) -> "null";
standardize_helper({{_, _, _}, { _, _, _}} = DateTime) ->
  core_util:parse_utc_str_time(DateTime);
standardize_helper(X) when is_binary(X) -> erlang:binary_to_list(X);
standardize_helper(X) when is_atom(X) -> erlang:atom_to_list(X);
standardize_helper(X) -> X.

get_table_name(user) -> "users".
get_table_key(user) -> "username".