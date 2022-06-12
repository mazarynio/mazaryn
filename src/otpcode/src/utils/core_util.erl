%%%-------------------------------------------------------------------
%%% @author tandathuynh148@gmail.com
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. May 2022 9:04 PM
%%%-------------------------------------------------------------------
-module(core_util).
-author("tandathuynh148@gmail.com").

%% API
-export([read_all/1,
         clear_table/1,
         parse_utc_str_time/1
         ]).

%% Traverse the selected table from mnesia:info()
read_all(Table) ->
  Fun = fun() ->
            mnesia:foldl(fun(Row, Acc) ->
                            [Row|Acc]
                         end, [], Table)
          end,
    {atomic, Res} = mnesia:transaction(Fun),
  Res.

clear_table(Table) ->
  mnesia:clear_table(Table).

parse_utc_str_time({{Y,M,D},{ H,MM,SS}} = _DateTime) ->
  lists:flatten(io_lib:format(
    "~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B+00", [Y, M, D,H,MM,SS])).