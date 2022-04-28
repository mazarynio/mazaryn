%%%-------------------------------------------------------------------
%% @doc otpcode public API
%% @end
%%%-------------------------------------------------------------------

-module(otpcode_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    otpcode_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
