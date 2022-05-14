%%%-------------------------------------------------------------------
%% @doc otpcode top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(otpcode_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 50,
                 period => 1},
    ChildSpecs = [
                  #{id => event_server,
                    start => {event_server, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [event_server]},

                  #{id => user_server,
                    start => {user_server, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [user_server]}
                  ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions