%%%-------------------------------------------------------------------
%% @doc otpcode top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(otpcode_sup).

-behaviour(supervisor).

-include("records.hrl").

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
      % create schema and directory
    application:set_env(mnesia, dir, "Mnesia/"),
    mnesia:create_schema([node()]),
    mnesia:start(),

    % create tables
    mnesia:create_table(event, [{attributes, record_info(fields, event)},
                                {disc_copies, [node()]},
                                {type, ordered_set}]),

    mnesia:create_table(follower, [{attributes, record_info(fields, follower)},
                                    {disc_copies, [node()]},
                                        {type, ordered_set}]),

    mnesia:create_table(group, [{attributes, record_info(fields, group)},
                                {disc_copies, [node()]},
                                {type, ordered_set}]),

    mnesia:create_table(msg, [{attributes, record_info(fields, msg)},
                              {disc_copies, [node()]},
                              {type, ordered_set}]),

    mnesia:create_table(post, [{attributes, record_info(fields, post)},
                               {disc_copies, [node()]},
                               {type, ordered_set}]),

    mnesia:create_table(user, [{attributes, record_info(fields, user)},
                               {disc_copies, [node()]},
                               {type, ordered_set}]),

    mnesia:add_table_index(user, email),

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
                    modules => [user_server]},

                  #{id => post_server,
                    start => {post_server, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [post_server]}
                  ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions