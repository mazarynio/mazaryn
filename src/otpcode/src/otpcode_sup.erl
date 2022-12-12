%%%-------------------------------------------------------------------
%% @doc otpcode top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(otpcode_sup).

-include("records.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    % create schema and directory
    application:set_env(mnesia, dir, "Mnesia/"),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:change_table_copy_type(schema, node(), disc_copies),

    %% enable httpc
    ssl:start(),
    application:start(inets),
    %% create ETS table to store user_reg process
    ets:new(user_reg, [set, named_table, public]),

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

    mnesia:create_table(message, [{attributes, record_info(fields, message)},
                              {disc_copies, [node()]},
                              {type, ordered_set}]),

    mnesia:create_table(post, [{attributes, record_info(fields, post)},
                               {disc_copies, [node()]},
                               {type, ordered_set}]),

    mnesia:create_table(user, [{attributes, record_info(fields, user)},
                               {disc_copies, [node()]},
                               {type, ordered_set}]),

    % create index key for user -> username 
    mnesia:add_table_index(user, username)

    mnesia:create_table(comment, [{attributes, record_info(fields, comment)},
                               {disc_copies, [node()]},
                               {type, ordered_set}]),

    mnesia:create_table(wallet, [{attributes, record_info(fields, wallet)},
                                 {disc_copies, [node()]},
                                 {type, ordered_set}]),

    mnesia:create_table(ae_wallet, [{attributes, record_info(fields, ae_wallet)},
                                    {disc_copies, [node()]},
                                    {type, ordered_set}]),

    mnesia:create_table(hed_wallet, [{attributes, record_info(fields, hed_wallet)},
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
                    modules => [post_server]},

                  #{id => msg_server,
                    start => {msg_server, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [msg_server]},

                  #{id => wallet_server,
                    start => {wallet_server, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [wallet_server]},

                  #{id => ae_wallet_server,
                    start => {ae_wallet_server, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [ae_wallet_server]},

                  #{id => token_server,
                  start => {token_server, start_link, []},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [token_server]},

                  #{id => hedera_wallet_server,
                  start => {hedera_wallet_server, start_link, []},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [hedera_wallet_server]}

                  ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
