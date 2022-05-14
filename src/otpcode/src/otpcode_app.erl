%%%-------------------------------------------------------------------
%% @doc otpcode public API
%% @end
%%%-------------------------------------------------------------------

-module(otpcode_app).

-behaviour(application).

-include("records.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % create schema and directory
    application:set_env(mnesia, dir, "../../Mnesia/"),
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

    otpcode_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
