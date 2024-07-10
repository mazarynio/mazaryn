%%%-------------------------------------------------------------------
%% @doc otpcode top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(otpcode_sup).

-include("records.hrl").
-include("kademlia/kademlia.hrl").
-include("supervisor.hrl").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    % Initialize Mnesia and required applications
    initialize(),

    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 50,
                 period => 1},
    ChildSpecs = [
        ?USER_SERVER,
        ?USER_LEVEL_SERVER,
        ?POST_SERVER,
        ?TOKEN_SERVER,
        ?CHAT_SERVER,
        ?BLOG_SERVER,
        ?MEDIA_SERVER,
        ?NOTIF_EVENT],
    {ok, {SupFlags, ChildSpecs}}.

initialize() ->
    application:set_env(mnesia, dir, "Mnesia/"),
    %
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:change_table_copy_type(schema, node(), disc_copies),

    %% enable httpc
    start_ssl(),
    start_inets(),
    create_tables(),
    create_table_indexes().

start_ssl() ->
  case application:start(ssl) of
        ok -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> {error, {ssl_start_failed, Reason}}
  end.

start_inets() ->
  case application:start(inets) of
        ok -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> {error, {inets_start_failed, Reason}}
  end.

create_tables() ->
  Tables = [post, notif, user, blog_post, comment, blog_comment, like, chat, media, report, knode],
  [create_table(Table) || Table <- Tables].

create_table(Table) ->
  Attributes = table_attributes(Table),
  mnesia:create_table(Table, [
        {attributes, Attributes},
        {disc_copies, [node()]},
        {type, ordered_set}
  ]).

create_table_indexes() ->
  mnesia:add_table_index(user, username),
  mnesia:add_table_index(user, email).

table_attributes(post) -> record_info(fields, post);
table_attributes(notif) -> record_info(fields, notif);
table_attributes(user) -> record_info(fields, user);
table_attributes(blog_post) -> record_info(fields, blog_post);
table_attributes(comment) -> record_info(fields, comment);
table_attributes(blog_comment) -> record_info(fields, blog_comment);
table_attributes(like) -> record_info(fields, like);
table_attributes(chat) -> record_info(fields, chat);
table_attributes(media) -> record_info(fields, media);
table_attributes(report) -> record_info(fields, report);
table_attributes(knode) -> record_info(fields, knode).


