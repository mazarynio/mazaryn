-module(otpcode_sup).

-include("records.hrl").
-include("kademlia/kademlia.hrl").
-include("supervisor.hrl").

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).
-define(MNESIA_DIR, "Mnesia/").
-define(TABLES, [post, notif, user, blog_post, comment, blog_comment, like, chat, media, report, knode]).

%% API
start_link() ->
    case initialize() of
        ok ->
            supervisor:start_link({local, ?SERVER}, ?MODULE, []);
        {error, Reason} ->
            {error, Reason}
    end.

%% Supervisor callbacks
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 50,
        period => 1
    },
    ChildSpecs = [
        ?USER_SERVER,
        ?USER_LEVEL_SERVER,
        ?POST_SERVER,
        ?TOKEN_SERVER,
        ?CHAT_SERVER,
        ?BLOG_SERVER,
        ?MEDIA_SERVER,
        ?NOTIF_EVENT
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% Internal functions
initialize() ->
    try
        logger:info("Initializing otpcode application..."),
        ok = set_mnesia_dir(),
        ok = create_mnesia_schema(),
        ok = start_mnesia(),
        ok = change_table_copy_type(),
        ok = start_ssl(),
        ok = start_inets(),
        ok = create_tables(),
        ok = wait_for_tables(),
        Tables = mnesia:system_info(tables),
        logger:info("Existing tables before index creation: ~p", [Tables]),
        ok = create_table_indexes(),
        logger:info("otpcode application initialized successfully."),
        ok
    catch
        error:{Error, Reason} ->
            logger:error("Initialization failed: ~p - ~p", [Error, Reason]),
            {error, {Error, Reason}}
    end.

set_mnesia_dir() ->
    application:set_env(mnesia, dir, ?MNESIA_DIR).

create_mnesia_schema() ->
    case mnesia:create_schema([node()]) of
        ok -> ok;
        {error, {_, {already_exists, _}}} -> 
            logger:info("Mnesia schema already exists."),
            ok;
        {error, Reason} -> error({create_schema_failed, Reason})
    end.

start_mnesia() ->
    application:start(mnesia).

change_table_copy_type() ->
    case mnesia:table_info(schema, storage_type) of
        disc_copies -> ok;
        _ ->
            case mnesia:change_table_copy_type(schema, node(), disc_copies) of
                {atomic, ok} -> ok;
                {aborted, Reason} -> error({change_table_copy_type_failed, Reason})
            end
    end.

start_ssl() ->
    application:ensure_started(ssl).

start_inets() ->
    application:ensure_started(inets).

create_tables() ->
    Results = [create_table(Table) || Table <- ?TABLES],
    case lists:all(fun(Result) -> Result == ok end, Results) of
        true -> ok;
        false -> 
            logger:info("Some tables already exist or failed to be created: ~p", [Results]),
            ok
    end.

create_table(Table) ->
    Attributes = table_attributes(Table),
    case mnesia:create_table(Table, [
        {attributes, Attributes},
        {disc_copies, [node()]},
        {type, ordered_set}
    ]) of
        {atomic, ok} -> 
            logger:info("Table ~p created successfully", [Table]),
            ok;
        {aborted, {already_exists, _}} -> 
            logger:info("Table ~p already exists", [Table]),
            ok;
        {aborted, Reason} -> 
            logger:error("Failed to create table ~p: ~p", [Table, Reason]),
            {error, {create_table_failed, Table, Reason}}
    end.

wait_for_tables() ->
    case mnesia:wait_for_tables(?TABLES, 5000) of
        ok -> 
            logger:info("All tables are ready"),
            ok;
        {timeout, BadTables} ->
            logger:error("Timeout waiting for tables: ~p", [BadTables]),
            {error, {tables_not_ready, BadTables}};
        {error, Reason} ->
            logger:error("Error waiting for tables: ~p", [Reason]),
            {error, {wait_for_tables_failed, Reason}}
    end.

create_table_indexes() ->
    IndexesToCreate = [{user, username}, {user, email}],
    Results = [create_index(Table, Field) || {Table, Field} <- IndexesToCreate],
    case lists:all(fun(Result) -> Result == ok end, Results) of
        true -> ok;
        false -> 
            logger:info("Some indexes already exist or couldn't be created: ~p", [Results]),
            ok
    end.

create_index(Table, Field) ->
    case mnesia:table_info(Table, attributes) of
        undefined ->
            logger:error("Table ~p does not exist. Cannot create index on ~p", [Table, Field]),
            {error, {table_not_found, Table}};
        Attributes ->
            case lists:member(Field, Attributes) of
                true ->
                    case mnesia:add_table_index(Table, Field) of
                        {atomic, ok} -> 
                            logger:info("Index created on ~p.~p", [Table, Field]),
                            ok;
                        {aborted, {already_exists, Table, _}} -> 
                            logger:info("Index on ~p.~p already exists", [Table, Field]),
                            ok;
                        {aborted, Reason} -> 
                            logger:error("Failed to create index on ~p.~p: ~p", [Table, Field, Reason]),
                            {error, {create_index_failed, Table, Field, Reason}}
                    end;
                false ->
                    logger:error("Field ~p does not exist in table ~p", [Field, Table]),
                    {error, {field_not_found, Table, Field}}
            end
    end.

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