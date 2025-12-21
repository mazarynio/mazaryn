-module(otpcode_sup).

-include("records.hrl").
-include("ml_records.hrl").
-include("media_records.hrl").
-include("kademlia/kademlia.hrl").
-include("supervisor.hrl").

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_distributed/0, add_node/1, add_extra_nodes/1, create_tables_on_nodes/1]).

-define(SERVER, ?MODULE).
-define(MNESIA_DIR, "Mnesia/").
-define(CLUSTER_NODES, ['social_net@node1', 'social_net@node2', 'social_net@node3']).
-define(TABLES, [post, notif, user, blog_post, comment, blog_comment, like, reply, chat, media, report, knode, business, ads, quantum,
 ai_user, ai_post, ai_chat, ai_media, ai_business, ai_ads, p2p_node,
 pin_info, pin_params, pin_history, bulk_operation, scheduled_job, rate_limiter_usage, pin_info_lookup, pin_health, storage_quota, presence,
 dataset, competition, notebook, model, video, music, album, playlist, ai_video, media_view, livestream]).

%% API
start_link() ->
    case initialize() of
        ok ->
            supervisor:start_link({local, ?SERVER}, ?MODULE, []);
        {error, Reason} ->
            {error, Reason}
    end.

start_distributed() ->
    logger:info("Starting distributed Mnesia with nodes: ~p", [?CLUSTER_NODES]),
    case net_kernel:start([node_name(), shortnames]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> error({net_kernel_start_failed, Reason})
    end,

    [net_kernel:connect_node(Node) || Node <- ?CLUSTER_NODES],

    case initialize() of
        ok ->
            add_extra_nodes(?CLUSTER_NODES),
            create_tables_on_nodes(?CLUSTER_NODES),
            ok;
        Error -> Error
    end.

node_name() ->
    case node() of
        nonode@nohost ->
            list_to_atom("social_net_" ++ integer_to_list(erlang:system_time(millisecond)) ++ "@localhost");
        Name -> Name
    end.

add_node(Node) ->
    logger:info("Adding node ~p to Mnesia schema", [Node]),
    case mnesia:change_config(extra_db_nodes, [Node]) of
        {ok, [Node]} ->
            mnesia:change_table_copy_type(schema, Node, disc_copies),
            ok;
        {ok, []} ->
            logger:warning("Node ~p could not be added to Mnesia", [Node]),
            {error, node_not_added};
        {error, Reason} ->
            logger:error("Failed to add node: ~p", [Reason]),
            {error, Reason}
    end.

add_extra_nodes(Nodes) ->
    logger:info("Adding nodes ~p to Mnesia schema", [Nodes]),
    Results = [add_node(Node) || Node <- Nodes, Node =/= node()],
    case lists:all(fun(Result) -> Result == ok end, Results) of
        true -> ok;
        false -> {error, some_nodes_not_added}
    end.

create_tables_on_nodes(Nodes) ->
    logger:info("Creating tables on nodes: ~p", [Nodes]),
    Results = [create_table_on_nodes(Table, Nodes) || Table <- ?TABLES],
    case lists:all(fun(Result) -> Result == ok end, Results) of
        true -> ok;
        false -> {error, some_tables_not_created}
    end.

create_table_on_nodes(Table, Nodes) ->
    Attributes = table_attributes(Table),
    TableType = table_type(Table),
    TableNodes = [node() | [N || N <- Nodes, N =/= node()]],

    TableInfo = mnesia:table_info(Table, all),
    case TableInfo of
        [{aborted, no_exists}] ->
            case mnesia:create_table(Table, [
                {attributes, Attributes},
                {disc_copies, TableNodes},
                {type, TableType}
            ]) of
                {atomic, ok} ->
                    logger:info("Table ~p created successfully on nodes ~p with type ~p", [Table, TableNodes, TableType]),
                    ok;
                {aborted, Reason} ->
                    logger:error("Failed to create table ~p: ~p", [Table, Reason]),
                    {error, {create_table_failed, Table, Reason}}
            end;
        _ ->
            [add_table_copy(Table, Node) || Node <- TableNodes],
            ok
    end.

add_table_copy(Table, Node) ->
    case mnesia:add_table_copy(Table, Node, disc_copies) of
        {atomic, ok} ->
            logger:info("Added table ~p to node ~p", [Table, Node]),
            ok;
        {aborted, {already_exists, Table, Node}} ->
            logger:info("Table ~p already exists on node ~p", [Table, Node]),
            ok;
        {aborted, Reason} ->
            logger:error("Failed to add table ~p to node ~p: ~p", [Table, Node, Reason]),
            {error, {add_table_copy_failed, Table, Node, Reason}}
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
        ?NOTIF_EVENT,
        ?BUSINESS_SERVER,
        ?QUANTUM_SERVER,
        ?ADS_SERVER,
        ?AI_USER_SERVER,
        ?AI_POST_SERVER,
        ?AI_CHAT_SERVER,
        ?AI_MEDIA_SERVER,
        ?AI_BUSINESS_SERVER,
        ?AI_ADS_SERVER,
        ?PIN_POST_SERVER,
        ?STORAGE_QUOTA_SERVER,
        ?RATE_LIMITER_SERVER,
        ?CONTENT_CACHE
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% Internal functions
initialize() ->
    try
        logger:info("Initializing distributed otpcode application..."),
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
    TableType = table_type(Table),
    case mnesia:create_table(Table, [
        {attributes, Attributes},
        {disc_copies, [node()]},
        {type, TableType}
    ]) of
        {atomic, ok} ->
            logger:info("Table ~p created successfully with type ~p", [Table, TableType]),
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

table_type(post) -> ordered_set;
table_type(blog_post) -> ordered_set;
table_type(_) -> set.

table_attributes(post) -> record_info(fields, post);
table_attributes(notif) -> record_info(fields, notif);
table_attributes(user) -> record_info(fields, user);
table_attributes(blog_post) -> record_info(fields, blog_post);
table_attributes(comment) -> record_info(fields, comment);
table_attributes(blog_comment) -> record_info(fields, blog_comment);
table_attributes(like) -> record_info(fields, like);
table_attributes(reply) -> record_info(fields, reply);
table_attributes(chat) -> record_info(fields, chat);
table_attributes(media) -> record_info(fields, media);
table_attributes(report) -> record_info(fields, report);
table_attributes(knode) -> record_info(fields, knode);
table_attributes(business) -> record_info(fields, business);
table_attributes(ads) -> record_info(fields, ads);
table_attributes(quantum) -> record_info(fields, quantum);
table_attributes(ai_user) -> record_info(fields, ai_user);
table_attributes(ai_post) -> record_info(fields, ai_post);
table_attributes(ai_chat) -> record_info(fields, ai_chat);
table_attributes(ai_media) -> record_info(fields, ai_media);
table_attributes(ai_business) -> record_info(fields, ai_business);
table_attributes(ai_ads) -> record_info(fields, ai_ads);
table_attributes(p2p_node) -> record_info(fields, p2p_node);
table_attributes(pin_info) -> record_info(fields, pin_info);
table_attributes(pin_params) -> record_info(fields, pin_params);
table_attributes(pin_history) -> record_info(fields, pin_history);
table_attributes(bulk_operation) -> record_info(fields, bulk_operation);
table_attributes(scheduled_job) -> record_info(fields, scheduled_job);
table_attributes(rate_limiter_usage) -> record_info(fields, rate_limiter_usage);
table_attributes(pin_info_lookup) -> record_info(fields, pin_info_lookup);
table_attributes(pin_health) -> record_info(fields, pin_health);
table_attributes(storage_quota) -> record_info(fields, storage_quota);
table_attributes(presence) -> record_info(fields, presence);
table_attributes(dataset) -> record_info(fields, dataset);
table_attributes(competition) -> record_info(fields, competition);
table_attributes(notebook) -> record_info(fields, notebook);
table_attributes(model) -> record_info(fields, model);
table_attributes(video) -> record_info(fields, video);
table_attributes(music) -> record_info(fields, music);
table_attributes(album) -> record_info(fields, album);
table_attributes(playlist) -> record_info(fields, playlist);
table_attributes(ai_video) -> record_info(fields, ai_video);
table_attributes(media_view) -> record_info(fields, media_view);
table_attributes(livestream) -> record_info(fields, livestream).
