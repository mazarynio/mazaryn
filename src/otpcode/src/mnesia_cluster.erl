-module(mnesia_cluster).

-export([
    setup_cluster/0,
    start_node/2,
    join_cluster/1,
    check_cluster_status/0,
    add_table_replicas/0,
    fragment_tables/0
]).

-define(CLUSTER_NODES, ['mazaryn@node1', 'mazaryn@node2', 'mazaryn@node3']).
-define(HIGH_WRITE_TABLES, [post, chat, media, pin_info, presence]).
-define(FRAGMENT_TABLES, [post, media]).

setup_cluster() ->
    logger:info("Setting up Mnesia cluster with nodes: ~p", [?CLUSTER_NODES]),
    
    case lists:member(node(), ?CLUSTER_NODES) of
        true -> ok;
        false -> 
            logger:error("Current node ~p is not in the cluster configuration ~p", 
                        [node(), ?CLUSTER_NODES]),
            {error, node_not_in_cluster}
    end,
    
    create_schema(),
    
    [rpc:call(Node, mnesia, start, []) || Node <- ?CLUSTER_NODES],
    
    timer:sleep(1000),
    
    add_table_replicas(),
    
    fragment_tables(),
    
    Tables = mnesia:system_info(tables),
    mnesia:wait_for_tables(Tables, 10000),
    
    logger:info("Mnesia cluster setup complete. Running on nodes: ~p", 
               [mnesia:system_info(running_db_nodes)]),
    ok.

start_node(NodeName, Cookie) ->
    erlang:set_cookie(node(), Cookie),
    
    net_kernel:start([NodeName, shortnames]),
    
    [net_kernel:connect_node(Node) || Node <- ?CLUSTER_NODES],
    
    join_cluster(?CLUSTER_NODES -- [node()]).

join_cluster(ExistingNodes) ->
    logger:info("Joining Mnesia cluster with existing nodes: ~p", [ExistingNodes]),
    
    ConnectedNode = connect_to_cluster(ExistingNodes),
    
    case ConnectedNode of
        {ok, Node} ->
            logger:info("Connected to cluster node: ~p", [Node]),
            
            {ok, [Node]} = mnesia:change_config(extra_db_nodes, [Node]),
            
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            
            add_table_copies(),
            
            Tables = mnesia:system_info(tables),
            mnesia:wait_for_tables(Tables, 10000),
            
            logger:info("Successfully joined Mnesia cluster"),
            ok;
        {error, Reason} ->
            logger:error("Failed to join cluster: ~p", [Reason]),
            {error, Reason}
    end.

check_cluster_status() ->
    RunningNodes = mnesia:system_info(running_db_nodes),
    AllNodes = mnesia:system_info(db_nodes),
    Tables = mnesia:system_info(tables) -- [schema],
    
    logger:info("Mnesia cluster status:"),
    logger:info("  Running nodes: ~p", [RunningNodes]),
    logger:info("  Configured nodes: ~p", [AllNodes]),
    logger:info("  Tables: ~p", [Tables]),
    
    [check_table_status(Table) || Table <- Tables],
    
    OfflineNodes = AllNodes -- RunningNodes,
    case OfflineNodes of
        [] -> 
            logger:info("All nodes are online");
        _ -> 
            logger:warning("Offline nodes: ~p", [OfflineNodes])
    end,
    
    #{
        running_nodes => RunningNodes,
        configured_nodes => AllNodes,
        tables => Tables,
        offline_nodes => OfflineNodes
    }.

create_schema() ->
    case mnesia:create_schema(?CLUSTER_NODES) of
        ok -> ok;
        {error, {_, {already_exists, _}}} -> 
            logger:info("Mnesia schema already exists"),
            ok;
        {error, Reason} ->
            logger:error("Failed to create schema: ~p", [Reason]),
            {error, Reason}
    end.

connect_to_cluster([]) ->
    {error, no_available_nodes};
connect_to_cluster([Node | Rest]) ->
    case net_kernel:connect_node(Node) of
        true -> {ok, Node};
        false -> connect_to_cluster(Rest)
    end.

add_table_copies() ->
    Tables = mnesia:system_info(tables) -- [schema],
    [add_table_copy(Table) || Table <- Tables],
    ok.

add_table_copy(Table) ->
    StorageType = disc_copies,
    case mnesia:add_table_copy(Table, node(), StorageType) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _, _}} -> ok;
        {aborted, Reason} ->
            logger:error("Failed to add table copy for ~p: ~p", [Table, Reason]),
            {error, Reason}
    end.

add_table_replicas() ->
    [distribute_high_write_table(Table) || Table <- ?HIGH_WRITE_TABLES],
    
    RegularTables = mnesia:system_info(tables) -- [schema] -- ?HIGH_WRITE_TABLES,
    [ensure_table_on_all_nodes(Table) || Table <- RegularTables],
    
    ok.

distribute_high_write_table(Table) ->
    [PrimaryNode | SecondaryNodes] = ?CLUSTER_NODES,
    
    Attrs = otpcode_sup:table_attributes(Table),
    case mnesia:create_table(Table, [
        {attributes, Attrs},
        {ram_copies, [PrimaryNode]},
        {type, ordered_set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> ok;
        {aborted, Reason} ->
            logger:error("Failed to create high-write table ~p: ~p", [Table, Reason]),
            {error, Reason}
    end,
    
    [mnesia:add_table_copy(Table, Node, disc_copies) || Node <- SecondaryNodes],
    
    ok.

ensure_table_on_all_nodes(Table) ->
    Attrs = otpcode_sup:table_attributes(Table),
    case mnesia:create_table(Table, [
        {attributes, Attrs},
        {disc_copies, ?CLUSTER_NODES},
        {type, ordered_set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> 
            [mnesia:add_table_copy(Table, Node, disc_copies) || 
             Node <- ?CLUSTER_NODES],
            ok;
        {aborted, Reason} ->
            logger:error("Failed to create table ~p: ~p", [Table, Reason]),
            {error, Reason}
    end.

fragment_tables() ->
    [fragment_table(Table) || Table <- ?FRAGMENT_TABLES],
    ok.

fragment_table(Table) ->
    FragmentedTable = list_to_atom(atom_to_list(Table) ++ "_frag"),
    Attrs = otpcode_sup:table_attributes(Table),
    
    case mnesia:create_table(FragmentedTable, [
        {attributes, Attrs},
        {disc_copies, ?CLUSTER_NODES},
        {type, ordered_set},
        {frag_properties, [
            {node_pool, ?CLUSTER_NODES},
            {n_fragments, length(?CLUSTER_NODES) * 3},  
            {n_disc_copies, 2}  
        ]}
    ]) of
        {atomic, ok} -> 
            logger:info("Created fragmented table ~p", [FragmentedTable]),
            ok;
        {aborted, {already_exists, _}} -> 
            logger:info("Fragmented table ~p already exists", [FragmentedTable]),
            ok;
        {aborted, Reason} ->
            logger:error("Failed to create fragmented table ~p: ~p", [FragmentedTable, Reason]),
            {error, Reason}
    end.

check_table_status(Table) ->
    CopyHolders = mnesia:table_info(Table, all_nodes),
    CopyTypes = [{Node, mnesia:table_info(Table, storage_type)} || Node <- CopyHolders, mnesia:table_info(Table, where_to_read) == Node],
    Size = mnesia:table_info(Table, size),
    
    logger:info("Table ~p:", [Table]),
    logger:info("  Size: ~p records", [Size]),
    logger:info("  Copies: ~p", [CopyTypes]),
    
    {Table, #{
        size => Size,
        copies => CopyTypes
    }}.