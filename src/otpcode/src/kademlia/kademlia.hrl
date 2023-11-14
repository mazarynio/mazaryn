-record(k_node, {
    id, % 160-bit Number
    ip,
    port,
    distance
}).
-record(k_bucket, {
    k_nodes = []
}).
-record(routing_table, {
    k_node,
    k_buckets = []
}).

