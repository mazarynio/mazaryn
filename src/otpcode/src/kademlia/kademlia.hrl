-record(knode, {
    id, % 160-bit Number
    ip,
    port,
    user, 
    distance
}).
-record(kbucket, {
    knodes = [],
    size
}).
-record(routing_table, {
    knode,
    kbuckets = [],
    sender,
    receiver
}).
-record(rpc, {
    socket,
    knode 
}).

