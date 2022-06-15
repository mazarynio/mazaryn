-record(node, {id, host, port}).

-record(peer, {id, ip, port}).
-type(peer() :: #peer{}).

-record(file, {name, size, info, loc = ""}).