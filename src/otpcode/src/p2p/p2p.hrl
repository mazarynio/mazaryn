-record(node, {id, host, port}).

-record(peer, {id, ip, port}).

-record(file, {name, size, info, loc = ""}).