-module (b58).
-author ("Mazaryn").

-export ([decode/1]).
-export ([encode/1]).

-spec decode( list() | binary()) -> binary().
decode(List) when is_list(List) ->
  base58:base58_to_binary(List);  
decode(Bin) when is_binary(Bin) ->
  base58:base58_to_binary(binary_to_list(Bin)).

-spec encode(list() | binary()) -> binary().
encode(List) when is_list(List) ->
  list_to_binary(base58:binary_to_base58(list_to_binary(List)));
encode(Bin) when is_binary(Bin) ->
  list_to_binary(base58:binary_to_base58(Bin)).