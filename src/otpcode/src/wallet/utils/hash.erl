-module(hash).
-export([hash160/1, hash256/1, hash/1]).

-spec hash160(binary()) -> binary().
hash160(B) -> crypto:hash(ripemd160, crypto:hash(sha256, B)).

-spec hash256(binary()) -> binary().
hash256(B) -> crypto:hash(sha256, crypto:hash(sha256, B)).

hash(Data) ->
  Bin = crypto:hash(sha256, crypto:hash(sha256, Data)),
  base58:binary_to_base58(Bin).