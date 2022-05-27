-module(crypto_utils).
-export([generate_key_pair/0, generate_key_pair_256k1/0]).

generate_key_pair() ->
  crypto:generate_key(ecdh, getEcdhParams521r1()).

generate_key_pair_256k1() ->
  crypto:generate_key(ecdh, getEcdhParams256k1()).

getEcdhParams521r1() -> crypto:ec_curve(secp521r1).
getEcdhParams256k1() -> crypto:ec_curve(secp256k1).





