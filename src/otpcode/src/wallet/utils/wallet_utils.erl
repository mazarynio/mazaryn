-module(wallet_utils).

-export([entropy_to_mnemonic/1,
	 generate_mnemonic/0,
	 mnemonic_to_entropy/1,
	 mnemonic_to_seed/1,
	 mnemonic_to_seed/2,
	 prf/2,
	 validate_mnemonic/1,
	 word_list/0]).

-define(HLEN, (512 / 8)). %% sha512
-define(MAX_DERIVED_KEY_LENGTH, ((1 bsl 32 - 1) * ?HLEN)).

word_list() ->
    {ok, Binary} = file:read_file("./src/english.txt"),
    binary:split(Binary, <<"\n">>, [global, trim]).


generate_mnemonic() ->
    Entropy = crypto:strong_rand_bytes(16),
    entropy_to_mnemonic(Entropy).


entropy_to_mnemonic(Entropy) when size(Entropy) >= 16 andalso
				  size(Entropy) =< 32 ->
    WordList = word_list(),
    Checksum = derive_checksum(Entropy),
    entropy_to_mnemonic(<<Entropy/binary,Checksum/bitstring>>, WordList, []).

entropy_to_mnemonic(<<>>, _, Acc) ->
    lists:reverse(Acc);
entropy_to_mnemonic(<<I:11,B/bitstring>>, WordList, Acc) ->
    entropy_to_mnemonic(B, WordList, [lists:nth(I + 1, WordList)|Acc]).

derive_checksum(Entropy) ->
    ENT = size(Entropy) * 8,
    CS = ENT div 32,
    <<Checksum:CS/bitstring,_/bitstring>> = crypto:hash(sha256, Entropy),
    Checksum.
    
    
mnemonic_to_entropy(Mnemonic) ->
    B = mnemonic_to_binary(Mnemonic),
    ENT = bit_size(B) div 33 * 32,
    <<Entropy:ENT/bitstring, Checksum/bitstring>> = B,
    case Checksum =:= derive_checksum(Entropy) of
    true ->
        {ok, Entropy};
    false ->
        {error, invalid_checksum}
    end.
    
    
mnemonic_to_binary(Mnemonic) ->
    WordList = word_list(),
    WordMap = maps:from_list(lists:zip(WordList, lists:seq(0, length(WordList) - 1))),
    mnemonic_to_binary(Mnemonic, WordMap,<<>>).
    %%
mnemonic_to_binary([], _, Acc) ->
    Acc;
mnemonic_to_binary([H|T], WordMap, Acc) ->
    Index = maps:get(H,WordMap),
    mnemonic_to_binary(T, WordMap, <<Acc/bitstring,Index:11>>).
    
    
validate_mnemonic(Mnemonic) ->
    case mnemonic_to_entropy(Mnemonic) of
    {ok, _} ->
        true;
    {error, _} ->
        false
    end.
    
    
mnemonic_to_seed(Mnemonic) ->
    mnemonic_to_seed(Mnemonic, <<>>).
    
%% To create a binary seed from the mnemonic, we use the PBKDF2 function
%% with a mnemonic sentence (in UTF-8 NFKD) used as the password and the
%% string "mnemonic" + passphrase (again in UTF-8 NFKD) used as the salt.
%% The iteration count is set to 2048 and HMAC-SHA512 is used as the
%% pseudo-random function. The length of the derived key is 512 bits
%% (= 64 bytes).
%%
%% This seed can be later used to generate deterministic wallets using
%% BIP-0032 or similar methods.
mnemonic_to_seed(Mnemonic0, Passphrase) ->
    %% English words are already in UTF-8 NFKD
    Mnemonic = intersperse(<<" ">>, Mnemonic0),
    Salt = unicode:characters_to_nfkd_binary(<<"mnemonic",Passphrase/binary>>),
    pbkdf2(Mnemonic, Salt, 64).


intersperse(Sep, L) ->
    intersperse(Sep, L, <<>>).
%%
intersperse(_Sep, [], Acc) ->
    Acc;
intersperse(Sep, [H], Acc) ->
    intersperse(Sep, [], <<Acc/binary,H/binary>>);
intersperse(Sep, [H|T], Acc) ->
    intersperse(Sep, T, <<Acc/binary,H/binary,Sep/binary>>).


pbkdf2(_Password, _Salt, DKLen) when DKLen > ?MAX_DERIVED_KEY_LENGTH ->
    {error, derived_key_too_long};
pbkdf2(Password, Salt, DKLen) ->
    Bin = pbkdf2(fun prf/2, Password, Salt, 2048, DKLen, 1, <<>>),
    {ok, Bin}.
%%
pbkdf2(_Prf, _Password, _Salt, _Count, DKLen, _BlockIndex, Acc)
  when byte_size(Acc) > DKLen ->
    <<Bin:DKLen/binary, _/binary>> = Acc,
    Bin;
pbkdf2(Prf, Password, Salt, Count, DKLen, BlockIndex, Acc) ->
    Block = block(Prf, Password, Salt, Count, BlockIndex),
    pbkdf2(Prf, Password, Salt, Count, DKLen, BlockIndex + 1, <<Acc/binary,Block/binary>>).


block(Prf, Password, Salt, Count, BlockIndex) ->
    block(Prf, Password, Salt, Count, BlockIndex, 1, <<>>, <<>>).
%%
block(_Prf, _Password, _Salt, Count, _BlockIndex, Iteration, _Prev, Acc)
  when Iteration > Count ->
    Acc;
block(Prf, Password, Salt, Count, BlockIndex, 1, _Prev, _Acc) ->
    InitialBlock = Prf(Password, <<Salt/binary, BlockIndex:32/unsigned-big-integer>>),
    block(Prf, Password, Salt, Count, BlockIndex, 2, InitialBlock, InitialBlock);
block(Prf, Password, Salt, Count, BlockIndex, Iteration, Prev, Acc) ->
    Next = Prf(Password, Prev),
    block(Prf, Password, Salt, Count, BlockIndex, Iteration + 1, Next, crypto:exor(Next, Acc)).


prf(Key, Data) ->
    crypto:mac(sha512, Key, Data).  