-module(solana_crypto).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").
-export([
    generate_keypair/0,
    generate_keypair_from_seed/1,
    derive_public_key/1,
    encrypt_private_key/2,
    decrypt_private_key/4,
    sign_transaction/2,
    verify_signature/3,
    generate_seed/0,
    derive_keypair_from_mnemonic/2,
    encrypt_with_user_password/2,
    decrypt_with_user_password/3,
    hash_password/1,
    verify_password/2,
    base58_encode/1,
    base58_decode/1
]).

-define(ED25519_PUBLIC_KEY_BYTES, 32).
-define(ED25519_SECRET_KEY_BYTES, 64).
-define(ED25519_SEED_BYTES, 32).

generate_keypair() ->
    Seed = crypto:strong_rand_bytes(?ED25519_SEED_BYTES),
    generate_keypair_from_seed(Seed).

generate_keypair_from_seed(Seed) when byte_size(Seed) =:= ?ED25519_SEED_BYTES ->
    {PublicKey, PrivateKey} = crypto:generate_key(eddsa, ed25519, Seed),
    #{
        public_key => base58_encode(PublicKey),
        private_key => base58_encode(PrivateKey),
        seed => base58_encode(Seed)
    };
generate_keypair_from_seed(_) ->
    {error, invalid_seed_length}.

derive_public_key(PrivateKeyBase58) ->
    try
        PrivateKey = base58_decode(PrivateKeyBase58),
        PublicKey = binary:part(PrivateKey, byte_size(PrivateKey) - 32, 32),
        {ok, base58_encode(PublicKey)}
    catch
        _:_ -> {error, invalid_private_key}
    end.

generate_seed() ->
    crypto:strong_rand_bytes(?ED25519_SEED_BYTES).

derive_keypair_from_mnemonic(Mnemonic, DerivationPath) when is_list(Mnemonic) ->
    derive_keypair_from_mnemonic(list_to_binary(Mnemonic), DerivationPath);
derive_keypair_from_mnemonic(Mnemonic, DerivationPath) when is_list(DerivationPath) ->
    derive_keypair_from_mnemonic(Mnemonic, list_to_binary(DerivationPath));
derive_keypair_from_mnemonic(Mnemonic, DerivationPath) ->
    Salt = <<"mnemonic", DerivationPath/binary>>,
    Seed = crypto:pbkdf2_hmac(sha512, Mnemonic, Salt, 2048, ?ED25519_SEED_BYTES),
    generate_keypair_from_seed(Seed).

encrypt_with_user_password(PrivateKey, Password) when is_list(Password) ->
    encrypt_with_user_password(PrivateKey, list_to_binary(Password));
encrypt_with_user_password(PrivateKey, Password) when is_list(PrivateKey) ->
    encrypt_with_user_password(list_to_binary(PrivateKey), Password);
encrypt_with_user_password(PrivateKey, Password) ->
    Salt = crypto:strong_rand_bytes(32),
    EncryptionKey = crypto:pbkdf2_hmac(sha256, Password, Salt, 100000, 32),
    IV = crypto:strong_rand_bytes(16),
    {Ciphertext, AuthTag} = crypto:crypto_one_time_aead(
        aes_256_gcm,
        EncryptionKey,
        IV,
        PrivateKey,
        <<>>,
        true
    ),
    #{
        encrypted_private_key => base64:encode(Ciphertext),
        encryption_iv => base64:encode(IV),
        encryption_auth_tag => base64:encode(AuthTag),
        encryption_salt => base64:encode(Salt)
    }.

decrypt_with_user_password(EncryptedData, Password, Salt) when is_list(Password) ->
    decrypt_with_user_password(EncryptedData, list_to_binary(Password), Salt);
decrypt_with_user_password(EncryptedData, Password, Salt) when is_list(Salt) ->
    decrypt_with_user_password(EncryptedData, Password, list_to_binary(Salt));
decrypt_with_user_password(EncryptedData, Password, Salt) ->
    try
        EncryptionKey = crypto:pbkdf2_hmac(sha256, Password, Salt, 100000, 32),
        IV = base64:decode(maps:get(encryption_iv, EncryptedData)),
        Ciphertext = base64:decode(maps:get(encrypted_private_key, EncryptedData)),
        AuthTag = base64:decode(maps:get(encryption_auth_tag, EncryptedData)),
        case crypto:crypto_one_time_aead(
            aes_256_gcm,
            EncryptionKey,
            IV,
            Ciphertext,
            <<>>,
            AuthTag,
            false
        ) of
            error -> {error, decryption_failed};
            Plaintext -> {ok, Plaintext}
        end
    catch
        _:_ -> {error, decryption_failed}
    end.

encrypt_private_key(PrivateKey, EncryptionKey) ->
    IV = crypto:strong_rand_bytes(16),
    {Ciphertext, AuthTag} = crypto:crypto_one_time_aead(
        aes_256_gcm,
        EncryptionKey,
        IV,
        PrivateKey,
        <<>>,
        true
    ),
    #{
        ciphertext => Ciphertext,
        iv => IV,
        auth_tag => AuthTag
    }.

decrypt_private_key(Ciphertext, IV, AuthTag, EncryptionKey) ->
    case crypto:crypto_one_time_aead(
        aes_256_gcm,
        EncryptionKey,
        IV,
        Ciphertext,
        <<>>,
        AuthTag,
        false
    ) of
        error -> {error, decryption_failed};
        Plaintext -> {ok, Plaintext}
    end.

sign_transaction(TransactionData, PrivateKeyBase58) ->
    try
        PrivateKey = base58_decode(PrivateKeyBase58),
        Signature = crypto:sign(eddsa, none, TransactionData, [PrivateKey, ed25519]),
        {ok, base58_encode(Signature)}
    catch
        _:Reason -> {error, {signing_failed, Reason}}
    end.

verify_signature(TransactionData, SignatureBase58, PublicKeyBase58) ->
    try
        Signature = base58_decode(SignatureBase58),
        PublicKey = base58_decode(PublicKeyBase58),
        crypto:verify(eddsa, none, TransactionData, Signature, [PublicKey, ed25519])
    catch
        _:_ -> false
    end.

hash_password(Password) ->
    erlpass:hash(Password).

verify_password(Password, Hash) ->
    erlpass:match(Password, Hash).

base58_encode(Binary) when is_binary(Binary) ->
    base58_encode(Binary, <<>>).

base58_encode(<<>>, Acc) ->
    binary_to_list(Acc);
base58_encode(<<0, Rest/binary>>, Acc) ->
    base58_encode(Rest, <<Acc/binary, $1>>);
base58_encode(Binary, Acc) ->
    Alphabet = <<"123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz">>,
    Int = binary:decode_unsigned(Binary, big),
    base58_encode_int(Int, Alphabet, Acc).

base58_encode_int(0, _Alphabet, Acc) ->
    binary_to_list(Acc);
base58_encode_int(Int, Alphabet, Acc) ->
    Rem = Int rem 58,
    Char = binary:at(Alphabet, Rem),
    base58_encode_int(Int div 58, Alphabet, <<Char, Acc/binary>>).

base58_decode(String) when is_list(String) ->
    base58_decode(list_to_binary(String));
base58_decode(Binary) when is_binary(Binary) ->
    Alphabet = <<"123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz">>,
    base58_decode(Binary, Alphabet, 0).

base58_decode(<<>>, _Alphabet, Acc) ->
    binary:encode_unsigned(Acc, big);
base58_decode(<<Char, Rest/binary>>, Alphabet, Acc) ->
    case binary:match(Alphabet, <<Char>>) of
        {Pos, _} ->
            base58_decode(Rest, Alphabet, Acc * 58 + Pos);
        nomatch ->
            error(invalid_base58)
    end.
