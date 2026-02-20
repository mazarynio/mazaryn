-module(solana_wallet_setup).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").
-export([
    create_secure_wallet/3,
    import_from_private_key/3,
    import_from_seed/3,
    change_wallet_password/4,
    backup_wallet/2,
    restore_wallet/3
]).

create_secure_wallet(UserId, WalletPassword, Label) ->
    #{
        public_key := PublicKey,
        private_key := PrivateKey,
        seed := _Seed
    } = solana_crypto:generate_keypair(),

    EncryptedData = solana_crypto:encrypt_with_user_password(
        list_to_binary(PrivateKey),
        WalletPassword
    ),

    solana_walletdb:create_wallet(
        UserId,
        PublicKey,
        maps:get(encrypted_private_key, EncryptedData),
        maps:get(encryption_iv, EncryptedData),
        maps:get(encryption_auth_tag, EncryptedData),
        Label
    ).

import_from_private_key(UserId, PrivateKeyBase58, WalletPassword) ->
    case solana_crypto:derive_public_key(PrivateKeyBase58) of
        {ok, PublicKey} ->
            EncryptedData = solana_crypto:encrypt_with_user_password(
                list_to_binary(PrivateKeyBase58),
                WalletPassword
            ),

            solana_walletdb:import_wallet(
                UserId,
                PublicKey,
                maps:get(encrypted_private_key, EncryptedData),
                maps:get(encryption_iv, EncryptedData),
                maps:get(encryption_auth_tag, EncryptedData),
                "m/44'/501'/0'/0'",
                undefined
            );
        {error, _} = Error -> Error
    end.

import_from_seed(UserId, Mnemonic, WalletPassword) ->
    #{
        public_key := PublicKey,
        private_key := PrivateKey
    } = solana_crypto:derive_keypair_from_mnemonic(Mnemonic, <<"m/44'/501'/0'/0'">>),

    EncryptedData = solana_crypto:encrypt_with_user_password(
        list_to_binary(PrivateKey),
        WalletPassword
    ),

    solana_walletdb:import_wallet(
        UserId,
        PublicKey,
        maps:get(encrypted_private_key, EncryptedData),
        maps:get(encryption_iv, EncryptedData),
        maps:get(encryption_auth_tag, EncryptedData),
        "m/44'/501'/0'/0'",
        undefined
    ).

change_wallet_password(WalletId, OldPassword, NewPassword, UserId) ->
    case solana_walletdb:export_private_key(WalletId, OldPassword) of
        {ok, PrivateKey} ->
            EncryptedData = solana_crypto:encrypt_with_user_password(
                list_to_binary(PrivateKey),
                NewPassword
            ),

            Fun = fun() ->
                case mnesia:read(solana_wallet, WalletId) of
                    [] -> {error, wallet_not_found};
                    [Wallet] ->
                        if
                            Wallet#solana_wallet.user_id =/= UserId ->
                                {error, access_denied};
                            true ->
                                UpdatedWallet = Wallet#solana_wallet{
                                    encrypted_private_key = maps:get(encrypted_private_key, EncryptedData),
                                    encryption_iv = maps:get(encryption_iv, EncryptedData),
                                    encryption_auth_tag = maps:get(encryption_auth_tag, EncryptedData),
                                    encryption_salt = maps:get(encryption_salt, EncryptedData)
                                },
                                mnesia:write(UpdatedWallet),
                                ok
                        end
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, Reason}
            end;
        {error, _} = Error -> Error
    end.

backup_wallet(WalletId, Password) ->
    case solana_walletdb:export_private_key(WalletId, Password) of
        {ok, PrivateKey} ->
            {ok, #{
                private_key => PrivateKey,
                timestamp => calendar:universal_time(),
                wallet_id => WalletId
            }};
        {error, _} = Error -> Error
    end.

restore_wallet(UserId, BackupData, Password) ->
    PrivateKey = maps:get(private_key, BackupData),
    import_from_private_key(UserId, PrivateKey, Password).
