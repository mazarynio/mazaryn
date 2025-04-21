-module(ipfsdb).
-author("Zaryn Technologies").
-export([get_user_ipns/1, get_ipfs_key/1, remove_key/1]).
-include("../records.hrl"). 

get_user_ipns(UserID) ->
    Fun = fun() ->
            case mnesia:read(user, UserID, read) of
                [User] ->
                    IPFSKey = User#user.ipfs_key,
                    "/ipns/" ++ IPFSKey;
                [] ->
                    {error, user_not_found}
            end
    end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

get_ipfs_key(UserID) ->
    Fun = fun() ->
            case mnesia:read(user, UserID, read) of
                [User] ->
                    IPFSKey = User#user.ipfs_key,
                    IPFSKey;
                [] ->
                    {error, user_not_found}
            end
    end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

remove_key(UserID) ->
    Fun = fun() ->
            case mnesia:read(user, UserID, read) of
                [User] ->
                    case ipfs_client_4:key_rm(UserID) of
                        {ok, _} -> 
                            UpdatedUser = User#user{ipfs_key = undefined},
                            mnesia:write(UpdatedUser),
                            ok;
                        Error -> 
                            Error
                    end;
                [] ->
                    {error, user_not_found}
            end
    end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.
