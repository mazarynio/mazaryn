-record(transaction, {
    trx_id,
    type,
    amount,
    status 
}).

-record(wallet, {name, password, address, balance, pub_key, priv_key, tokens}).