-record(transaction, {
    id,
    type,
    amount,
    status 
}).

-record(wallet, {name, password, address, balance, pub_key, priv_key, tokens}).

-define(MSG_INSUFFICIENT_FUNDS, <<"Insufficient funds.">>).