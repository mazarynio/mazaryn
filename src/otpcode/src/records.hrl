%% TODO: change id order for testing, modify later
-record(user, { id,
                username,
                password,
                email,
                media= [],
                post = [],
                following = [],
                follower = [],
                blocked = [],
                saved_posts = [],
                other_info = [], %location, birthday
                private = false,
                date_created,
                date_updated,
                avatar_url,
                banner_url,
                token_id,
                notification = [] }).

-record(notification, { id,
                        from,
                        to,
                        message }).

-record(post, { id,
                content,
                comments = [],
                media = [],
                hashtag = [],
                author,
                other = [],
                date_created,
                date_updated}).

-record(comment, {id,
                  post,
                  author,
                  content=[],
                  date_created}).

-record(group, {id, gp_name, num_members, members}).
-record(event, {name, date, loc, desc}).
-record(follower, {id, username}).
-record(following, {id, username}).

%% crypto wallet and transaction
-record(transaction, {id,
                      type,
                      amount,
                      status}).

-record(hed_wallet, { id, password, date_created }).

-record(ae_transaction, {id,
                         signature,
                         type,
                         amount,
                         status,
                         fee}).

-record(nft, {
    music,
    art
}).


-define(MSG_INSUFFICIENT_FUNDS, <<"Insufficient funds.">>).
