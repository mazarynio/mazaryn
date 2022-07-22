-record(user, { username,
                password,
                email,
                post = [],
                following = [],
                follower = [],
                blocked = [],
                saved_posts = [],
                other_info = [], %location, birthday
                private = false,
                date_created,
                date_updated}).

-record(post, { id,
                content,
                comments = [],
                media,
                author,
                date_created,
                date_updated}).

-record(comment, {id,
                  post,
                  username,
                  content,
                  date_created}).

-record(group, {id, gp_name, num_members, members}).
-record(event, {name, date, loc, desc}).
-record(msg, {sender, receiver, content, timestamp}).
-record(follower, {id, username}).
-record(following, {id, username}).

%% crypto wallet and transaction
-record(transaction, {id,
                      type,
                      amount,
                      status}).

-record(wallet, {name,
                 password,
                 address,
                 balance,
                 pub_key,
                 priv_key,
                 secret_phase,
                 transaction = []}).


-define(MSG_INSUFFICIENT_FUNDS, <<"Insufficient funds.">>).