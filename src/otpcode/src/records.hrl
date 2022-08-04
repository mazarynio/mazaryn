%% TODO: change id order for testing, modify later
-record(user, { username,
                id,
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
                date_updated}).

-record(post, { id,
                content,
                comments = [],
                media = [],
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
