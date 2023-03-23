%% TODO: change id order for testing, modify later
-record(user, { id,
                username,
                password,
                email,
                media= [],
                post = [],
                notif = [],
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
                token_id}).

-record(notif, { id,
                 user_id,
                 message,
                 date_created }).

-record(post, { id,
                content,
                comments = [],
                likes = [],
                media = [],
                hashtag = [],
                author,
                other = [],
                date_created,
                date_updated}).

-record(comment, {id,
                  post,
                  author,
                  content,
                  date_created}).

-record(like, {id,
               post,
               userID,
               date_created}).

-record(event, {name, date, loc, desc}).
-record(follower, {id, username}).
-record(following, {id, username}).

-record(hed_wallet, { id, password, date_created }).
-record(chat, {id, title, peer_ids, type, inserted_at, updated_at}).



-define(MSG_INSUFFICIENT_FUNDS, <<"Insufficient funds.">>).
