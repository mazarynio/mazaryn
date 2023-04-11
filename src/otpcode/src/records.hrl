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
                token_id,
                chat = []}).

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

-record(chat, {id, peer_ids, title, messages, inserted_at, updated_at}).

-record(message, {id,
                chat_id,
               user_id,
               recipient_id,
               body,
               date_created,
               date_updated}).

-record(event, {name, date, loc, desc}).
-record(follower, {id, username}).
-record(following, {id, username}).
-record(hed_wallet, { id, password, date_created }).

-define(MSG_INSUFFICIENT_FUNDS, <<"Insufficient funds.">>).
