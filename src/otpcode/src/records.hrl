%% TODO: change id order for testing, modify later
-record(user, { id,
                username,
                password,
                email,
                media = [],
                post = [],
                blog_post = [],
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
                chat = [],
                verified = false,
                report = [],
                data = #{} }). 

-record(notif, { id,
                 user_id,
                 message,
                 date_created,
                 data = #{} }).

-record(post, { id,
                content,
                comments = [],
                likes = [],
                media = [],
                hashtag = [],
                mention,
                link_url,
                author,
                other = [],
                date_created,
                date_updated,
                report = [],
                data = #{} }). 

-record(blog_post, {id, 
                    content,
                    comments = [],
                    media,
                    author,
                    date_created,
                    date_updated,
                    data = #{} }).

-record(comment, {id,
                  post,
                  author,
                  content,
                  date_created,
                  data = #{} }).

-record(blog_comment, {id,
                       blog_post,
                       author, 
                       content,
                       date_created,
                       data = #{} }).

-record(like, {id,
               post,
               userID,
               date_created,
               data = #{} }).

-record(chat, {id,
               user_id,
               recipient_id,
               body,
               bot,
               date_created,
               date_updated,
               data = #{} }).


-record(event, {name, date, loc, desc, data = #{} }).
-record(follower, {id, username, data = #{}}).
-record(following, {id, username, data = #{} }).
-record(hed_wallet, { id, password, date_created, data =#{} }).

-record(media, {id, user_id, file, files, type,
date_created, date_updated, report = [], data = #{}}). 
-record(suspend, {
    id,
    user,
    status = false,
    duration
}).
-record(report, {
    id,
    type,
    description,
    reporter,
    user,
    post,
    media,
    date_created,
    data = #{}
}).

-define(MSG_INSUFFICIENT_FUNDS, <<"Insufficient funds.">>).
