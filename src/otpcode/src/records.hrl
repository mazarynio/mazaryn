-record(user, { username,
                password,
                email,
                following = [],
                follower = [],
                blocking = [],
                saved_posts = [],
                other_info = [], % location, birthday, vvv
                private = false, % by default users are public
                last_login,
                date_created,
                date_updated}).

-record(post, { id,
                content,
                comments = [],
                author,
                date_created}).

-record(comment, {username, content, date_created}).


-record(group, {id, gp_name, num_members, members}).
-record(event, {name, date, loc, desc}).
-record(msg, {sender, receiver, content, timestamp}).
-record(follower, {id, username}).
-record(following, {id, username}).