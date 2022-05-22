-record(user, {id, username, password, email, pid, posts = 0, loc}).
-record(post, {id, content, comments = []}).
-record(comment, {username, content}).
-record(group, {id, gp_name, num_members, members}).
-record(event, {name, date, loc, desc}).
-record(msg, {sender, receiver, content, timestamp}).
-record(follower, {id, username}).
-record(following, {id, username}).