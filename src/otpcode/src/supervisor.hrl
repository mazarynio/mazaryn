-define(USER_SERVER, #{
    id => user_server,
    start => {user_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [user_server]
}).

-define(POST_SERVER, #{
    id => post_server,
    start => {post_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [post_server]
}).

-define(TOKEN_SERVER, #{
    id => token_server,
    start => {token_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [token_server]
}).

-define(CHAT_SERVER, #{
    id => chat_server,
    start => {chat_server, start_link, []},
    restart => permanent,
    shutdown => 500000,
    type => worker,
    modules => [chat_server]
}).

-define(BLOG_SERVER, #{
    id => blog_server,
    start => {blog_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [blog_server]
}).

-define(MEDIA_SERVER, #{
    id => media_server,
    start => {media_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [media_server]
}).

-define(NOTIF_EVENT, #{
    id => notif_event,
    start => {notif_event, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [notif_event]
}).