-define(USER_SERVER, #{
    id => user_server,
    start => {user_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [user_server]
}).

-define(USER_LEVEL_SERVER, #{
    id => user_level_server,
    start => {user_level_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [user_level_server]
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

-define(BUSINESS_SERVER, #{
    id => business_server,
    start => {business_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [business_server]
}).

-define(ADS_SERVER, #{
    id => ads_server,
    start => {ads_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [ads_server]
}).

-define(QUANTUM_SERVER, #{
    id => quantum_server,
    start => {quantum_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [quantum_server]
}).

-define(AI_USER_SERVER, #{
    id => ai_user_server,
    start => {ai_user_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [ai_user_server]
}).

-define(AI_POST_SERVER, #{
    id => ai_post_server,
    start => {ai_post_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [ai_post_server]
}).

-define(AI_CHAT_SERVER, #{
    id => ai_chat_server,
    start => {ai_chat_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [ai_chat_server]
}).

-define(AI_MEDIA_SERVER, #{
    id => ai_media_server,
    start => {ai_media_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [ai_media_server]
}).

-define(AI_BUSINESS_SERVER, #{
    id => ai_business_server,
    start => {ai_business_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [ai_business_server]
}).

-define(AI_ADS_SERVER, #{
    id => ai_ads_server,
    start => {ai_ads_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [ai_ads_server]
}).