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

-define(PIN_POST_SERVER, #{
    id => pin_post_server,
    start => {pin_post_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [pin_post]
}).

-define(STORAGE_QUOTA_SERVER, #{
    id => storage_quota_server,
    start => {storage_quota_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [storage_quota]
}).

-define(RATE_LIMITER_SERVER, #{
    id => rate_limiter_server,
    start => {rate_limiter_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [rate_limiter]
}).

-define(CONTENT_CACHE, #{
    id => content_cache,
    start => {content_cache, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [content_cache]
}).

-define(VIDEO_SERVER, #{
    id => video_server,
    start => {video_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [video_server]
}).

-define(DATASET_SERVER, #{
    id => dataset_server,
    start => {dataset_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [dataset_server]
}).

-define(NOTEBOOK_SERVER, #{
    id => notebook_server,
    start => {notebook_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [notebook_server]
}).

-define(MODEL_SERVER, #{
    id => model_server,
    start => {model_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [model_server]
}).

-define(COMPETITION_SERVER, #{
    id => competition_server,
    start => {competition_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [competition_server]
}).

-define(LEADERBOARD_SERVER, #{
    id => leaderboard_server,
    start => {leaderboard_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [leaderboard_server]
}).

-define(SOLANA_WALLET_SERVER, #{
    id => solana_wallet_server,
    start => {solana_wallet_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [solana_wallet_server]
}).

-define(NEAR_WALLET_SERVER, #{
    id => near_wallet_server,
    start => {near_wallet_server, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [near_wallet_server]
}).
