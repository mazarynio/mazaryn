-module(user_server).
-compile([export_all, nowarn_export_all]).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).
-import(postdb, [init/0]).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

create_account(Username, Password, Email) ->
    gen_server:call({global, ?MODULE}, {create_account, Username, Password, Email}).

get_user(Username) ->
    gen_server:call({global, ?MODULE}, {get_user, Username}). 

get_users() ->
    gen_server:call({global, ?MODULE}, {get_users}).

get_password(Username) ->
    gen_server:call({global, ?MODULE}, {get_password, Username}).

get_user_by_email(Email) ->
    gen_server:call({global, ?MODULE}, {get_user_by_email, Email}).

change_password(Username, CurrentPass, NewPass) ->
    gen_server:call({global, ?MODULE}, {change_password, Username, CurrentPass, NewPass}).

change_email(Username, CurrentPass, NewEmail) -> 
    gen_server:call({global, ?MODULE}, {change_email, Username, CurrentPass, NewEmail}).

change_username(Username, CurrentPass, NewUsername) ->
    gen_server:call({global, ?MODULE}, {change_username, Username, CurrentPass, NewUsername}).

delete_user(Username) ->
    gen_server:call({global, ?MODULE}, {delete_user, Username}).

following(Username, Following) ->
    gen_server:call({global, ?MODULE}, {following, Username, Following}).

unfollowing(Username, Following) ->
    gen_server:call({global, ?MODULE}, {unfollowing, Username, Following}).

follow_multiple(Username, Others) ->
    gen_server:call({global, ?MODULE}, {follow_multiple, Username, Others}).

unfollow_multiple(Username, Others) ->
    gen_server:call({global, ?MODULE}, {unfollow_multiple, Username, Others}).

save_post(Username, PostId) ->
    gen_server:call({global, ?MODULE}, {save_post, Username, PostId}).

unsave_post(Username, PostId) ->
    gen_server:call({global, ?MODULE}, {unsave_post, Username, PostId}).

save_posts(Username, PostIds) ->
    gen_server:call({global, ?MODULE}, {save_posts, Username, PostIds}).

unsave_posts(Username, PostIds) ->
    gen_server:call({global, ?MODULE}, {unsave_posts, Username, PostIds}).

get_save_posts(Username) ->
    gen_server:call({global, ?MODULE}, {get_save_posts, Username}).

get_following(Username) ->
    gen_server:call({global, ?MODULE}, {get_following, Username}).

get_follower(Username) ->
    gen_server:call({global, ?MODULE}, {get_follower, Username}).

get_user_info(Username) ->
    gen_server:call({global, ?MODULE}, {get_user_info, Username}).



init([]) ->
    io:format("~p (~p) starting.... ~n", [{global, ?MODULE}, self()]),
    {ok, #state{}}.

handle_call({create_account, Username, Password, Email}, _From, State = #state{}) ->
    userdb:insert(Username, Password, Email),
    io:format("New User is added on"),
    {reply, ok, State};


handle_call({get_user, Username}, _From, State) ->
    userdb:get_user(Username),
    {reply, ok, State};

handle_call({get_users}, _From, State = #state{}) ->
    userdb:get_users(),
    {reply, ok, State};

handle_call({get_password, Username}, _From, State) ->
    userdb:get_password(Username),
    {reply, ok, State};

handle_call({get_user_by_email, Email}, _From, State) ->
    userdb:get_user_by_email(Email),
    {reply, ok, State};

handle_call({change_password, Username, CurrentPass, NewPass}, _From, State) ->
    userdb:change_password(Username, CurrentPass, NewPass),
    {reply, ok, State};

handle_call({change_email, Username, CurrentPass, NewEmail}, _From, State) ->
    userdb:change_email(Username, CurrentPass, NewEmail),
    {reply, ok, State};

handle_call({change_username, Username, CurrentPass, NewUsername}, _From, State) ->
    userdb:change_username(Username, CurrentPass, NewUsername),
    {reply, ok, State};

handle_call({delete_user, Username}, _From, State) ->
    userdb:delete_user(Username),
    {reply, ok, State};

handle_call({following, Username, Following}, _From, State) ->
    userdb:following(Username, Following),
    {reply, ok, State};

handle_call({unfollowing, Username, Following}, _From, State) ->
    userdb:unfollowing(Username, Following),
    {reply, ok, State};

handle_call({follow_multiple, Username, Others}, _From, State) ->
    userdb:follow_multiple(Username, Others),
    {reply, ok, State};

handle_call({unfollow_multiple, Username, Others}, _From, State) ->
    userdb:unfollow_multiple(Username, Others),
    {reply, ok, State};

handle_call({save_post, Username, PostId}, _From, State) ->
    userdb:save_post(Username, PostId),
    {reply, ok, State};

handle_call({unsave_post, Username, PostId}, _From, State) ->
    userdb:unsave_post(Username, PostId),
    {reply, ok, State};

handle_call({save_posts, Username, PostIds}, _From, State) ->
    userdb:save_posts(Username, PostIds),
    {reply, ok, State};

handle_call({unsave_posts, Username, PostIds}, _From, State) ->
    userdb:unsave_posts(Username, PostIds),
    {reply, ok, State};

handle_call({get_save_posts, Username}, _From, State) ->
    userdb:get_save_posts(Username),
    {reply, ok, State};

handle_call({get_following, Username}, _From, State) ->
    userdb:get_following(Username),
    {reply, ok, State};

handle_call({get_follower, Username}, _From, State) ->
    userdb:get_follower(Username),
    {reply, ok, State};

handle_call({get_user_info, Username}, _From, State) ->
    userdb:get_user_info(Username),
    {reply, ok, State};


handle_call(_Request, _From, State) ->
    {noreply, State}.
    
handle_cast(_Request, State) ->
    {noreply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

is_username_taken(Username) ->
    Username == not_logged_in orelse userdb:return_user(Username) =/= [].

