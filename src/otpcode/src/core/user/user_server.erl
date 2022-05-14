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

login(Username, Password) ->
    gen_server:call({global, ?MODULE}, {login, Username, Password}).

set_user_info(Username, Fields, Values) ->
    gen_server:call({global, ?MODULE}, {set_user_info, Username, Fields, Values}).

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

follow(Username, Following) ->
    gen_server:call({global, ?MODULE}, {follow, Username, Following}).

unfollow(Username, Following) ->
    gen_server:call({global, ?MODULE}, {unfollow, Username, Following}).

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
    Res = userdb:insert(Username, Password, Email),
    io:format("New User is added on"),
    {reply, Res, State};

handle_call({login, Username, Password}, _From, State = #state{}) ->
    Res = userdb:login(Username, Password),
    {reply, Res, State};

handle_call({set_user_info, Username, Fields, Values}, _From, State) ->
    Res = userdb:set_user_info(Username, Fields, Values),
    {reply, Res, State};

handle_call({get_user, Username}, _From, State) ->
    Res = userdb:get_user(Username),
    {reply, Res, State};

handle_call({get_users}, _From, State = #state{}) ->
    Res = userdb:get_users(),
    {reply, Res, State};

handle_call({get_password, Username}, _From, State) ->
    Res = userdb:get_password(Username),
    {reply, Res, State};

handle_call({get_user_by_email, Email}, _From, State) ->
    Res = userdb:get_user_by_email(Email),
    {reply, Res, State};

handle_call({change_password, Username, CurrentPass, NewPass}, _From, State) ->
    Res = userdb:change_password(Username, CurrentPass, NewPass),
    {reply, Res, State};

handle_call({change_email, Username, CurrentPass, NewEmail}, _From, State) ->
    Res = userdb:change_email(Username, CurrentPass, NewEmail),
    {reply, Res, State};

handle_call({change_username, Username, CurrentPass, NewUsername}, _From, State) ->
    Res = userdb:change_username(Username, CurrentPass, NewUsername),
    {reply, Res, State};

handle_call({delete_user, Username}, _From, State) ->
    Res = userdb:delete_user(Username),
    {reply, Res, State};

handle_call({follow, Username, Following}, _From, State) ->
    Res = userdb:follow(Username, Following),
    {reply, Res, State};

handle_call({unfollow, Username, Following}, _From, State) ->
    Res = userdb:unfollow(Username, Following),
    {reply, Res, State};

handle_call({follow_multiple, Username, Others}, _From, State) ->
    Res = userdb:follow_multiple(Username, Others),
    {reply, Res, State};

handle_call({unfollow_multiple, Username, Others}, _From, State) ->
    Res = userdb:unfollow_multiple(Username, Others),
    {reply, Res, State};

handle_call({save_post, Username, PostId}, _From, State) ->
    Res = userdb:save_post(Username, PostId),
    {reply, Res, State};

handle_call({unsave_post, Username, PostId}, _From, State) ->
    Res = userdb:unsave_post(Username, PostId),
    {reply, Res, State};

handle_call({save_posts, Username, PostIds}, _From, State) ->
    Res = userdb:save_posts(Username, PostIds),
    {reply, Res, State};

handle_call({unsave_posts, Username, PostIds}, _From, State) ->
    Res = userdb:unsave_posts(Username, PostIds),
    {reply, Res, State};

handle_call({get_save_posts, Username}, _From, State) ->
    Res = userdb:get_save_posts(Username),
    {reply, Res, State};

handle_call({get_following, Username}, _From, State) ->
    Res = userdb:get_following(Username),
    {reply, Res, State};

handle_call({get_follower, Username}, _From, State) ->
    Res = userdb:get_follower(Username),
    {reply, Res, State};

handle_call({get_user_info, Username}, _From, State) ->
    Res = userdb:get_user_info(Username),
    {reply, Res, State};


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

