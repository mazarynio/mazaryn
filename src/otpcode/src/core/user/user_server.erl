-module(user_server).
-compile([export_all, nowarn_export_all]).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).
-import(postdb, [init/0]).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

create_account(Username, Password) ->
    gen_server:call({global, ?MODULE}, {create_account, Username, Password}).

get_user(Username) ->
    gen_server:call({global, ?MODULE}, {get_user, Username}). 

get_users() ->
    gen_server:call({global, ?MODULE}, {get_users}).

signup(Email, Username, Password) ->
    gen_server:call({global, ?MODULE}, {signup, Email, Username, Password}).

signin(Username, Password) ->
    gen_server:call({global, ?MODULE}, {signin, Username, Password}).


login(Username, Password) ->
    gen_server:call(?MODULE, {login, Username, Password}).

search_user(Username) ->
    gen_server:call({global, ?MODULE}, {search_user, Username}).

remove_user(Username) ->
    gen_server:call({global, ?MODULE}, {remove_user, Username}).

create_post(Content) ->
    gen_server:call({global, ?MODULE}, {create_post, Content}).

get_post() ->
    gen_server:call({global, ?MODULE}, {get_post}).


following(Username, Following) ->
    gen_server:call({global, ?MODULE}, {user_follow_other, Username, Following}).

unfollow_other(Username, Following) ->
    gen_server:call({global, ?MODULE}, {user_unfollow_other, Username, Following}).
    
follow_post(Username, Post) ->
    gen_server:call({global, ?MODULE}, {follow_post, Username, Post}).
    
unfollow_post(Username, Post) ->
    gen_server:call({global, ?MODULE}, {unfollow_other, Username, Post}).

init([]) ->
    io:format("~p (~p) starting.... ~n", [{global, ?MODULE}, self()]),
    {ok, #state{}}.

handle_call({create_account, Username, Password}, _From, State = #state{}) ->
    userdb:insert(Username, Password),
    io:format("New User is added on"),
    {reply, ok, State};


handle_call({get_user, Username}, _From, State) ->
    userdb:get_user(Username),
    {reply, ok, State};

handle_call({get_users}, _From, State = #state{}) ->
    userdb:get_users(),
    {reply, ok, State};

handle_call({search_user, Username}, _From, State = #state{}) ->
    userdb:search_user(Username),
    {reply, ok, State};

handle_call({follow, Username}, _From, State) ->
    followerdb:save_follow_user(Username),
    {reply, ok, State};

handle_call({unfollow, Username}, _From, State) ->
    followerfb:delete_follow_user(Username),
    {reply, ok, State};

handle_call({is_following, Username}, _From, State) ->
    followerdb:is_following(Username),
    {reply, ok, State};

handle_call({create_post, Content}, _From, State) ->
    postdb:insert(Content),
    io:format("New Post is added on"),
    {reply, ok, State};

handle_call({user_follow_other, Username, Following}, _From, State) ->
    userdb:follow_other(Username, Following),
    userdb:following(Username, Following),
    {reply, ok, State};

handle_call({user_unfollow_other, Username, Other}, _From, State) ->
    userdb:unfollow_other(Username, Other),
    userdb:unfollowing(Username, Other),
    {reply, ok, State};
    
handle_call({user_follow_post, Username, Post}, _From, State) ->
    userdb:follow_post(Username, Post),
    {reply, ok, State};

handle_call({unfollow_post, Username, Post}, _From, State) ->
    userdb:unfollow_post(Username, Post),
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

