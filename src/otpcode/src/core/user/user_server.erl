-module(user_server).
-compile([export_all, nowarn_export_all]).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

create_account(Username, Password) ->
    gen_server:call({global, ?MODULE}, {create_account, Username, Password}).

signup(Email, Username, Password) ->
    gen_server:call({global, ?MODULE}, {signup, Email, Username, Password}).

signin(Username, Password) ->
    gen_server:call({global, ?MODULE}, {signin, Username, Password}).


login(Username, Password) ->
    gen_server:call(?MODULE, {login, Username, Password}).

get_user(Username) ->
    gen_server:call({global, ?MODULE}, {get_user, Username}).

get_users() ->
    gen_server:call({global, ?MODULE}, {get_users}).

search_user(Username) ->
    gen_server:call({global, ?MODULE}, {search_user, Username}).

follow(Username) ->
    gen_server:call({global, ?MODULE}, {follow, Username}).

unfollow(Username) ->
    gen_server:call({global, ?MODULE}, {unfollow, Username}).

is_following(Username) ->
    gen_server:call({global, ?MODULE}, {is_following, Username}).

remove_user(Username) ->
    gen_server:call({global, ?MODULE}, {remove_user, Username}).

create_post() ->
    gen_server:call({global, ?MODULE}, {create_post}).

get_post() ->
    gen_server:call({global, ?MODULE}, {get_post}).

follow_user() ->
    gen_server:call({global, ?MODULE}, {follow_user}).

init([]) ->
    io:format("~p (~p) starting.... ~n", [{global, ?MODULE}, self()]),
    userdb:init(),
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
    follower_db:is_following(Username),
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

