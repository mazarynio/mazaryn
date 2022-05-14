-module(user_server).
-export([start_link/0,
         create_account/3,login/2,
         get_user/1, get_users/0, remove_user/1,
         get_user_by_email/1, get_password/1,
         set_user_info/3, get_user_info/1,
         follow/2, unfollow/2, follow_multiple/2, unfollow_multiple/2,
         change_username/3, change_password/3, change_email/3,
         save_post/2, unsave_post/2,
         save_posts/2, unsave_posts/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_account(Username, Password, Email) ->
    gen_server:call(?MODULE, {create_account, Username, Password, Email}).

login(Username, Password) ->
    gen_server:call(?MODULE, {login, Username, Password}).

get_user(Username) ->
    gen_server:call(?MODULE, {get_user, Username}).

get_users() ->
    gen_server:call(?MODULE, {get_users}).

remove_user(Username) ->
    gen_server:call(?MODULE, {remove_user, Username}).

get_user_by_email(Email) ->
    gen_server:call(?MODULE, {get_user_by_email, Email}).

get_password(Username) ->
    gen_server:call(?MODULE, {get_password, Username}).

change_username(Username, CurrentPass, NewUsername) ->
    gen_server:call(?MODULE, {change_username, Username, CurrentPass, NewUsername}).

change_password(Username, CurrentPass, NewPass) ->
    gen_server:call(?MODULE, {change_password, Username, CurrentPass, NewPass}).

change_email(Username, CurrentPass, NewEmail) ->
    gen_server:call(?MODULE, {change_email, Username, CurrentPass, NewEmail}).

set_user_info(Username, Fields, Values) ->
    gen_server:call(?MODULE, {set_user_info, Username, Fields, Values}).

get_user_info(Username) ->
    gen_server:call(?MODULE, {get_user_info, Username}).

%%%%%
follow(Username, Following) ->
    gen_server:call(?MODULE, {follow_user, Username, Following}).

unfollow(Username, Following) ->
    gen_server:call(?MODULE, {unfollow_user, Username, Following}).

follow_multiple(Username, Others) ->
    gen_server:call(?MODULE, {follow_multiple, Username, Others}).

unfollow_multiple(Username, Others) ->
    gen_server:call(?MODULE, {unfollow_multiple, Username, Others}).

save_post(Username, Post) ->
    gen_server:call(?MODULE, {save_post, Username, Post}).

unsave_post(Username, Post) ->
    gen_server:call(?MODULE, {unsave_post, Username, Post}).

save_posts(Username, Posts) ->
    gen_server:call(?MODULE, {save_posts, Username, Posts}).

unsave_posts(Username, Posts) ->
    gen_server:call(?MODULE, {unsave_posts, Username, Posts}).


%%%%%%%%%%%%%%%% handler functions %%%%%%%%%%%
init([]) ->
    io:format("~p (~p) starting.... ~n", [{local, ?MODULE}, self()]),
    {ok, #state{}}.

handle_call({create_account, Username, Password, Email}, _From, State = #state{}) ->
    Res = userdb:insert(Username, Password, Email),
    {reply, Res, State};

handle_call({login, Username, Password}, _From, State = #state{}) ->
    Res = userdb:login(Username, Password),
    {reply, Res, State};

handle_call({get_user, Username}, _From, State) ->
    Res = userdb:get_user(Username),
    {reply, Res, State};

handle_call({get_users}, _From, State = #state{}) ->
    Res = userdb:get_users(),
    {reply, Res, State};

handle_call({get_user_by_email, Email}, _From, State) ->
    Res = userdb:get_user_by_email(Email),
    {reply, Res, State};

handle_call({get_password, Username}, _From, State) ->
    Res = userdb:get_password(Username),
    {reply, Res, State};

handle_call({remove_user, Username}, _From, State) ->
    userdb:delete_user(Username),
    {reply, ok, State};

handle_call({change_username, Username, CurrentPass, NewUsername}, _From, State = #state{}) ->
    Res = userdb:change_username(Username, CurrentPass, NewUsername),
    {reply, Res, State};

handle_call({change_password, Username, CurrentPass, NewPass}, _From, State = #state{}) ->
    Res = userdb:change_password(Username, CurrentPass, NewPass),
    {reply, Res, State};

handle_call({change_email, Username, CurrentPass, NewEmail}, _From, State = #state{}) ->
    Res = userdb:change_email(Username, CurrentPass, NewEmail),
    {reply, Res, State};

handle_call({set_user_info, Username, Fields, Values}, _From, State = #state{}) ->
    userdb:set_user_info(Username, Fields, Values),
    {reply, ok, State};

handle_call({get_user_info, Username}, _From, State = #state{}) ->
    Res = userdb:get_user_info(Username),
    {reply, Res, State};


%%%%%%% following/follower

handle_call({follow_user, Username, Following}, _From, State) ->
    userdb:follow(Username, Following),
    {reply, ok, State};

handle_call({unfollow_user, Username, Following}, _From, State) ->
    userdb:unfollow(Username, Following),
    {reply, ok, State};

handle_call({follow_multiple, Username, Others}, _From, State) ->
    userdb:follow_multiple(Username, Others),
    {reply, ok, State};

handle_call({unfollow_multiple, Username, Others}, _From, State) ->
    userdb:unfollow_multiple(Username, Others),
    {reply, ok, State};

%% Save post for reading alter
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

