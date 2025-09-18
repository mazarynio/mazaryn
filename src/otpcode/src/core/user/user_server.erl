-module(user_server).
-author("Zaryn Technologies").
-include_lib("kernel/include/logger.hrl").

%% API exports
-export([start_link/0,
         create_account/3, create_account_concurrent/3, login/2, insert_notif/2,
         get_user/1, get_user_in_transaction/1, get_users/0, delete_user/1,
         get_user_by_email/1, get_user_by_id/1, get_token_by_id/1, get_single_notif/1,
         get_all_notifs/1, get_password/1,
         set_user_info/3, get_user_info/2,
         follow/2, unfollow/2,
         follow_multiple/2, unfollow_multiple/2,
         save_post/2, unsave_post/2, save_posts/2, unsave_posts/2,
         get_save_posts/1,
         change_username/3, change_password/3, change_email/3,
         get_following/1, get_follower/1,
         block/2, unblock/2, get_blocked/1, add_media/3, get_media/2,
         search_user/1, search_user_pattern/1, insert_avatar/2,
         insert_banner/2, report_user/4, make_private/1, make_public/1, validate_user/1, get_following_usernames/1, search_followings/2,
         get_follower_usernames/1, search_followers/2, get_user_level/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {worker_pool_size = 100, 
                worker_pool = []}). 


%% @doc Start the user server
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%% @doc Create a new user account
create_account(Username, Password, Email) ->
    gen_server:call({global, ?MODULE}, {create_account, Username, Password, Email}, 30000).

create_account_concurrent(Username, Password, Email) ->
    RequestId = make_ref(),
    gen_server:cast({global, ?MODULE}, {create_account_concurrent, 
                                  Username, Password, Email, 
                                  {self(), RequestId}}),
    receive
        {account_creation_result, RequestId, Result} -> Result
    after 15000 ->
        {error, timeout}
    end.

%% @doc User login
login(Email, Password) ->
    gen_server:call({global, ?MODULE}, {login, Email, Password}). 

%% @doc Insert notification for a user
insert_notif(UserID, Message) ->
    gen_server:call({global, ?MODULE}, {insert_notif, UserID, Message}).

%% @doc Add media to a user profile
add_media(Id, MediaType, Url) ->
    gen_server:call({global, ?MODULE}, {add_media, Id, MediaType, Url}).

%% @doc Insert avatar for a user profile
insert_avatar(Id, AvatarUrl) ->
    gen_server:call({global, ?MODULE}, {insert_avatar, Id, AvatarUrl}).

%% @doc Insert banner for a user profile
insert_banner(Id, BannerUrl) ->
    gen_server:call({global, ?MODULE}, {insert_banner, Id, BannerUrl}).

%% @doc Set user information fields
set_user_info(Username, Fields, Values) ->
    gen_server:call({global, ?MODULE}, {set_user_info, Username, Fields, Values}).

%% @doc Get media for a user
get_media(Id, Type) ->
    gen_server:call({global, ?MODULE}, {get_media, Id, Type}).

get_user_level(UserID) ->
    gen_server:call({global, ?MODULE}, {get_user_level, UserID}).

%% @doc Get user by username
get_user(Username) ->
    gen_server:call({global, ?MODULE}, {get_user, Username}).

%% @doc Get user by username within a transaction
get_user_in_transaction(Username) ->
    gen_server:call({global, ?MODULE}, {get_user_in_transaction, Username}).

%% @doc Get all users
get_users() ->
    gen_server:call({global, ?MODULE}, {get_users}).

%% @doc Get user password (hash)
get_password(Id) ->
    gen_server:call({global, ?MODULE}, {get_password, Id}).

%% @doc Get user by email
get_user_by_email(Email) ->
    gen_server:call({global, ?MODULE}, {get_user_by_email, Email}).

%% @doc Get user by ID
get_user_by_id(Id) ->
    gen_server:call({global, ?MODULE}, {get_user_by_id, Id}).

%% @doc Get token by ID
get_token_by_id(TokenID) ->
    gen_server:call({global, ?MODULE}, {get_token_by_id, TokenID}).

%% @doc Validate user token
validate_user(TokenID) ->
    gen_server:call({global, ?MODULE}, {validate_user, TokenID}).

%% @doc Get single notification
get_single_notif(NotifID) ->
    gen_server:call({global, ?MODULE}, {get_single_notif, NotifID}).

%% @doc Get all notifications for a user
get_all_notifs(UserID) ->
    gen_server:call({global, ?MODULE}, {get_all_notifs, UserID}).

%% @doc Change user password
change_password(Username, CurrentPass, NewPass) ->
    gen_server:call({global, ?MODULE}, {change_password, Username, CurrentPass, NewPass}).

%% @doc Change user email
change_email(Username, Password, NewEmail) ->
    gen_server:call({global, ?MODULE}, {change_email, Username, Password, NewEmail}).

%% @doc Change username
change_username(Username, CurrentPass, NewUsername) ->
    gen_server:call({global, ?MODULE}, {change_username, Username, CurrentPass, NewUsername}).

%% @doc Delete user
delete_user(Username) ->
    gen_server:call({global, ?MODULE}, {delete_user, Username}).

%% @doc Follow another user
follow(Id, Following) ->
    gen_server:call({global, ?MODULE}, {follow, Id, Following}).

%% @doc Unfollow another user
unfollow(Id, Following) ->
    gen_server:call({global, ?MODULE}, {unfollow, Id, Following}).

%% @doc Follow multiple users
follow_multiple(Id, Others) ->
    gen_server:call({global, ?MODULE}, {follow_multiple, Id, Others}).

%% @doc Unfollow multiple users
unfollow_multiple(Id, Others) ->
    gen_server:call({global, ?MODULE}, {unfollow_multiple, Id, Others}).

%% @doc Save a post
save_post(Id, PostId) ->
    gen_server:call({global, ?MODULE}, {save_post, Id, PostId}).

%% @doc Unsave a post
unsave_post(Id, PostId) ->
    gen_server:call({global, ?MODULE}, {unsave_post, Id, PostId}).

%% @doc Save multiple posts
save_posts(Id, PostIds) ->
    gen_server:call({global, ?MODULE}, {save_posts, Id, PostIds}).

%% @doc Unsave multiple posts
unsave_posts(Id, PostIds) ->
    gen_server:call({global, ?MODULE}, {unsave_posts, Id, PostIds}).

%% @doc Get saved posts
get_save_posts(Id) ->
    gen_server:call({global, ?MODULE}, {get_save_posts, Id}).

%% @doc Get users the user is following
get_following(Id) ->
    gen_server:call({global, ?MODULE}, {get_following, Id}).

get_following_usernames(Id) ->
    gen_server:call({global, ?MODULE}, {get_following_usernames, Id}).

search_followings(MyUserId, TargetUsername) ->
    gen_server:call({global, ?MODULE}, {search_followings, MyUserId, TargetUsername}).

%% @doc Get users following the user
get_follower(Id) ->
    gen_server:call({global, ?MODULE}, {get_follower, Id}).

get_follower_usernames(Id) ->
    gen_server:call({global, ?MODULE}, {get_follower_usernames, Id}).

search_followers(MyUserId, TargetUsername) ->
    gen_server:call({global, ?MODULE}, {search_followers, MyUserId, TargetUsername}).

%% @doc Get user information for specific fields
get_user_info(Username, Fields) ->
    gen_server:call({global, ?MODULE}, {get_user_info, Username, Fields}).

%% @doc Block a user
block(Id, Blocked) ->
    gen_server:call({global, ?MODULE}, {user_block, Id, Blocked}).

%% @doc Unblock a user
unblock(Id, Unblocked) ->
    gen_server:call({global, ?MODULE}, {user_unblock, Id, Unblocked}).

%% @doc Get blocked users
get_blocked(Id) ->
    gen_server:call({global, ?MODULE}, {get_blocked, Id}).

%% @doc Search for user by exact username
search_user(Username) ->
    gen_server:call({global, ?MODULE}, {search_user, Username}).

%% @doc Search for users by pattern
search_user_pattern(Pattern) ->
    gen_server:call({global, ?MODULE}, {search_user_pattern, Pattern}).

%% @doc Report a user
report_user(MyID, UserID, Type, Description) ->
    gen_server:call({global, ?MODULE}, {report_user, MyID, UserID, Type, Description}).

%% @doc Make user account private
make_private(UserID) ->
    gen_server:call({global, ?MODULE}, {make_private, UserID}).

%% @doc Make user account public
make_public(UserID) ->
    gen_server:call({global, ?MODULE}, {make_public, UserID}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the server
init([]) ->
    ?LOG_NOTICE("User server has been started - ~p", [self()]),
    PoolSize = application:get_env(social_network, user_worker_pool_size, 100),
    WorkerPool = initialize_worker_pool(PoolSize),
    {ok, #state{worker_pool_size = PoolSize, worker_pool = WorkerPool}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% @doc Initialize worker pool
initialize_worker_pool(Size) ->
    [spawn_link(fun() -> worker_loop() end) || _ <- lists:seq(1, Size)].

worker_loop() ->
    receive
        {create_account, Username, Password, Email, From} ->
            Result = userdb:insert_concurrent(Username, Password, Email),
            gen_server:reply(From, Result),
            worker_loop();
        
        {create_account_async, Username, Password, Email, {Pid, RequestId}} ->
            Result = userdb:insert_concurrent(Username, Password, Email),
            Pid ! {account_creation_result, RequestId, Result},
            worker_loop();
            
        {stop, From} ->
            From ! {stopped, self()};
            
        Other ->
            ?LOG_WARNING("Worker received unknown message: ~p", [Other]),
            worker_loop()
    end.
%%====================================================================
%% gen_server callback implementations
%%====================================================================

handle_call({create_account, Username, Password, Email}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_account, Username, Password, Email, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};

handle_call({insert_notif, UserID, Message}, _From, State) ->
    Res = notifdb:insert(UserID, Message),
    {reply, Res, State};

handle_call({add_media, Id, MediaType, Url}, _From, State) ->
    Res = userdb:insert_media(Id, MediaType, Url),
    {reply, Res, State};

handle_call({insert_avatar, Id, AvatarUrl}, _From, State) ->
    Res = userdb:insert_avatar(Id, AvatarUrl),
    {reply, Res, State};

handle_call({insert_banner, Id, BannerUrl}, _From, State) ->
    Res = userdb:insert_banner(Id, BannerUrl),
    {reply, Res, State};

handle_call({login, Email, Password}, _From, State) ->
    Res = userdb:login(Email, Password),
    {reply, Res, State};

handle_call({set_user_info, Username, Fields, Values}, _From, State) ->
    Res = userdb:set_user_info(Username, Fields, Values),
    {reply, Res, State};

handle_call({get_media, Id, Type}, _From, State) ->
    Res = userdb:get_media(Id, Type),
    {reply, Res, State};

handle_call({get_user_level, UserID}, _From, State) ->
    Res = userdb:get_user_level(UserID),
    {reply, Res, State};

handle_call({get_user, Username}, _From, State) ->
    Res = userdb:get_user(Username),
    {reply, Res, State};

handle_call({get_user_in_transaction, Username}, _From, State) ->
    Res = userdb:get_user_in_transaction(Username),
    {reply, Res, State};

handle_call({get_users}, _From, State) ->
    Res = userdb:get_users(),
    {reply, Res, State};

handle_call({get_password, Id}, _From, State) ->
    Res = userdb:get_password(Id),
    {reply, Res, State};

handle_call({get_user_by_email, Email}, _From, State) ->
    Res = userdb:get_user_by_email(Email),
    {reply, Res, State};

handle_call({get_user_by_id, Id}, _From, State) ->
    Res = userdb:get_user_by_id(Id),
    {reply, Res, State};

handle_call({get_token_by_id, TokenID}, _From, State) ->
    Res = userdb:get_token_by_id(TokenID),
    {reply, Res, State};

handle_call({validate_user, TokenID}, _From, State) ->
    Res = userdb:validate_user(TokenID),
    {reply, Res, State};

handle_call({get_single_notif, NotifID}, _From, State) ->
    Res = notifdb:get_single_notif(NotifID),
    {reply, Res, State};

handle_call({get_all_notifs, UserID}, _From, State) ->
    Res = notifdb:get_all_notifs(UserID),
    {reply, Res, State};

handle_call({change_password, Username, CurrentPass, NewPass}, _From, State) ->
    Res = userdb:change_password(Username, CurrentPass, NewPass),
    {reply, Res, State};

handle_call({change_email, Username, Password, NewEmail}, _From, State) ->
    Res = userdb:change_email(Username, Password, NewEmail),
    {reply, Res, State};

handle_call({change_username, Username, CurrentPass, NewUsername}, _From, State) ->
    Res = userdb:change_username(Username, CurrentPass, NewUsername),
    {reply, Res, State};

handle_call({delete_user, Username}, _From, State) ->
    Res = userdb:delete_user(Username),
    {reply, Res, State};

handle_call({follow, Id, Following}, _From, State) ->
    Res = userdb:follow(Id, Following),
    {reply, Res, State};

handle_call({unfollow, Id, Following}, _From, State) ->
    Res = userdb:unfollow(Id, Following),
    {reply, Res, State};

handle_call({follow_multiple, Id, Others}, _From, State) ->
    Res = userdb:follow_multiple(Id, Others),
    {reply, Res, State};

handle_call({unfollow_multiple, Id, Others}, _From, State) ->
    Res = userdb:unfollow_multiple(Id, Others),
    {reply, Res, State};

handle_call({save_post, Id, PostId}, _From, State) ->
    Res = userdb:save_post(Id, PostId),
    {reply, Res, State};

handle_call({unsave_post, Id, PostId}, _From, State) ->
    Res = userdb:unsave_post(Id, PostId),
    {reply, Res, State};

handle_call({save_posts, Id, PostIds}, _From, State) ->
    Res = userdb:save_posts(Id, PostIds),
    {reply, Res, State};

handle_call({unsave_posts, Id, PostIds}, _From, State) ->
    Res = userdb:unsave_posts(Id, PostIds),
    {reply, Res, State};

handle_call({get_save_posts, Id}, _From, State) ->
    Res = userdb:get_save_posts(Id),
    {reply, Res, State};

handle_call({get_following, Id}, _From, State) ->
    Res = userdb:get_following(Id),
    {reply, Res, State};

handle_call({get_following_usernames, Id}, _From, State) ->
    Res = userdb:get_following_usernames(Id),
    {reply, Res, State};

handle_call({search_followings, MyUserId, TargetUsername}, _From, State) ->
    Res = userdb:search_followings(MyUserId, TargetUsername),
    {reply, Res, State};

handle_call({get_follower, Id}, _From, State) ->
    Res = userdb:get_follower(Id),
    {reply, Res, State};

handle_call({get_follower_usernames, Id}, _From, State) ->
    Res = userdb:get_follower_usernames(Id),
    {reply, Res, State};

handle_call({search_followers, MyUserId, TargetUsername}, _From, State) ->
    Res = userdb:search_followers(MyUserId, TargetUsername),
    {reply, Res, State};

handle_call({get_user_info, Username, Fields}, _From, State) ->
    Res = userdb:get_user_info(Username, Fields),
    {reply, Res, State};

handle_call({user_block, Id, Blocked}, _From, State) ->
    Res = userdb:block(Id, Blocked),
    {reply, Res, State};

handle_call({user_unblock, Id, Unblocked}, _From, State) ->
    Res = userdb:unblock(Id, Unblocked),
    {reply, Res, State};

handle_call({get_blocked, Id}, _From, State) ->
    Res = userdb:get_blocked(Id),
    {reply, Res, State};

handle_call({search_user, Username}, _From, State) ->
    Res = userdb:search_user(Username),
    {reply, Res, State};

handle_call({search_user_pattern, Pattern}, _From, State) ->
    Res = userdb:search_user_pattern(Pattern),
    {reply, Res, State};

handle_call({report_user, MyID, UserID, Type, Description}, _From, State) ->
    Res = userdb:report_user(MyID, UserID, Type, Description),
    {reply, Res, State};

handle_call({make_private, UserID}, _From, State) ->
    Res = userdb:make_private(UserID),
    {reply, Res, State};

handle_call({make_public, UserID}, _From, State) ->
    Res = userdb:make_public(UserID),
    {reply, Res, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle casts
handle_cast({create_account_concurrent, Username, Password, Email, {Pid, RequestId}}, 
           State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_account_async, Username, Password, Email, {Pid, RequestId}},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc Handle info messages
handle_info({'EXIT', Pid, Reason}, State = #state{worker_pool = Workers}) ->
    case lists:member(Pid, Workers) of
        true ->
            ?LOG_WARNING("Worker ~p crashed with reason: ~p. Replacing.", [Pid, Reason]),
            NewWorker = spawn_link(fun() -> worker_loop() end),
            NewWorkers = lists:keyreplace(Pid, 1, Workers, NewWorker),
            {noreply, State#state{worker_pool = NewWorkers}};
        false ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Handle termination
terminate(_Reason, #state{worker_pool = Workers}) ->
    [Worker ! {stop, self()} || Worker <- Workers],
    [receive {stopped, W} -> ok after 1000 -> ok end || W <- Workers],
    ok.

%% @doc Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.