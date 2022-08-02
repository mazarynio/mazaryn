-module(user_server).

-include_lib("kernel/include/logger.hrl"). 

-export([start_link/0,
         create_account/3,login/2,
         get_user/1, get_user_in_transaction/1, get_users/0, delete_user/1,
         get_user_by_email/1, get_password/1,
         set_user_info/3, get_user_info/2,
         follow/2, unfollow/2,
         follow_multiple/2, unfollow_multiple/2,
         save_post/2, unsave_post/2, save_posts/2, unsave_posts/2,
         get_save_posts/1,
         change_username/3, change_password/3, change_email/3,
         get_following/1, get_follower/1,
         block/2, unblock/2, get_blocked/1, add_media/3, get_media/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).


start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).  

create_account(Username, Password, Email) ->
    gen_server:call({global, ?MODULE}, {create_account, Username, Password, Email}).

login(Email, Password) ->
    gen_server:call({global, ?MODULE}, {login, Email, Password}).

set_user_info(Username, Fields, Values) ->
    gen_server:call({global, ?MODULE}, {set_user_info, Username, Fields, Values}).

get_user(Username) ->
    gen_server:call({global, ?MODULE}, {get_user, Username}). 

get_user_in_transaction(Username) ->
    gen_server:call({global, ?MODULE}, {get_user_in_transaction, Username}).

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

get_user_info(Username, Fields) ->
    gen_server:call({global, ?MODULE}, {get_user_info, Username, Fields}).

block(Username, Blocked) ->
    gen_server:call({global, ?MODULE}, {user_block, Username, Blocked}).

unblock(Username, Unblocked) ->
    gen_server:call({global, ?MODULE}, {user_unblock, Username, Unblocked}).

get_blocked(Username) ->
    gen_server:call({global, ?MODULE}, {get_blocked, Username}).

add_media(Username, MediaType, Url) ->
    gen_server:call({global, ?MODULE}, {add_media, Username, MediaType, Url}).

get_media(Username, MediaType) ->
    gen_server:call({global, ?MODULE}, {get_media, Username, MediaType}).

%% INTERNAL HANDLERS

init([]) ->
    ?LOG_NOTICE("User server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({create_account, Username, Password, Email}, _From, State = #state{}) ->
    Res = userdb:insert(Username, Password, Email),
    ?LOG_INFO("User ~p was added", [Username]),
    {reply, Res, State};

handle_call({login, Email, Password}, _From, State = #state{}) ->
    Res = userdb:login(Email, Password),
    {reply, Res, State};

handle_call({set_user_info, Username, Fields, Values}, _From, State) ->
    Res = userdb:set_user_info(Username, Fields, Values),
    {reply, Res, State};

handle_call({get_user, Username}, _From, State) ->
    Res = userdb:get_user(Username),
    {reply, Res, State};

handle_call({get_user_in_transaction, Username}, _From, State) ->
    Res = userdb:get_user_in_transaction(Username),
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

handle_call({get_user_info, Username, Fields}, _From, State) ->
    Res = userdb:get_user_info(Username, Fields),
    {reply, Res, State};

handle_call({user_block, Username, Blocked}, _From, State) ->
    Res = userdb:block(Username, Blocked),
    {reply, Res, State};

handle_call({user_unblock, Username, Unblocked}, _From, State) ->
    Res = userdb:unblock(Username, Unblocked),
    {reply, Res, State};

handle_call({get_blocked, Username}, _From, State) ->
    Res = userdb:get_blocked(Username),
    {reply, Res, State};

handle_call({add_media, Username, MediaType, Url}, _From, State) ->
    Res = userdb:insert_media(Username, MediaType, Url),
    {reply, Res, State};

handle_call({get_media, Username, MediaType}, _From, State) ->
    Res = userdb:get_media(Username, MediaType),
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



