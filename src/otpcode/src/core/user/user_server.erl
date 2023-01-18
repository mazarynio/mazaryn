-module(user_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0,
         create_account/3,login/2, insert_notif/2,
         get_user/1, get_user_in_transaction/1, get_users/0, delete_user/1,
         get_user_by_email/1, get_user_by_id/1, get_token_by_id/1, get_password/1,
         set_user_info/3, get_user_info/2,
         follow/2, unfollow/2,
         follow_multiple/2, unfollow_multiple/2,
         save_post/2, unsave_post/2, save_posts/2, unsave_posts/2,
         get_save_posts/1,
         change_username/3, change_password/3, change_email/3,
         get_following/1, get_follower/1,
         block/2, unblock/2, get_blocked/1, add_media/3, get_media/2,
         search_user_pattern/1, insert_avatar/2, insert_banner/2]).

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

insert_notif(UserID, Message) ->
    gen_server:call({global, ?MODULE}, {insert_notif, UserID, Message}).

add_media(Id, MediaType, Url) ->
    gen_server:call({global, ?MODULE}, {add_media, Id, MediaType, Url}).

insert_avatar(Id, AvatarUrl) ->
    gen_server:call({global, ?MODULE}, {insert_avatar, Id, AvatarUrl}).

insert_banner(Id, BannerUrl) ->
    gen_server:call({global, ?MODULE}, {insert_banner, Id, BannerUrl}).

set_user_info(Username, Fields, Values) ->
    gen_server:call({global, ?MODULE}, {set_user_info, Username, Fields, Values}).

get_media(Id, Type) ->
    gen_server:call({global, ?MODULE}, {get_media, Id, Type}).

get_user(Username) ->
    gen_server:call({global, ?MODULE}, {get_user, Username}).

get_user_in_transaction(Username) ->
    gen_server:call({global, ?MODULE}, {get_user_in_transaction, Username}).

get_users() ->
    gen_server:call({global, ?MODULE}, {get_users}).

get_password(Id) ->
    gen_server:call({global, ?MODULE}, {get_password, Id}).

get_user_by_email(Email) ->
    gen_server:call({global, ?MODULE}, {get_user_by_email, Email}).

get_user_by_id(Id) ->
    gen_server:call({global, ?MODULE}, {get_user_by_id, Id}).

get_token_by_id(TokenID) ->
  gen_server:call({global, ?MODULE}, {get_token_by_id, TokenID}).

change_password(Username, CurrentPass, NewPass) ->
    gen_server:call({global, ?MODULE}, {change_password, Username, CurrentPass, NewPass}).

change_email(Username, Password, NewEmail) ->
    gen_server:call({global, ?MODULE}, {change_email, Username, Password, NewEmail}).

change_username(Username, CurrentPass, NewUsername) ->
    gen_server:call({global, ?MODULE}, {change_username, Username, CurrentPass, NewUsername}).

delete_user(Username) ->
    gen_server:call({global, ?MODULE}, {delete_user, Username}).

follow(Id, Following) ->
    gen_server:call({global, ?MODULE}, {follow, Id, Following}).

unfollow(Id, Following) ->
    gen_server:call({global, ?MODULE}, {unfollow, Id, Following}).

follow_multiple(Id, Others) ->
    gen_server:call({global, ?MODULE}, {follow_multiple, Id, Others}).

unfollow_multiple(Id, Others) ->
    gen_server:call({global, ?MODULE}, {unfollow_multiple, Id, Others}).

save_post(Id, PostId) ->
    gen_server:call({global, ?MODULE}, {save_post, Id, PostId}).

unsave_post(Id, PostId) ->
    gen_server:call({global, ?MODULE}, {unsave_post, Id, PostId}).

save_posts(Id, PostIds) ->
    gen_server:call({global, ?MODULE}, {save_posts, Id, PostIds}).

unsave_posts(Id, PostIds) ->
    gen_server:call({global, ?MODULE}, {unsave_posts, Id, PostIds}).

get_save_posts(Id) ->
    gen_server:call({global, ?MODULE}, {get_save_posts, Id}).

get_following(Id) ->
    gen_server:call({global, ?MODULE}, {get_following, Id}).

get_follower(Id) ->
    gen_server:call({global, ?MODULE}, {get_follower, Id}).

get_user_info(Username, Fields) ->
    gen_server:call({global, ?MODULE}, {get_user_info, Username, Fields}).

block(Id, Blocked) ->
    gen_server:call({global, ?MODULE}, {user_block, Id, Blocked}).

unblock(Id, Unblocked) ->
    gen_server:call({global, ?MODULE}, {user_unblock, Id, Unblocked}).

get_blocked(Id) ->
    gen_server:call({global, ?MODULE}, {get_blocked, Id}).

search_user_pattern(Pattern) ->
    gen_server:call({global, ?MODULE}, {search_user_pattern, Pattern}).
%% INTERNAL HANDLERS

init([]) ->
    ?LOG_NOTICE("User server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({create_account, Username, Password, Email}, _From, State = #state{}) ->
    Res = userdb:insert(Username, Password, Email),
    ?LOG_INFO("User ~p was added", [Username]),
    {reply, Res, State};

handle_call({insert_notif, UserID, Message}, _From, State) ->
    Res = notifdb:insert(UserID, Message),
    {reply, Res, State};

handle_call({add_media, Id, MediaType, Url}, _From, State) ->
    Res = userdb:insert_media(Id, MediaType, Url),
    {reply, Res, State};

handle_call({insert_avatar, Id, AvatarUrl}, _From, State = #state{}) ->
    Res = userdb:insert_avatar(Id, AvatarUrl),
    {reply, Res, State};

handle_call({insert_banner, Id, BannerUrl}, _From, State = #state{}) ->
    Res = userdb:insert_banner(Id, BannerUrl),
    {reply, Res, State};

handle_call({login, Email, Password}, _From, State = #state{}) ->
    Res = userdb:login(Email, Password),
    {reply, Res, State};

handle_call({set_user_info, Username, Fields, Values}, _From, State) ->
    Res = userdb:set_user_info(Username, Fields, Values),
    {reply, Res, State};

handle_call({get_media, Id, Type}, _From, State) ->
    Res = userdb:get_media(Id, Type),
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

handle_call({get_follower, Id}, _From, State) ->
    Res = userdb:get_follower(Id),
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

handle_call({search_user_pattern, Pattern}, _From, State) ->
    Res = userdb:search_user_pattern(Pattern),
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
