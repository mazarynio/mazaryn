%%%-------------------------------------------------------------------
%%% @author dhuynh
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. May 2022 9:45 PM
%%%-------------------------------------------------------------------
-module(post_server).
-author("dhuynh").

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).
%% API
-export([start_link/0, insert/3, get_post_by_id/1, modify_post/3,
         get_posts_by_author/1, delete_post/1,add_comment/3, get_posts/0,
         get_all_posts_from_date/4, get_all_posts_from_month/3,
         get_comments/1]).

-export([save_post/2, unsave_post/2,
         save_posts/2, unsave_posts/2,
         get_save_posts/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3]).

start_link() ->
  ?LOG_NOTICE("Post server has been started - ~p", [self()]),
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

insert(Author, Content, Media) ->
  gen_server:call({global, ?MODULE}, {insert, Author, Content, Media}).

modify_post(Id, Username, NewContent) ->
  gen_server:call({global, ?MODULE}, {modify_post, Id, Username, NewContent}).

get_post_by_id(Id) ->
  gen_server:call({global, ?MODULE}, {get_post_by_id, Id}).

get_posts_by_author(Author) ->
  gen_server:call({global, ?MODULE}, {get_posts_by_author, Author}).

delete_post(Id) ->
  gen_server:call({global, ?MODULE}, {delete_post, Id}).

add_comment(Id, Username, Comment) ->
  gen_server:call({global, ?MODULE}, {add_comment, Id, Username, Comment}).

get_posts() ->
    gen_server:call({global, ?MODULE}, {get_posts}).

get_all_posts_from_date(Year, Month, Date, Author) ->
  gen_server:call({global, ?MODULE}, {get_all_posts_from_date, Year, Month, Date, Author}).

get_all_posts_from_month(Year, Month, Author) ->
  gen_server:call({global, ?MODULE}, {get_all_posts_from_month, Year, Month, Author}).

get_comments(Id) ->
  gen_server:call({global, ?MODULE}, {get_comments, Id}).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%
init([]) ->
    {ok, []}.


handle_call({insert, Author, Content, Media}, _From, State) ->
    Id = postdb:insert(Author, Content, Media),
    {reply, Id, State};

handle_call({modify_post, Id, Username, NewContent}, _From, State) ->
  Res = postdb:modify_post(Id, Username, NewContent),
  {reply, Res, State};

handle_call({get_post_by_id, Id}, _From, State) ->
    Posts = postdb:get_post_by_id(Id),
    {reply, Posts, State};

handle_call({get_posts_by_author, Author}, _From, State) ->
    Posts = postdb:get_posts_by_author(Author),
    {reply, Posts, State};

handle_call({add_comment, Id, Username, Comment}, _From, State) ->
    postdb:add_comment(Id, Username, Comment),
    {reply, ok, State};

handle_call({get_posts}, _From, State) ->
    Res = postdb:get_posts(),
    {reply, Res, State};

handle_call({get_all_posts_from_date, Year, Month, Date, Author}, _From, State) ->
    Posts = postdb:get_all_posts_from_date(Year, Month, Date, Author),
    {reply, Posts, State};

handle_call({get_all_posts_from_month, Year, Month, Author}, _From, State) ->
    Posts = postdb:get_all_posts_from_month(Year, Month, Author),
    {reply, Posts, State};

handle_call({get_comments, Id}, _From, State) ->
    Comments = postdb:get_comments(Id),
    {reply, Comments, State};

%% Save post for reading alter
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
