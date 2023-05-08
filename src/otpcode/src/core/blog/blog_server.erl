-module(blog_server).
-author("Zaryn Technologies").

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

%%API
-export([start_link/0, insert/3, delete_post/1, get_post/1, add_comment/3, update_comment/2,
get_single_comment/1, get_all_comments/1, delete_comment/2]). 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

insert(Author, Content, _Media) ->
    gen_server:call({global, ?MODULE}, {insert, Author, Content, _Media}).

delete_post(PostID) ->
    gen_server:call({global, ?MODULE}, {delete_post, PostID}).

get_post(PostID) ->
    gen_server:call({global, ?MODULE}, {get_post, PostID}).

add_comment(Author, PostID, Content) ->
    gen_server:call({global, ?MODULE}, {add_comment, Author, PostID, Content}).

update_comment(CommentID, NewContent) ->
    gen_server:call({global, ?MODULE}, {update_comment, CommentID, NewContent}).

get_single_comment(CommentId) ->
    gen_server:call({global, ?MODULE}, {get_single_comment, CommentId}).

get_all_comments(PostId) ->
    gen_server:call({global, ?MODULE}, {get_all_comments, PostId}).

delete_comment(CommentID, PostId) ->
    gen_server:call({global, ?MODULE}, {delete_comment, CommentID, PostId}).

init([]) ->
    ?LOG_NOTICE("Blog server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({insert, Author, Content, _Media}, _From, State) ->
    Id = blogdb:insert(Author, Content, _Media),
    {reply, Id, State};

handle_call({delete_post, PostID}, _From, State) ->
    Res = blogdb:delete_post(PostID),
    {reply, Res, State};

handle_call({get_post, PostID}, _From, State) ->
    Res = blogdb:get_post(PostID),
    {reply, Res, State};

handle_call({add_comment, Author, PostID, Content}, _From, State) ->
    Res = blogdb:add_comment(Author, PostID, Content),
    {reply, Res, State};

handle_call({update_comment, CommentID, NewContent}, _From, State) ->
    Res = blogdb:update_comment(CommentID, NewContent),
    {reply, Res, State};

handle_call({get_single_comment, CommentId}, _From, State) ->
    Res = blogdb:get_single_comment(CommentId),
    {reply, Res, State};

handle_call({get_all_comments, PostId}, _From, State) ->
    Res = blogdb:get_all_comments(PostId),
    {reply, Res, State};

handle_call({delete_comment, CommentID, PostId}, _From, State) ->
    Res = blogdb:delete_comment(CommentID, PostId),
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