%%%-------------------------------------------------------------------
%%% @author Mazaryn 
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. May 2022 9:45 PM
%%%-------------------------------------------------------------------
-module(post_server).
-author("Zaryn Technologies"). 
-define(NUM, 5).
-include_lib("kernel/include/logger.hrl"). 

-behaviour(gen_server).
%% API
-export([start_link/0, insert/7, get_post_by_id/1, get_post_content_by_id/1, modify_post/7,
         get_posts_by_author/1, get_posts_by_user_id/1, get_posts_content_by_author/1, get_posts_by_hashtag/1, get_latest_posts/1, update_post/2,
         delete_post/1, like_post/2, unlike_post/2, add_comment/3, get_posts_content_by_user_id/1,
         update_comment/2, like_comment/2, get_comment_likes/1, reply_comment/3, delete_reply/1, get_reply/1, get_all_replies/1,
         get_single_comment/1, get_all_comments/1, delete_comment/2, get_likes/1,
         get_media/1, get_posts/0,
         get_all_posts_from_date/4, get_all_posts_from_month/3]).

-export([save_post/2, unsave_post/2,
         save_posts/2, unsave_posts/2,
         get_save_posts/1, report_post/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3]).

start_link() ->
  ?LOG_NOTICE("Post server has been started - ~p", [self()]),
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

insert(Author, Content, Emoji, Media, Hashtag, Mention, Link_URL) ->
  gen_server:call({global, ?MODULE}, {insert, Author, Content, Emoji, Media, Hashtag, Mention, Link_URL}).

modify_post(Author, NewContent, NewEmoji, NewMedia, NewHashtag, NewMention, NewLink_URL) ->
  gen_server:call({global, ?MODULE},
     {modify_post, Author, NewContent, NewEmoji, NewMedia, NewHashtag, NewMention, NewLink_URL}).

get_post_by_id(Id) ->
  gen_server:call({global, ?MODULE}, {get_post_by_id, Id}).

get_post_content_by_id(Id) ->
  gen_server:call({global, ?MODULE}, {get_post_content_by_id, Id}).

get_posts_by_author(Author) ->
  gen_server:call({global, ?MODULE}, {get_posts_by_author, Author}).

get_posts_by_user_id(UserID) ->
  gen_server:call({global, ?MODULE}, {get_posts_by_user_id, UserID}).

get_posts_content_by_author(Author) ->
  gen_server:call({global, ?MODULE}, {get_posts_content_by_author, Author}).

get_posts_content_by_user_id(UserID) ->
  gen_server:call({global, ?MODULE}, {get_posts_content_by_user_id, UserID}).

get_posts_by_hashtag(Hashtag) ->
  gen_server:call({global, ?MODULE}, {get_posts_by_hashtag, Hashtag}).

get_latest_posts(Author) ->
    gen_server:call({global, ?MODULE}, {get_latest_posts, Author}).

update_post(PostId, NewContent) ->
    gen_server:call({global, ?MODULE}, {update_post, PostId, NewContent}).

delete_post(Id) ->
  gen_server:call({global, ?MODULE}, {delete_post, Id}).

like_post(UserID, PostId) ->
    gen_server:call({global, ?MODULE}, {like_post, UserID, PostId}).

unlike_post(LikeID, PostId) ->
    gen_server:call({global, ?MODULE}, {unlike_post, LikeID, PostId}).

add_comment(Author, PostID, Content) ->
  gen_server:call({global, ?MODULE}, {add_comment, Author, PostID, Content}).

update_comment(CommentID, NewContent) ->
    gen_server:call({global, ?MODULE}, {update_comment, CommentID, NewContent}).

like_comment(UserID, CommentID) ->
    gen_server:call({global, ?MODULE}, {like_comment, UserID, CommentID}). 

get_comment_likes(CommentID) ->
    gen_server:call({global, ?MODULE}, {get_comment_likes, CommentID}). 

reply_comment(UserID, CommentID, Content) ->
    gen_server:call({global, ?MODULE}, {reply_comment, UserID, CommentID, Content}).

delete_reply(ReplyID) ->
    gen_server:call({global, ?MODULE}, {delete_reply, ReplyID}).

get_reply(ReplyID) ->
    gen_server:call({global, ?MODULE}, {get_reply, ReplyID}).

get_all_replies(CommentID) ->
    gen_server:call({global, ?MODULE}, {get_all_replies, CommentID}).

get_single_comment(CommentId) ->
    gen_server:call({global, ?MODULE}, {get_single_comment, CommentId}).

get_all_comments(PostId) ->
    gen_server:call({global, ?MODULE}, {get_all_comments, PostId}).

delete_comment(CommentID, PostId) ->
    gen_server:call({global, ?MODULE}, {delete_comment, CommentID, PostId}).

get_likes(PostID) ->
    gen_server:call({global, ?MODULE}, {get_likes, PostID}).

get_media(Media) ->
    gen_server:call({global, ?MODULE}, {get_media, Media}).

get_posts() ->
    gen_server:call({global, ?MODULE}, {get_posts}).

get_all_posts_from_date(Year, Month, Date, Author) ->
  gen_server:call({global, ?MODULE}, {get_all_posts_from_date, Year, Month, Date, Author}).

get_all_posts_from_month(Year, Month, Author) ->
  gen_server:call({global, ?MODULE}, {get_all_posts_from_month, Year, Month, Author}).

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

report_post(MyID, PostID, Type, Description) ->
    gen_server:call({global, ?MODULE}, {report_post, MyID, PostID, Type, Description}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%
init([]) ->
    {ok, []}.


handle_call({insert, Author, Content, Emoji, Media, Hashtag, Mention, Link_URL}, _From, State) ->
    Id = postdb:insert(Author, Content, Emoji, Media, Hashtag, Mention, Link_URL),
    {reply, Id, State};

handle_call({modify_post, Author, NewContent, NewEmoji, NewMedia, NewHashtag, NewMention, NewLink_URL},
             _From, State) ->
  Res = postdb:modify_post(Author, NewContent, NewEmoji, NewMedia, NewHashtag, NewMention, NewLink_URL),
  {reply, Res, State};

handle_call({get_post_by_id, Id}, _From, State) ->
    Posts = postdb:get_post_by_id(Id),
    {reply, Posts, State};

handle_call({get_post_content_by_id, Id}, _From, State) ->
    Posts = postdb:get_post_content_by_id(Id),
    {reply, Posts, State};

handle_call({get_posts_by_author, Author}, _From, State) ->
    Posts = postdb:get_posts_by_author(Author),
    {reply, Posts, State};

handle_call({get_posts_by_user_id, UserID}, _From, State) ->
    Posts = postdb:get_posts_by_user_id(UserID),
    {reply, Posts, State};

handle_call({get_posts_content_by_author, Author}, _From, State) ->
    Posts = postdb:get_posts_content_by_author(Author),
    {reply, Posts, State};

handle_call({get_posts_by_hashtag, Hashtag}, _From, State) ->
    Posts = postdb:get_posts_by_hashtag(Hashtag),
    {reply, Posts, State};

handle_call({get_latest_posts, Author}, _From, State) ->
    AllPosts = post_server:get_posts_by_author(Author),
    LatestPosts = lists:sublists(AllPosts, ?NUM),
    {reply, LatestPosts, State};

handle_call({update_post, PostId, NewContent}, _From, State) ->
    postdb:update_post(PostId, NewContent),
    {reply, ok, State};

handle_call({delete_post, Id}, _From, State) ->
    postdb:delete_post(Id),
    {reply, ok, State};

handle_call({like_post, UserID, PostId}, _From, State) ->
    Res = postdb:like_post(UserID, PostId),
    {reply, Res, State};

handle_call({unlike_post, LikeID, PostId}, _From, State) ->
    Res = postdb:unlike_post(LikeID, PostId),
    {reply, Res, State};

handle_call({add_comment, Author, PostID, Content}, _From, State) ->
    Id = postdb:add_comment(Author, PostID, Content),
    {reply, Id, State};

handle_call({update_comment, CommentID, NewContent}, _From, State) ->
    postdb:update_comment(CommentID, NewContent),
    {reply, ok, State};

handle_call({like_comment, UserID, CommentID}, _From, State) ->
    ID = postdb:like_comment(UserID, CommentID),
    {reply, ID, State};

handle_call({get_comment_likes, CommentID}, _From, State) ->
    IDs = postdb:get_comment_likes(CommentID),
    {reply, IDs, State};

handle_call({reply_comment, UserID, CommentID, Content}, _From, State) ->
    ID = postdb:reply_comment(UserID, CommentID, Content),
    {reply, ID, State};

handle_call({delete_reply, ReplyID}, _From, State) ->
    postdb:delete_reply(ReplyID),
    {reply, ok, State};

handle_call({get_reply, ReplyID}, _From, State) ->
    ID = postdb:get_reply(ReplyID),
    {reply, ID, State};

handle_call({get_all_replies, CommentID}, _From, State) ->
    IDs = postdb:get_all_replies(CommentID),
    {reply, IDs, State};

handle_call({get_single_comment, CommentId}, _From, State) ->
    Comment = postdb:get_single_comment(CommentId),
    {reply, Comment, State};

handle_call({get_all_comments, PostId}, _From, State) ->
    postdb:get_all_comments(PostId),
    {reply, ok, State};

handle_call({delete_comment, CommentID, PostId}, _From, State) ->
    postdb:delete_comment(CommentID, PostId),
    {reply, ok, State};

handle_call({get_likes, PostID}, _From, State) ->
    Res = postdb:get_likes(PostID),
    {reply, Res, State};

handle_call({get_media, Media}, _From, State) ->
    postdb:get_media(Media),
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

handle_call({report_post, MyID, PostID, Type, Description}, _From, State) ->
    Res = postdb:report_post(MyID, PostID, Type, Description),
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
