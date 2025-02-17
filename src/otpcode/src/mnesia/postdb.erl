-module(postdb).
-author("Zaryn Technologies").
-export([insert/7, get_post_by_id/1, get_post_content_by_id/1,
         modify_post/7, get_posts_by_author/1, get_posts_content_by_author/1,
         get_posts_by_hashtag/1, update_post/2,
         delete_post/1, get_posts/0,
         get_all_posts_from_date/4, get_all_posts_from_month/3,
         like_post/2, unlike_post/2, add_comment/3, update_comment/2, like_comment/2, unlike_comment/2, get_comment_likes/1, reply_comment/3,
          get_reply/1, get_all_replies/1, get_all_comments/1, delete_comment/2, delete_comment_from_mnesia/1, get_likes/1,
         get_single_comment/1, get_media/1, report_post/4, update_activity/2]).
-export([get_comments/0]).

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% if post or comment do not have media,
%% their value in record are nil

insert(Author, Content, Emoji, Media, Hashtag, Mention, Link_URL) ->  
  F = fun() ->
          Id = nanoid:gen(),
          Date = calendar:universal_time(),
          AI_Post_ID = ai_postdb:insert(Id),
          %Device = device:nif_device_info(),
          mnesia:write(#post{id = Id,
                             ai_post_id = AI_Post_ID,
                             content = erl_deen:main(Content),
                             emoji = Emoji,
                             author = Author,
                             media = Media,
                             hashtag = Hashtag,
                             mention = Mention,
                             link_url = Link_URL,
                             date_created = Date}),
                             %device_info = Device}),
          [User] = mnesia:index_read(user, Author, username),
          Posts = User#user.post,
          mnesia:write(User#user{post = [Id | Posts]}),
          update_activity(Author, Date),
          Id
      end,
  {atomic, Res} = mnesia:transaction(F),
  Res.

modify_post(Author, NewContent, NewEmoji, NewMedia, NewHashtag, NewMention, NewLink_URL) ->
  Fun = fun() ->
            [Post] = mnesia:read({post, Author}),
            mnesia:write(Post#post{content = erl_deen:main(NewContent),
                                   emoji = NewEmoji,
                                   media = NewMedia,
                                   hashtag = NewHashtag,
                                   mention = NewMention,
                                   link_url = NewLink_URL,
                                   date_updated = calendar:universal_time()})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

%% Get post by PostID
get_post_by_id(Id) ->
  Res = mnesia:transaction(
          fun() ->
              mnesia:match_object(#post{id = Id, _= '_'})
          end),
  case Res of
    {atomic, []} -> post_not_exist;
    {atomic, [Post]} -> Post;
    _ -> error
  end.

get_post_content_by_id(Id) -> 
    Fun = fun() ->
              [Post] = mnesia:read({post, Id}),
              Post#post.content
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

%% get_posts_by_author(Username)
get_posts_by_author(Author) ->
  Fun = fun() ->
            mnesia:match_object(#post{author = Author,
                                      _ = '_'})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_posts_content_by_author(Author) ->
    Fun = fun() ->
              Posts = mnesia:match_object(#post{author = Author, _ = '_'}),
              [Post#post.content || Post <- Posts]
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_posts_by_hashtag(Hashtag) ->
  Fun = fun() ->
            mnesia:match_object(#post{hashtag = Hashtag,
                                      _ = '_'})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

update_post(PostId, NewContent) ->
  Fun = fun() ->
            [Post] = mnesia:read({post, PostId}),
            Content = Post#post.content,
            UpdatedContent =
              case NewContent of
                [{Key, Value} | _] ->  % Handle key-value pairs
                  lists:foldl(fun({K, V}, Acc) ->
                    case lists:keymember(K, 1, Acc) of
                      true ->
                        lists:keyreplace(K, 1, Acc, {K, V});
                      false ->
                        [{K, V} | Acc]
                    end
                  end, Content, NewContent);
                _ ->  % Handle raw string
                  NewContent
              end,
            mnesia:write(Post#post{content = UpdatedContent})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

%% delete_post(PostID)
delete_post(Id) ->
  F = fun() ->
      %% Check if the post exists
      case mnesia:read({post, Id}) of
          [] -> 
              {error, post_not_found};  %% Return error if post doesn't exist
          _ ->
              %% Delete the post
              mnesia:delete({post, Id}),
              ok  %% Return success after deleting
      end
  end,

  case mnesia:activity(transaction, F) of
      ok -> 
          ok;  %% Return success message
      {error, post_not_found} -> 
          {error, post_not_found};  %% Handle case when the post does not exist
      {aborted, Reason} -> 
          {error, transaction_failed, Reason}  %% Handle aborted transactions
  end.


%% Get all posts
get_posts() ->
  Fun = fun() ->
            mnesia:all_keys(post)
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

%%% {2022,05,01}
%%% User = [], select all users
%%% USer = dat for particular
get_all_posts_from_date(Year, Month, Date, Author) ->
  DateTime = {Year, Month, Date},
  Object =
  case Author of
    [] -> #post{date_created = {DateTime, '_'},
                _ = '_'};
    Author -> #post{date_created = {DateTime, '_'},
                    author = Author,
                    _ = '_'}
  end,
  {atomic, Res} = mnesia:transaction(fun() -> mnesia:match_object(Object) end),
  Res.

get_all_posts_from_month(Year, Month, Author) ->
  Object =
  case Author of
    [] -> #post{date_created = {{Year, Month, '_'}, '_'},
                _ = '_'};
    Author -> #post{date_created = {{Year, Month, '_'}, '_'},
                    author = Author,
                    _ = '_'}
  end,
  {atomic, Res} = mnesia:transaction(fun() -> mnesia:match_object(Object) end),
  Res.

%% like_post(MyID, PostID)
like_post(UserID, PostId) ->  
  Fun = fun() ->
            ID = nanoid:gen(),
            mnesia:write(#like{id = ID,
                               post = PostId,
                               userID = UserID,
                               date_created = calendar:universal_time()}),
            [Post] = mnesia:read({post, PostId}),
            Likes = Post#post.likes,
            mnesia:write(Post#post{likes = [ID|Likes]}),
            ID
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

unlike_post(LikeID, PostId) ->  
  Fun = fun() -> 
            [Post] = mnesia:read(post, PostId),
            Unlike = lists:delete(LikeID, Post#post.likes),
            mnesia:write(Post#post{likes = Unlike,
                                   date_created = calendar:universal_time()})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

%% Content = [{text, Text}, {media, Media}, {mention, Name}, {like, Like}]
add_comment(Author, PostID, Content) ->
  Fun = fun() ->
      %% Check if the post exists
      case mnesia:read({post, PostID}) of
          [] -> 
              {error, post_not_found}; 
          [Post] ->
              Id = nanoid:gen(),

              Comment = #comment{
                  id = Id,
                  post = PostID,
                  author = Author,
                  content = Content,
                  date_created = calendar:universal_time()
              },
              mnesia:write(Comment),

              UpdatedComments = [Id | Post#post.comments],
              UpdatedPost = Post#post{
                  comments = UpdatedComments
              },

              mnesia:write(UpdatedPost),
              Id 
      end
  end,

  case mnesia:transaction(Fun) of
      {atomic, Id} -> 
          Id;  %% Return the ID of the newly added comment
      {atomic, {error, Reason}} -> 
          {error, Reason};  
      {aborted, Reason} -> 
          {error, transaction_failed, Reason}  
  end.


update_comment(CommentID, NewContent) ->
  Fun = fun() ->
            [Comment] = mnesia:read({comment, CommentID}),
            mnesia:write(Comment#comment{content = NewContent}),
            CommentID
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

like_comment(UserID, CommentId) ->  
  Fun = fun() ->
            ID = nanoid:gen(),
            mnesia:write(#like{id = ID,
                               post = CommentId,
                               userID = UserID,
                               date_created = calendar:universal_time()}),
            [Comment] = mnesia:read({comment, CommentId}),
            Likes = Comment#comment.likes,
            mnesia:write(Comment#comment{likes = [ID|Likes]}),
            ID
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

%unlike_comment(LikeID, CommentId) ->  
%  Fun = fun() -> 
%            [Comment] = mnesia:read(comment, CommentId),
%            Unlike = lists:delete(LikeID, Comment#comment.likes),
%            mnesia:write(Comment#comment{likes = Unlike,
%                                   date_created = calendar:universal_time()})
%        end,
%  {atomic, Res} = mnesia:transaction(Fun),
%  Res.
unlike_comment(LikeID, CommentId) ->  
    Fun = fun() -> 
        case mnesia:read(comment, CommentId) of
            [] -> 
                {error, comment_not_found};
            [Comment] ->
                Unlike = lists:delete(LikeID, Comment#comment.likes),
                mnesia:write(Comment#comment{
                    likes = Unlike,
                    date_created = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, Reason}
    end.
get_comment_likes(CommentID) -> 
  Fun = fun() ->
            case mnesia:read({comment, CommentID}) of
                [] -> 
                    [];  % Return an empty list if the comment doesn't exist
                [Comment] ->
                    lists:foldl(fun(ID, Acc) ->
                                    [Like] = mnesia:read({like, ID}),
                                    [Like|Acc]
                                end,
                                [], Comment#comment.likes)
            end
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

reply_comment(UserID, CommentID, Content) ->
  Fun = fun() ->
      %% Check if the post exists
      case mnesia:read({comment, CommentID}) of
          [] -> 
              {error, comment_not_found}; 
          [Comment] ->
              Id = nanoid:gen(),

              Reply = #reply{
                  id = Id,
                  comment = CommentID,
                  userID = UserID,
                  content = Content,
                  date_created = calendar:universal_time()
              },
              mnesia:write(Reply),

              UpdatedReplies = [Id | Comment#comment.replies],
              UpdatedComment = Comment#comment{
                  replies = UpdatedReplies
              },

              mnesia:write(UpdatedComment),
              Id 
      end
  end,

  case mnesia:transaction(Fun) of
      {atomic, Id} -> 
          Id;  %% Return the ID of the newly added comment
      {atomic, {error, Reason}} -> 
          {error, Reason};  
      {aborted, Reason} -> 
          {error, transaction_failed, Reason}  
  end.

get_reply(ReplyID) ->
  Fun = fun() ->
    [Reply] = mnesia:read({reply, ReplyID}),
    Reply 
  end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_all_replies(CommentID) ->
  Fun = fun() ->
            mnesia:match_object(#reply{comment = CommentID,
                                         _ = '_'}),
            [Comment] = mnesia:read({comment, CommentID}),
            lists:foldl(fun(Id, Acc) ->
                            [Reply] = mnesia:read({reply, Id}),
                            [Reply|Acc]
                        end,
                        [], Comment#comment.replies)
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_single_comment(CommentId) ->
  Fun = fun() ->
            [Comment] = mnesia:read({comment, CommentId}),
            Comment
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

%% Get all Comments for Specific Post using PostID
get_all_comments(PostId) ->
  Fun = fun() ->
            mnesia:match_object(#comment{post = PostId,
                                         _ = '_'}),
            [Post] = mnesia:read({post, PostId}),
            lists:foldl(fun(Id, Acc) ->
                            [Comment] = mnesia:read({comment, Id}),
                            [Comment|Acc]
                        end,
                        [], Post#post.comments)
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

%% Get all posts
get_comments() ->
  Fun = fun() ->
            mnesia:all_keys(comment)
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.


delete_comment(CommentID, PostId) ->
    Fun = fun() -> 
        %% Check if the post exists
        case mnesia:read(post, PostId) of
            [] -> 
                {error, post_not_found}; 
            [Post] ->
                %% Check if the comment exists in the post
                case lists:member(CommentID, Post#post.comments) of
                    false -> 
                        {error, comment_not_found}; 
                    true -> 
                        %% Remove the comment from the post's comment list
                        UpdatedComments = lists:delete(CommentID, Post#post.comments),
                        UpdatedPost = Post#post{
                            comments = UpdatedComments,
                            date_created = calendar:universal_time()
                        },
                        %% Write the updated post back to the database
                        mnesia:write(UpdatedPost),
                        
                        %% Delete the comment from the `comment` table
                        mnesia:delete({comment, CommentID}),
                        
                        {ok, comment_deleted}
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, comment_deleted}} -> 
            {ok, comment_deleted};  
        {atomic, {error, Reason}} -> 
            {error, Reason}; 
        {aborted, Reason} -> 
            {error, transaction_failed, Reason}  
    end.



delete_comment_from_mnesia(CommentID) ->
  Fun = fun() -> 
    mnesia:delete({comment, CommentID})
  end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_likes(PostID) ->
  Fun = fun() ->
            mnesia:match_object(#like{post = PostID,
                                      _ = '_'}),
            [Post] = mnesia:read({post, PostID}),
            lists:foldl(fun(ID, Acc) ->
                            [Like] = mnesia:read({like, ID}),
                            [Like|Acc]
                        end,
                        [], Post#post.likes)
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_media(Media) ->
  case Media of
    nil -> nil;
    Media -> [Media]  %% media link here
  end.
% Report Post
report_post(MyID, PostID, Type, Description) ->
  Fun = fun() ->
    ID = nanoid:gen(),
    mnesia:read(post, PostID),
        Report = #report{
          id = ID,
          type = Type,
          description = Description,
          reporter = MyID,
          post = PostID,
          date_created = calendar:universal_time()},
        mnesia:write(Report),
        ID
  end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

update_activity(Author, Date) ->
  User = userdb:get_user(Author),
  UserID = User#user.id,
  LastActivity = userdb:update_last_activity(UserID, Date),
  LastActivity.


