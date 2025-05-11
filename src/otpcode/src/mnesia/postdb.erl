-module(postdb).
-author("Zaryn Technologies").
-export([insert/7, get_post_by_id/1, get_post_content_by_id/1,
         modify_post/8, get_posts_by_author/1, get_posts_by_user_id/1, get_posts_content_by_author/1, get_posts_content_by_user_id/1,
         get_posts_by_hashtag/1, update_post/2, get_last_50_posts_content_by_user_id/1, get_last_50_comments_for_user/1,
         delete_post/1, get_posts/0, delete_reply_from_mnesia/1, get_all_comments_by_user_id/2, get_user_by_single_comment/1, get_last_50_comments_content_for_user/1,
         get_all_posts_from_date/4, get_all_posts_from_month/3, get_all_comments_for_user/1, get_all_likes_for_user/1, get_last_50_likes_for_user/1,
         like_post/2, unlike_post/2, add_comment/3, update_comment/2, like_comment/2, update_comment_likes/2, get_comment_likes/1, get_comment_replies/1, reply_comment/3,
          get_reply/1, get_all_replies/1, delete_reply/1, get_all_comments/1, delete_comment/2, delete_comment_from_mnesia/1, get_likes/1,
         get_single_comment/1, get_media/1, report_post/4, update_activity/2, get_user_id_by_post_id/1, get_post_ipns_by_id/1, get_post_ipfs_by_ipns/1,
         pin_post/1, get_comment_content/1, get_reply_content/1]).
-export([get_comments/0]).

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

insert(Author, Content, Media, Hashtag, Link_URL, Emoji, Mention) ->  
    Fun = fun() ->
        Id = nanoid:gen(),
        Date = calendar:universal_time(),
        AI_Post_ID = ai_postdb:insert(Id),
        UserID = userdb:get_user_id(Author),
        [User] = mnesia:index_read(user, Author, #user.username),
        
        ContentToCache = if
            is_binary(Content) -> binary_to_list(Content);
            true -> Content
        end,
        
        ok = content_cache:set(Id, ContentToCache),
        
        MediaForCache = if 
            Media =:= undefined -> undefined;
            Media =:= "" -> "";
            true -> Media
        end,
        ok = content_cache:set({media, Id}, MediaForCache),
        
        PlaceholderContent = Id, 
        PlaceholderMedia = {media, Id}, 
        
        mnesia:write(#post{
            id = Id,
            ai_post_id = AI_Post_ID,
            user_id = UserID,
            content = PlaceholderContent, 
            emoji = Emoji,
            author = Author,
            media = PlaceholderMedia, 
            hashtag = Hashtag,
            mention = Mention,
            link_url = Link_URL,
            comments = [], 
            likes = [],
            date_created = Date
        }),
        Posts = User#user.post,
        mnesia:write(User#user{post = [Id | Posts]}),
        update_activity(Author, Date),
        
        {ok, Id}
    end,
  
    case mnesia:transaction(Fun) of
        {atomic, {ok, Id}} -> 
            spawn(fun() ->
                ContentToUse = content_cache:get(Id),
                CIDString = case ContentToUse of
                    "" -> ""; 
                    _ -> ipfs_content:upload_text(ContentToUse)
                end,
                
                MediaToUse = content_cache:get({media, Id}),
                MediaCID = case MediaToUse of
                    undefined -> undefined;
                    "" -> "";
                    _ -> 
                        case ipfs_media:upload_media(MediaToUse) of
                            {error, Reason} -> 
                                error_logger:error_msg("Failed to upload media: ~p", [Reason]),
                                undefined;
                            CID when is_list(CID) -> CID;
                            CID when is_binary(CID) -> binary_to_list(CID);
                            Other ->
                                error_logger:error_msg("Unexpected result from upload_media: ~p", [Other]),
                                undefined
                        end
                end,
                UpdateF = fun() ->
                    case mnesia:read({post, Id}) of
                        [Post] ->
                            UpdatedPost = Post#post{
                                content = CIDString,
                                media = MediaCID
                            },
                            mnesia:write(UpdatedPost);
                        [] -> ok
                    end
                end,
                mnesia:transaction(UpdateF),
                
                content_cache:delete(Id),
                content_cache:delete({media, Id}),
                
                spawn(fun() ->
                    timer:sleep(15000),
                    
                    PostCID = case CIDString of
                        "" -> MediaCID; 
                        _ -> CIDString  
                    end,  
                    case PostCID of
                        undefined -> 
                            error_logger:info_msg("No content to publish to IPNS for post ~p", [Id]);
                        "" -> 
                            error_logger:info_msg("Empty content, skipping IPNS publish for post ~p", [Id]);
                        _ ->
                            try
                                {ok, #{id := _KeyID, name := _BinID}} = ipfs_client_4:key_gen(Id),
                                
                                PublishOptions = [
                                    {key, Id},
                                    {resolve, true},         
                                    {lifetime, "24h0m0s"},   
                                    {ttl, "2m0s"},           
                                    {v1compat, true},        
                                    {ipns_base, "base36"}    
                                ],
                                
                                case ipfs_client_5:name_publish(
                                    "/ipfs/" ++ PostCID,
                                    PublishOptions
                                ) of
                                    {ok, #{name := IPNSKey}} ->
                                        update_post_ipns(Id, IPNSKey);
                                    {error, _Reason} ->
                                        error_logger:error_msg("IPNS publish failed for post ~p: ~p", [Id, _Reason]),
                                        err 
                                end
                            catch
                                Exception:Error:Stacktrace ->
                                    error_logger:error_msg(
                                        "Exception while publishing to IPNS for post ~p: ~p:~p~n~p", 
                                        [Id, Exception, Error, Stacktrace]
                                    )
                            end
                    end
                end)
            end),
            
            Id;
        {atomic, {error, Reason}} -> 
            {error, Reason};
        {aborted, Reason} -> 
            {error, {transaction_failed, Reason}}
    end.

update_post_ipns(PostId, IPNSKey) ->
    UpdateF = fun() ->
        case mnesia:read({post, PostId}) of
            [Post] ->
                UpdatedPost = Post#post{ipns = IPNSKey},
                mnesia:write(UpdatedPost),
                ok;
            [] ->
                {error, not_found}
        end
    end,
    
    case mnesia:transaction(UpdateF) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> 
            error_logger:error_msg("Failed to update post ~p with IPNS: ~p", [PostId, Reason]),
            {error, Reason};
        {aborted, Reason} ->
            error_logger:error_msg("Transaction aborted while updating post ~p with IPNS: ~p", [PostId, Reason]),
            {error, {transaction_failed, Reason}}
    end.

modify_post(PostId, Author, NewContent, NewEmoji, NewMedia, NewHashtag, NewMention, NewLink_URL) ->
    F = fun() ->
        case mnesia:read({post, PostId}) of
            [] -> 
                error;
            [Post] ->
                case Post#post.author =:= Author of
                    false -> 
                        unauthorized;
                    true ->
                        ContentToUse = if
                            is_binary(NewContent) -> binary_to_list(NewContent);
                            true -> NewContent
                        end,
                        CIDString = ipfs_content:upload_text(ContentToUse),
                        
                        MediaCID = case NewMedia of
                            undefined -> Post#post.media;
                            _ -> ipfs_media:upload_media(NewMedia)
                        end,
                        
                        UpdatedPost = Post#post{
                            content = CIDString,
                            emoji = NewEmoji,
                            media = MediaCID,
                            hashtag = NewHashtag,
                            mention = NewMention,
                            link_url = NewLink_URL,
                            date_updated = calendar:universal_time()
                        },
                        mnesia:write(UpdatedPost),
                        ok
                end
        end
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {atomic, unauthorized} -> {error, unauthorized};
        _ -> error
    end.

%% Get post by PostID
get_post_by_id(Id) ->
  Fun = fun() ->
      case mnesia:read({post, Id}) of
          [] -> post_not_exist;
          [Post] -> Post
      end
  end,
  case mnesia:transaction(Fun) of
      {atomic, Result} -> Result;
      {aborted, Reason} -> {error, {transaction_failed, Reason}}
  end.

get_user_id_by_post_id(PostId) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#post{id = PostId, _ = '_'})
            end),
    case Res of
        {atomic, []} -> post_not_exist;
        {atomic, [#post{user_id = UserId}]} -> UserId;
        _ -> error
    end.

get_post_ipns_by_id(Id) -> 
    Fun = fun() ->
              case mnesia:read({post, Id}) of
                [Post] ->
                  try
                    IPNSString = Post#post.content,
                    {ok, IPNSString}
                  catch
                    _:Error -> {error, Error}
                  end;
                [] -> {error, post_not_found}
              end
          end,
    case mnesia:transaction(Fun) of
      {atomic, {ok, IPNSString}} -> IPNSString;
      {atomic, {error, Reason}} -> {error, Reason};
      Error -> Error
    end.
  
get_post_content_by_id(PostID) ->
    Fun = fun() ->
        case mnesia:read({post, PostID}) of
            [] -> {error, post_not_found};
            [Post] -> 
                Content = Post#post.content,
                case Content of
                    ID when ID =:= PostID -> 
                        case content_cache:get(ID) of
                            undefined -> {error, content_not_ready};
                            CachedContent -> {ok, CachedContent}
                        end;
                    _ ->
                        try
                            ActualContent = ipfs_content:get_text_content(Content),
                            {ok, ActualContent}
                        catch
                            _:Error -> {error, Error}
                        end
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, Content}} -> Content;
        {atomic, {error, Reason}} -> {error, Reason};
        Error -> Error
    end.

get_media(PostID) ->
    Fun = fun() ->
        case mnesia:read({post, PostID}) of
            [] -> {error, post_not_found};
            [Post] -> 
                Media = Post#post.media,
                case Media of
                    {media, ID} when ID =:= PostID -> 
                        case content_cache:get({media, ID}) of
                            undefined -> {error, media_not_ready};
                            "" -> {ok, ""}; 
                            CachedMedia -> {ok, CachedMedia}
                        end;
                    nil -> {ok, nil};
                    "" -> {ok, ""};
                    _ ->
                        try
                            ActualMedia = ipfs_media:get_media(Media),
                            {ok, ActualMedia}
                        catch
                            _:Error -> {error, Error}
                        end
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, Media}} -> Media;
        {atomic, {error, Reason}} -> {error, Reason};
        Error -> Error
    end.

get_post_ipfs_by_ipns(IPNS) when is_binary(IPNS); is_list(IPNS) ->
    try
        case ipfs_client_5:name_resolve([{arg, IPNS}]) of
            {ok, #{path := Path}} ->
                case binary:split(Path, <<"/ipfs/">>) of
                    [_, CID] -> 
                        binary_to_list(CID);
                    _ ->
                        {error, invalid_path_format}
                end;
            {error, Reason} ->
                {error, {ipfs_resolution_failed, Reason}};
            UnexpectedResponse ->
                {error, {unexpected_response, UnexpectedResponse}}
        end
    catch
        error ->
            error;
        exit:Exit ->
            {error, {exit, Exit}};
        throw:Throw ->
            {error, {throw, Throw}}
    end;
get_post_ipfs_by_ipns(IPNS) ->
    {error, {invalid_ipns_format, IPNS}}.

pin_post(PostID) ->
    Fun = fun() ->
        case mnesia:read({post, PostID}) of
            [] -> {error, post_not_found};
            [Post] ->
                IPNS = Post#post.content,
                case get_post_ipfs_by_ipns(IPNS) of
                    {ok, IPFS} ->
                        case ipfs_client_5:pin_add([{arg, IPFS}]) of
                            {ok, _} -> ok;
                            Error -> Error
                        end;
                    Error -> Error
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.
    

%% get_posts_by_author(Username)
get_posts_by_author(Author) ->
  Fun = fun() ->
            mnesia:match_object(#post{author = Author,
                                      _ = '_'})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_posts_by_user_id(UserID) ->
  Fun = fun() ->
            mnesia:match_object(#post{user_id = UserID,
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

get_posts_content_by_user_id(UserID) ->
    Fun = fun() ->
              Posts = mnesia:match_object(#post{user_id = UserID, _ = '_'}),
              [Post#post.content || Post <- Posts]
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_last_50_posts_content_by_user_id(UserID) ->
    Fun = fun() ->
              Posts = mnesia:match_object(#post{user_id = UserID, _ = '_'}),
              
              SortedPosts = lists:sort(fun(A, B) -> 
                                          A#post.date_created > B#post.date_created 
                                       end, Posts),
              
              Last60Posts = lists:sublist(SortedPosts, 50),
              [Post#post.content || Post <- Last60Posts]
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
                [{_Key, _Value} | _] -> 
                  lists:foldl(fun({K, V}, Acc) ->
                    case lists:keymember(K, 1, Acc) of
                      true ->
                        lists:keyreplace(K, 1, Acc, {K, V});
                      false ->
                        [{K, V} | Acc]
                    end
                  end, Content, NewContent);
                _ ->  
                  NewContent
              end,
            mnesia:write(Post#post{content = UpdatedContent})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

%% delete_post(PostID)
delete_post(Id) ->
  F = fun() ->
      case mnesia:read({post, Id}) of
          [] -> 
              {error, post_not_found};  
          _ ->
              mnesia:delete({post, Id}),
              ok  
      end
  end,

  case mnesia:activity(transaction, F) of
      ok -> 
          ok;  
      {error, post_not_found} -> 
          {error, post_not_found};  
      {aborted, Reason} -> 
          {error, transaction_failed, Reason}  
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
      case mnesia:read({post, PostId}) of
          [] -> {error, post_not_found};
          [Post] ->
              ID = nanoid:gen(),
              mnesia:write(#like{
                  id = ID,
                  post = PostId,
                  userID = UserID,
                  date_created = calendar:universal_time()
              }),
              Likes = Post#post.likes,
              mnesia:write(Post#post{likes = [ID | Likes]}),
              ID
      end
  end,
  case mnesia:transaction(Fun) of
      {atomic, Result} -> Result;
      {aborted, Reason} -> {error, {transaction_failed, Reason}}
  end.

unlike_post(LikeID, PostId) ->
  Fun = fun() ->
      case mnesia:read({post, PostId}) of
          [] -> {error, post_not_found};
          [Post] ->
              Unlike = lists:delete(LikeID, Post#post.likes),
              mnesia:write(Post#post{
                  likes = Unlike,
                  date_created = calendar:universal_time()
              }),
              ok
      end
  end,
  case mnesia:transaction(Fun) of
      {atomic, Result} -> Result;
      {aborted, Reason} -> {error, {transaction_failed, Reason}}
  end.

get_all_likes_for_user(UserID) ->
  Fun = fun() ->
            Likes = mnesia:match_object(#like{userID = UserID, _ = '_'}),
            Likes
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_last_50_likes_for_user(UserID) ->
  Fun = fun() ->
            Likes = mnesia:match_object(#like{userID = UserID, _ = '_'}),
            SortedLikes = lists:sort(
              fun(A, B) ->
                A#like.date_created > B#like.date_created
              end,
              Likes
            ),
            lists:sublist(SortedLikes, 50)
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

%% Content = [{text, Text}, {media, Media}, {mention, Name}, {like, Like}]
add_comment(Author, PostID, Content) ->  
    Fun = fun() ->
        Id = nanoid:gen(),
        Date = calendar:universal_time(),
        UserID = userdb:get_user_id(Author),
        
        ContentToCache = if
            is_binary(Content) -> binary_to_list(Content);
            true -> Content
        end,
        
        ok = content_cache:set(Id, ContentToCache),
        PlaceholderContent = Id,
        
        case mnesia:read({post, PostID}) of
            [] -> 
                {error, post_not_found}; 
            [Post] ->
                Comment = #comment{
                    id = Id,
                    user_id = UserID,
                    post = PostID,
                    author = Author,
                    content = PlaceholderContent,
                    date_created = Date
                },
                mnesia:write(Comment),
                
                CurrentComments = case Post#post.comments of
                    nil -> [];
                    List when is_list(List) -> List;
                    _ -> []
                end,
                
                UpdatedComments = [Id | CurrentComments],
                UpdatedPost = Post#post{
                    comments = UpdatedComments
                },
                mnesia:write(UpdatedPost),
                
                update_activity(Author, Date),
                
                {ok, Id}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, Id}} -> 
            spawn(fun() ->
                ContentToUse = content_cache:get(Id),
                CIDString = case ContentToUse of
                    "" -> ""; 
                    _ -> ipfs_content:upload_text(ContentToUse)
                end,
                
                UpdateF = fun() ->
                    case mnesia:read({comment, Id}) of
                        [CommentToUpdate] ->
                            UpdatedComment = CommentToUpdate#comment{
                                content = CIDString
                            },
                            mnesia:write(UpdatedComment);
                        [] -> ok
                    end
                end,
                mnesia:transaction(UpdateF),
                
                content_cache:delete(Id),
                
                spawn(fun() ->
                    timer:sleep(5000),  
                    
                    case CIDString of
                        "" -> 
                            error_logger:info_msg("Empty content, skipping IPNS publish for comment ~p", [Id]);
                        _ ->
                            try
                                {ok, #{id := _KeyID, name := _BinID}} = ipfs_client_4:key_gen("comment_" ++ Id),
                                
                                PublishOptions = [
                                    {key, "comment_" ++ Id},
                                    {resolve, true},
                                    {lifetime, "12h0m0s"},  
                                    {ttl, "1m0s"},
                                    {v1compat, true},
                                    {ipns_base, "base36"}
                                ],
                                
                                case ipfs_client_5:name_publish(
                                    "/ipfs/" ++ CIDString,
                                    PublishOptions
                                ) of
                                    {ok, #{name := IPNSKey}} ->
                                        update_comment_ipns(Id, IPNSKey);
                                    {error, _Reason} ->
                                        error_logger:error_msg("IPNS publish failed for comment ~p: ~p", [Id, _Reason]),
                                        err 
                                end
                            catch
                                Exception:Error:Stacktrace ->
                                    error_logger:error_msg(
                                        "Exception while publishing to IPNS for comment ~p: ~p:~p~n~p", 
                                        [Id, Exception, Error, Stacktrace]
                                    )
                            end
                    end
                end)
            end),
            
            Id;
        {atomic, {error, Reason}} -> 
            {error, Reason};
        {aborted, Reason} -> 
            {error, {transaction_failed, Reason}}
    end.

% Helper function to update IPNS information for comments
update_comment_ipns(CommentId, IPNSKey) ->
    UpdateF = fun() ->
        case mnesia:read({comment, CommentId}) of
            [Comment] ->
                UpdatedComment = Comment#comment{ipns = IPNSKey},
                mnesia:write(UpdatedComment),
                ok;
            [] ->
                {error, not_found}
        end
    end,
    
    case mnesia:transaction(UpdateF) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> 
            error_logger:error_msg("Failed to update comment ~p with IPNS: ~p", [CommentId, Reason]),
            {error, Reason};
        {aborted, Reason} ->
            error_logger:error_msg("Transaction aborted while updating comment ~p with IPNS: ~p", [CommentId, Reason]),
            {error, {transaction_failed, Reason}}
    end.

get_comment_content(CommentID) ->
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
        Result = read_comment_and_fetch_content(CommentID),
        Parent ! {Ref, Result}
    end),
    receive
        {Ref, {ok, Content}} -> Content;
        {Ref, OtherResult} -> OtherResult
    after 5000 ->
        {error, timeout}
    end.

% Function that handles reading the comment and fetching content
read_comment_and_fetch_content(CommentID) ->
    ReadFun = fun() ->
        case mnesia:read({comment, CommentID}) of
            [] -> 
                {error, comment_not_found};
            [Comment] ->
                {ok, Comment}
        end
    end,
    
    case mnesia:transaction(ReadFun) of
        {atomic, {error, Reason}} -> 
            {error, Reason};
        {atomic, {ok, Comment}} ->
            Content = Comment#comment.content,
            fetch_content(Content, CommentID);
        Error -> 
            Error
    end.

% Function to handle the various ways of fetching content
fetch_content(Content, CommentID) when Content =:= CommentID ->
    case content_cache:get(CommentID) of
        undefined -> {error, content_not_ready};
        CachedContent -> {ok, CachedContent}
    end;
fetch_content(Content, _CommentID) ->
    IpfsRef = make_ref(),
    Parent = self(),
    IpfsWorker = spawn(fun() ->
        try
            Result = ipfs_content:get_text_content(Content),
            Parent ! {IpfsRef, {ok, Result}}
        catch
            _:Error -> 
                Parent ! {IpfsRef, {error, Error}}
        end
    end),
    
    MonitorRef = monitor(process, IpfsWorker),
    
    receive
        {IpfsRef, Result} ->
            demonitor(MonitorRef, [flush]),
            Result;
        {'DOWN', MonitorRef, process, IpfsWorker, normal} ->
            {error, ipfs_missing_result};
        {'DOWN', MonitorRef, process, IpfsWorker, Reason} ->
            {error, {ipfs_worker_crashed, Reason}}
    after 10000 ->
        exit(IpfsWorker, kill),
        demonitor(MonitorRef, [flush]),
        {error, ipfs_timeout}
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

update_comment_likes(CommentID, NewLikes) ->
  Fun = fun() ->
            [Comment] = mnesia:read({comment, CommentID}),
            mnesia:write(Comment#comment{likes = NewLikes}),
            CommentID
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_comment_likes(CommentID) -> 
  Fun = fun() ->
            case mnesia:read({comment, CommentID}) of
                [] -> 
                    []; 
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

get_comment_replies(CommentID) -> 
  Fun = fun() ->
            case mnesia:read({comment, CommentID}) of
                [] -> 
                    []; 
                [Comment] ->
                    lists:foldl(fun(ID, Acc) ->
                                    [Reply] = mnesia:read({reply, ID}),
                                    [Reply|Acc]
                                end,
                                [], Comment#comment.replies)
            end
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

reply_comment(UserID, CommentID, Content) ->
    Fun = fun() ->
        Id = nanoid:gen(),
        Date = calendar:universal_time(),
        
        ContentToCache = if
            is_binary(Content) -> binary_to_list(Content);
            true -> Content
        end,
        
        ok = content_cache:set(Id, ContentToCache),
        PlaceholderContent = Id,
        
        case mnesia:read({comment, CommentID}) of
            [] -> 
                {error, comment_not_found}; 
            [Comment] ->
                Reply = #reply{
                    id = Id,
                    comment = CommentID,
                    userID = UserID,
                    content = PlaceholderContent,
                    date_created = Date
                },
                mnesia:write(Reply),
                
                CurrentReplies = case Comment#comment.replies of
                    nil -> [];
                    List when is_list(List) -> List;
                    _ -> []
                end,
                
                UpdatedReplies = [Id | CurrentReplies],
                UpdatedComment = Comment#comment{
                    replies = UpdatedReplies
                },
                mnesia:write(UpdatedComment),
                
                {ok, Id}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, Id}} -> 
            spawn(fun() ->
                ContentToUse = content_cache:get(Id),
                CIDString = case ContentToUse of
                    "" -> ""; 
                    _ -> ipfs_content:upload_text(ContentToUse)
                end,
                
                UpdateF = fun() ->
                    case mnesia:read({reply, Id}) of
                        [ReplyToUpdate] ->
                            UpdatedReply = ReplyToUpdate#reply{
                                content = CIDString,
                                data = #{
                                    cid => CIDString
                                }
                            },
                            mnesia:write(UpdatedReply);
                        [] -> ok
                    end
                end,
                mnesia:transaction(UpdateF),
                
                content_cache:delete(Id)
            end),
            
            Id;
        {atomic, {error, Reason}} -> 
            {error, Reason};
        {aborted, Reason} -> 
            {error, {transaction_failed, Reason}}
    end.

get_reply_content(ReplyID) ->
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
        Result = read_reply_and_fetch_content(ReplyID),
        Parent ! {Ref, Result}
    end),
    receive
        {Ref, {ok, Content}} -> Content;
        {Ref, OtherResult} -> OtherResult
    after 5000 ->
        {error, timeout}
    end.

% Function that handles reading the reply and fetching content
read_reply_and_fetch_content(ReplyID) ->
    ReadFun = fun() ->
        case mnesia:read({reply, ReplyID}) of
            [] -> 
                {error, reply_not_found};
            [Reply] ->
                {ok, Reply}
        end
    end,
    
    case mnesia:transaction(ReadFun) of
        {atomic, {error, Reason}} -> 
            {error, Reason};
        {atomic, {ok, Reply}} ->
            Content = Reply#reply.content,
            fetch_reply_content(Content, ReplyID);
        Error -> 
            Error
    end.

% Function to handle the various ways of fetching content
fetch_reply_content("uploading...", _ReplyID) ->
    {ok, "Content is still being uploaded"};
fetch_reply_content("", _ReplyID) ->
    {ok, ""};
fetch_reply_content(Content, ReplyID) when Content =:= ReplyID ->
    case content_cache:get(ReplyID) of
        undefined -> {error, content_not_ready};
        CachedContent -> {ok, CachedContent}
    end;
fetch_reply_content(Content, _ReplyID) ->
    IpfsRef = make_ref(),
    Parent = self(),
    IpfsWorker = spawn(fun() ->
        try
            Result = ipfs_content:get_text_content(Content),
            Parent ! {IpfsRef, {ok, Result}}
        catch
            _:Error -> 
                Parent ! {IpfsRef, {error, Error}}
        end
    end),
    
    MonitorRef = monitor(process, IpfsWorker),
    
    receive
        {IpfsRef, Result} ->
            demonitor(MonitorRef, [flush]),
            Result;
        {'DOWN', MonitorRef, process, IpfsWorker, normal} ->
            {error, ipfs_missing_result};
        {'DOWN', MonitorRef, process, IpfsWorker, Reason} ->
            {error, {ipfs_worker_crashed, Reason}}
    after 10000 ->
        exit(IpfsWorker, kill),
        demonitor(MonitorRef, [flush]),
        {error, ipfs_timeout}
    end.


delete_reply_from_mnesia(ReplyID) ->
  Fun = fun() -> 
    mnesia:delete({reply, ReplyID})
  end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.
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

delete_reply(ReplyID) ->
  F = fun() ->
      case mnesia:read({reply, ReplyID}) of
          [] -> 
              {error, post_not_found};  
          _ ->
              mnesia:delete({reply, ReplyID}),
              ok 
      end
  end,

  case mnesia:activity(transaction, F) of
      ok -> 
          ok;  
      {error, post_not_found} -> 
          {error, post_not_found}; 
      {aborted, Reason} -> 
          {error, transaction_failed, Reason}  
  end.

get_single_comment(CommentId) ->
    case mnesia:dirty_read({comment, CommentId}) of
        [Comment] -> {ok, Comment};
        [] -> {error, not_found}
    end.

get_user_by_single_comment(CommentID) ->
  Fun = fun() ->
            case mnesia:read({comment, CommentID}) of
              [Comment] ->
                UserID = Comment#comment.user_id,
                case mnesia:read({user, UserID}) of
                  [User] -> User; 
                  _     -> undefined 
                end;
              _ ->
                []
            end
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

get_all_comments_for_user(UserID) ->
  Fun = fun() ->
            Comments = mnesia:match_object(#comment{user_id = UserID, _ = '_'}),
            Comments
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_last_50_comments_for_user(UserID) ->
  Fun = fun() ->
            Comments = mnesia:match_object(#comment{user_id = UserID, _ = '_'}),
            SortedComments = lists:sort(
              fun(A, B) ->
                A#comment.date_created > B#comment.date_created
              end,
              Comments
            ),
            lists:sublist(SortedComments, 50)
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_last_50_comments_content_for_user(UserID) ->
  Fun = fun() ->
            Comments = mnesia:match_object(#comment{user_id = UserID, _ = '_'}),
            SortedComments = lists:sort(
              fun(A, B) ->
                A#comment.date_created > B#comment.date_created
              end,
              Comments
            ),
            Last50Comments = lists:sublist(SortedComments, 50),
            lists:map(fun(Comment) -> Comment#comment.content end, Last50Comments)
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_all_comments_by_user_id(PostId, UserID) ->
  Fun = fun() ->
            case mnesia:read({post, PostId}) of
              [Post] ->
                Comments = lists:filter(
                  fun(Id) ->
                    case mnesia:read({comment, Id}) of
                      [Comment] -> Comment#comment.user_id =:= UserID;
                      _ -> false
                    end
                  end,
                  Post#post.comments
                ),
                lists:map(fun(Id) -> 
                  [Comment] = mnesia:read({comment, Id}),
                  Comment
                end, Comments);
              _ ->
                []
            end
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
        case mnesia:read(post, PostId) of
            [] -> 
                {error, post_not_found}; 
            [Post] ->
                case lists:member(CommentID, Post#post.comments) of
                    false -> 
                        {error, comment_not_found}; 
                    true -> 
                        UpdatedComments = lists:delete(CommentID, Post#post.comments),
                        UpdatedPost = Post#post{
                            comments = UpdatedComments,
                            date_created = calendar:universal_time()
                        },
                        mnesia:write(UpdatedPost),
                        
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


