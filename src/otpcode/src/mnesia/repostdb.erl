-module(repostdb).
-author("Zaryn Technologies").
-include("../records.hrl").

-export([
    simple_repost/2,
    repost_with_comment/3,
    unrepost/2,
    get_repost/1,
    get_original_post/1,
    get_user_reposts/1,
    get_post_reposters/1,
    is_reposted_by_user/2,
    get_repost_count/1,
    get_repost_type/1,
    get_target_post_for_interaction/1,
    get_repost_comment_content/1
]).

simple_repost(UserId, OriginalPostId) ->
    Fun = fun() ->
        case validate_repost(UserId, OriginalPostId) of
            {error, Reason} -> {error, Reason};
            ok ->
                case mnesia:read({post, OriginalPostId}) of
                    [] -> {error, original_post_not_found};
                    [OriginalPost] ->
                        NewPostId = nanoid:gen(),
                        Date = calendar:universal_time(),

                        [User] = mnesia:read({user, UserId}),
                        Username = User#user.username,

                        RepostRecord = #post{
                            id = NewPostId,
                            user_id = UserId,
                            author = Username,
                            content = "",
                            media = [],
                            hashtag = [],
                            is_repost = true,
                            original_post_id = OriginalPostId,
                            repost_type = simple,
                            repost_comment = undefined,
                            comments = [],
                            likes = [],
                            date_created = Date
                        },
                        mnesia:write(RepostRecord),

                        RepostInfo = {UserId, NewPostId, simple, Date},
                        UpdatedRepostedBy = [RepostInfo | OriginalPost#post.reposted_by],
                        UpdatedOriginalPost = OriginalPost#post{
                            repost_count = OriginalPost#post.repost_count + 1,
                            reposted_by = UpdatedRepostedBy
                        },
                        mnesia:write(UpdatedOriginalPost),

                        UserPosts = User#user.post,
                        UserReposts = User#user.reposts,
                        mnesia:write(User#user{
                            post = [NewPostId | UserPosts],
                            reposts = [NewPostId | UserReposts]
                        }),

                        userdb:update_last_activity(UserId, Date),

                        {ok, NewPostId}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, PostId}} -> {ok, PostId};
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

repost_with_comment(UserId, OriginalPostId, Comment) ->
    Fun = fun() ->
        case validate_repost(UserId, OriginalPostId) of
            {error, Reason} -> {error, Reason};
            ok ->
                case mnesia:read({post, OriginalPostId}) of
                    [] -> {error, original_post_not_found};
                    [OriginalPost] ->
                        NewPostId = nanoid:gen(),
                        Date = calendar:universal_time(),

                        [User] = mnesia:read({user, UserId}),
                        Username = User#user.username,

                        CommentToCache = if
                            is_binary(Comment) -> binary_to_list(Comment);
                            true -> Comment
                        end,
                        ok = content_cache:set({repost_comment, NewPostId}, CommentToCache),

                        PlaceholderComment = {repost_comment, NewPostId},

                        RepostRecord = #post{
                            id = NewPostId,
                            user_id = UserId,
                            author = Username,
                            content = OriginalPost#post.content,
                            media = OriginalPost#post.media,
                            hashtag = OriginalPost#post.hashtag,
                            is_repost = true,
                            original_post_id = OriginalPostId,
                            repost_type = with_comment,
                            repost_comment = PlaceholderComment,
                            comments = [],
                            likes = [],
                            date_created = Date
                        },
                        mnesia:write(RepostRecord),

                        RepostInfo = {UserId, NewPostId, with_comment, Date},
                        UpdatedRepostedBy = [RepostInfo | OriginalPost#post.reposted_by],
                        UpdatedOriginalPost = OriginalPost#post{
                            repost_count = OriginalPost#post.repost_count + 1,
                            reposted_by = UpdatedRepostedBy
                        },
                        mnesia:write(UpdatedOriginalPost),

                        UserPosts = User#user.post,
                        UserReposts = User#user.reposts,
                        mnesia:write(User#user{
                            post = [NewPostId | UserPosts],
                            reposts = [NewPostId | UserReposts]
                        }),

                        userdb:update_last_activity(UserId, Date),

                        {ok, NewPostId}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, PostId}} ->
            spawn(fun() ->
                CommentContent = content_cache:get({repost_comment, PostId}),
                CommentCID = case CommentContent of
                    "" -> "";
                    _ -> ipfs_content:upload_text(CommentContent)
                end,

                UpdateF = fun() ->
                    case mnesia:read({post, PostId}) of
                        [PostToUpdate] ->
                            UpdatedPost = PostToUpdate#post{
                                repost_comment = CommentCID
                            },
                            mnesia:write(UpdatedPost);
                        [] -> ok
                    end
                end,
                mnesia:transaction(UpdateF),
                content_cache:delete({repost_comment, PostId})
            end),

            {ok, PostId};
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

unrepost(UserId, RepostId) ->
    Fun = fun() ->
        case mnesia:read({post, RepostId}) of
            [] -> {error, repost_not_found};
            [Repost] ->
                case Repost#post.user_id =:= UserId of
                    false -> {error, unauthorized};
                    true ->
                        case Repost#post.is_repost of
                            false -> {error, not_a_repost};
                            true ->
                                OriginalPostId = Repost#post.original_post_id,

                                case mnesia:read({post, OriginalPostId}) of
                                    [OriginalPost] ->
                                        UpdatedRepostedBy = lists:filter(
                                            fun({Uid, Rid, _Type, _Date}) ->
                                                not (Uid =:= UserId andalso Rid =:= RepostId)
                                            end,
                                            OriginalPost#post.reposted_by
                                        ),
                                        UpdatedOriginalPost = OriginalPost#post{
                                            repost_count = max(0, OriginalPost#post.repost_count - 1),
                                            reposted_by = UpdatedRepostedBy
                                        },
                                        mnesia:write(UpdatedOriginalPost);
                                    [] -> ok
                                end,

                                [User] = mnesia:read({user, UserId}),
                                UpdatedPosts = lists:delete(RepostId, User#user.post),
                                UpdatedReposts = lists:delete(RepostId, User#user.reposts),
                                mnesia:write(User#user{
                                    post = UpdatedPosts,
                                    reposts = UpdatedReposts
                                }),

                                mnesia:delete({post, RepostId}),
                                ok
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_repost(RepostId) ->
    Fun = fun() ->
        case mnesia:read({post, RepostId}) of
            [] -> {error, not_found};
            [Post] ->
                case Post#post.is_repost of
                    true -> {ok, Post};
                    false -> {error, not_a_repost}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_original_post(RepostId) ->
    Fun = fun() ->
        case mnesia:read({post, RepostId}) of
            [] -> {error, repost_not_found};
            [Repost] ->
                case Repost#post.is_repost of
                    false -> {error, not_a_repost};
                    true ->
                        OriginalPostId = Repost#post.original_post_id,
                        case mnesia:read({post, OriginalPostId}) of
                            [] -> {error, original_post_not_found};
                            [OriginalPost] -> {ok, OriginalPost}
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_user_reposts(UserId) ->
    Fun = fun() ->
        case mnesia:read({user, UserId}) of
            [] -> [];
            [User] ->
                RepostIds = User#user.reposts,
                lists:foldl(fun(RepostId, Acc) ->
                    case mnesia:read({post, RepostId}) of
                        [Post] -> [Post | Acc];
                        [] -> Acc
                    end
                end, [], RepostIds)
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Reposts} -> Reposts;
        {aborted, _Reason} -> []
    end.

get_post_reposters(PostId) ->
    Fun = fun() ->
        case mnesia:read({post, PostId}) of
            [] -> [];
            [Post] -> Post#post.reposted_by
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Reposters} -> Reposters;
        {aborted, _Reason} -> []
    end.

is_reposted_by_user(UserId, PostId) ->
    Fun = fun() ->
        case mnesia:read({post, PostId}) of
            [] -> false;
            [Post] ->
                lists:any(fun({Uid, _RepostId, _Type, _Date}) ->
                    Uid =:= UserId
                end, Post#post.reposted_by)
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> false
    end.

get_repost_count(PostId) ->
    Fun = fun() ->
        case mnesia:read({post, PostId}) of
            [] -> 0;
            [Post] -> Post#post.repost_count
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Count} -> Count;
        {aborted, _Reason} -> 0
    end.

get_repost_type(RepostId) ->
    Fun = fun() ->
        case mnesia:read({post, RepostId}) of
            [] -> {error, not_found};
            [Post] ->
                case Post#post.is_repost of
                    false -> {error, not_a_repost};
                    true -> {ok, Post#post.repost_type}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_target_post_for_interaction(PostId) ->
    Fun = fun() ->
        case mnesia:read({post, PostId}) of
            [] -> {error, post_not_found};
            [Post] ->
                case Post#post.is_repost of
                    false ->
                        {ok, PostId};
                    true ->
                        case Post#post.repost_type of
                            simple ->
                                {ok, Post#post.original_post_id};
                            with_comment ->
                                {ok, PostId};
                            _ ->
                                {ok, PostId}
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_repost_comment_content(RepostID) ->
    Fun = fun() ->
        case mnesia:read({post, RepostID}) of
            [] -> {error, post_not_found};
            [Post] ->
                case Post#post.is_repost of
                    false -> {error, not_a_repost};
                    true ->
                        case Post#post.repost_type of
                            simple -> {ok, undefined};
                            with_comment ->
                                Comment = Post#post.repost_comment,
                                case Comment of
                                    undefined -> {ok, undefined};
                                    {repost_comment, ID} when ID =:= RepostID ->
                                        case content_cache:get({repost_comment, ID}) of
                                            undefined -> {error, comment_not_ready};
                                            CachedComment -> {ok, CachedComment}
                                        end;
                                    "" -> {ok, ""};
                                    _ ->
                                        try
                                            ActualComment = ipfs_content:get_text_content(Comment),
                                            {ok, ActualComment}
                                        catch
                                            _:Error -> {error, Error}
                                        end
                                end;
                            _ -> {ok, undefined}
                        end
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, Content}} -> Content;
        {atomic, {error, Reason}} -> {error, Reason};
        Error -> Error
    end.

validate_repost(UserId, OriginalPostId) ->
    case mnesia:read({user, UserId}) of
        [] -> {error, user_not_found};
        [_User] ->
            case mnesia:read({post, OriginalPostId}) of
                [] -> {error, original_post_not_found};
                [OriginalPost] ->
                    case OriginalPost#post.user_id =:= UserId of
                        true -> {error, cannot_repost_own_post};
                        false ->
                            AlreadyReposted = lists:any(
                                fun({Uid, _RepostId, _Type, _Date}) ->
                                    Uid =:= UserId
                                end,
                                OriginalPost#post.reposted_by
                            ),
                            case AlreadyReposted of
                                true -> {error, already_reposted};
                                false -> ok
                            end
                    end
            end
    end.
