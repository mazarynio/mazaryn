-module(user_post).
-author("Zaryn Technologies").
-export([get_last_50_posts_category/1, get_category_frequency/1, get_last_50_comment_category/1, get_comment_category_frequency/1,
get_last_50_liked_posts_content/1, get_last_50_liked_posts_content_with_category/1, get_like_category_frequency/1]).

-include("../../records.hrl").
get_last_50_posts_category(UserID) ->
    Last50PostsContent = postdb:get_last_50_posts_content_by_user_id(UserID),
    
    Categories = lists:map(fun(Content) ->
                             post_text:category(Content)
                           end, Last50PostsContent),
    
    Categories.

get_category_frequency(UserID) ->
    NestedCategories = get_last_50_posts_category(UserID),
    
    FreqDict = process_categories(NestedCategories, dict:new()),
    
    CategoryCounts = dict:to_list(FreqDict),
    lists:sort(fun({_, CountA}, {_, CountB}) -> CountA > CountB end, CategoryCounts).

% Get the categories of the last 50 comments made by a user
get_last_50_comment_category(UserID) ->
    Last50CommentsContent = postdb:get_last_50_comments_content_for_user(UserID),
    
    Categories = lists:map(fun(Content) ->
                             post_text:category(Content)
                           end, Last50CommentsContent),
    
    Categories.

% Get the frequency of categories for the last 50 comments
get_comment_category_frequency(UserID) ->
    NestedCategories = get_last_50_comment_category(UserID),
    
    FreqDict = process_categories(NestedCategories, dict:new()),
    
    CategoryCounts = dict:to_list(FreqDict),
    lists:sort(fun({_, CountA}, {_, CountB}) -> CountA > CountB end, CategoryCounts).


get_last_50_liked_posts_content(UserID) ->
  Fun = fun() ->
            Likes = mnesia:match_object(#like{userID = UserID, _ = '_'}),
            SortedLikes = lists:sort(
              fun(A, B) ->
                A#like.date_created > B#like.date_created
              end,
              Likes
            ),
            Last50Likes = lists:sublist(SortedLikes, 50),
            PostIDs = lists:map(fun(Like) -> Like#like.post end, Last50Likes),
            Posts = lists:map(
              fun(PostID) ->
                case mnesia:read({post, PostID}) of
                  [Post] -> Post;
                  _ -> undefined 
                end
              end,
              PostIDs
            ),
            lists:filtermap(
              fun(Post) ->
                case Post of
                  undefined -> false; 
                  _ -> {true, Post#post.content} 
                end
              end,
              Posts
            )
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_last_50_liked_posts_content_with_category(UserID) ->
  Fun = fun() ->
            Likes = mnesia:match_object(#like{userID = UserID, _ = '_'}),
            SortedLikes = lists:sort(
              fun(A, B) ->
                A#like.date_created > B#like.date_created
              end,
              Likes
            ),
            Last50Likes = lists:sublist(SortedLikes, 50),
            PostIDs = lists:map(fun(Like) -> Like#like.post end, Last50Likes),
            Posts = lists:map(
              fun(PostID) ->
                case mnesia:read({post, PostID}) of
                  [Post] -> Post;
                  _ -> undefined 
                end
              end,
              PostIDs
            ),
            lists:filtermap(
              fun(Post) ->
                case Post of
                  undefined -> false; 
                  _ -> 
                    Content = Post#post.content,
                    Category = post_text:category(Content),
                    {true, {Content, Category}} 
                end
              end,
              Posts
            )
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

% Get the frequency of categories for the last 50 liked posts
get_like_category_frequency(UserID) ->
  Last50LikedPostsContent = get_last_50_liked_posts_content(UserID),
  
  Categories = lists:map(fun(Content) ->
                           post_text:category(Content)
                         end, Last50LikedPostsContent),
  
  FreqDict = process_categories(Categories, dict:new()),
  
  CategoryCounts = dict:to_list(FreqDict),
  lists:sort(fun({_, CountA}, {_, CountB}) -> CountA > CountB end, CategoryCounts).

process_categories([], Acc) ->
    Acc;
process_categories([CategoryList|Rest], Acc) ->
    NewAcc = lists:foldl(
        fun(Category, Dict) ->
            dict:update_counter(Category, 1, Dict)
        end,
        Acc,
        CategoryList
    ),
    process_categories(Rest, NewAcc).