-module(user_post).
-author("Zaryn Technologies").
-export([get_last_50_posts_category/1, get_category_frequency/1, get_last_50_comment_category/1, get_comment_category_frequency/1]).

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