-module(manage_notif).
-author("Zaryn Technologies").
-export([get_user_by_id/1, get_user_by_username/1, welcome/2, mention/3]).

get_user_by_id(UserID) ->
    userdb:get_user_by_id(UserID).

get_user_by_username(Username) ->
    userdb:get_user(Username).

welcome(UserID, Username) ->
    case userdb:get_user_by_id(UserID) of
        User ->
            Message = io:fwrite("Welcome to Mazaryn Dear ~p~n", [Username]),
            notifdb:insert(UserID, Message);
        _ ->
            'user_not_exist'
    end.

mention(Author, UserID, PostID) ->
    Post = postdb:get_post_by_id(PostID),
    case userdb:get_user_by_id(UserID) of
        User ->
            Message =
            io:fwrite("User ~p mentioned you on post ~p~n", [Author, PostID]),
            Notif = notifdb:insert(UserID, Message),
            Notif;
        _ ->
            'user_not_exist'
    end.