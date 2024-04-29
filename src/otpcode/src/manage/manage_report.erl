-module(manage_report).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("admins.hrl").
-export([get_report_by_id/2, get_reports/0, get_user_by_report_id/2,
get_reporter_by_report_id/1, get_post_by_report_id/1]).

get_report_by_id(ID, AdminUsername) ->
    case userdb:get_user_id(AdminUsername) of
        {error, _} = Error ->
            Error;
        {ok, _} ->
            case lists:member(AdminUsername, ?ADMIN_USERNAMES) of
                true ->
                    Res = mnesia:transaction(
                              fun() ->
                                  mnesia:match_object(#report{id = ID, _ = '_'})
                              end),
                    case Res of
                        {atomic, []} -> {error, user_not_exist};
                        {atomic, [User]} -> User;
                        _ -> {error, unknown_error}
                    end;
                false ->
                    {error, not_admin}
            end
    end.

get_reports() ->
    Fun = fun() ->
            mnesia:all_keys(report)
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_user_by_report_id(ID, AdminUsername) ->
    Report = get_report_by_id(ID, AdminUsername),
    UserID = Report#report.user,
    User = userdb:get_user_by_id(UserID),
    User.

get_reporter_by_report_id(ID) ->
    Report = manage_report:get_report_by_id(ID),
    ReporterID = Report#report.reporter,
    Reporter = userdb:get_user_by_id(ReporterID),
    Reporter.

get_post_by_report_id(ID) ->
    Report = manage_report:get_report_by_id(ID),
    PostID = Report#report.post,
    Post = postdb:get_post_by_id(PostID),
    Post.