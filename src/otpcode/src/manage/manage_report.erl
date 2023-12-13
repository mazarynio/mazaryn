-module(manage_report).
-author("Zaryn Technologies").
-include("../records.hrl").
-export([get_report_by_id/1, get_reports/0, get_user_by_report_id/1,
get_reporter_by_report_id/1, get_post_by_report_id/1]).

get_report_by_id(ID) ->
    Res = mnesia:transaction(
          fun() ->
              mnesia:match_object(#report{id = ID, _= '_'})
          end),
    case Res of
      {atomic, []} -> user_not_exist;
      {atomic, [User]} -> User;
      _ -> error
  end.

get_reports() ->
    Fun = fun() ->
            mnesia:all_keys(report)
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_user_by_report_id(ID) ->
    Report = manage_report:get_report_by_id(ID),
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