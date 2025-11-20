-module(teamdb).
-author("Zaryn Technologies").

-export([
    create_team/4,
    create_team_with_members/4,
    delete_team/2,
    disband_team/2,
    get_team_by_id/1,
    get_teams_by_competition/1,
    get_team_by_name/2,

    add_member/3,
    add_member_with_role/4,
    remove_member/3,
    update_member_role/4,
    get_team_members/1,
    get_member_count/1,
    is_member/2,
    get_member_role/2,

    invite_user/3,
    invite_user_with_expiry/4,
    accept_invitation/2,
    reject_invitation/2,
    cancel_invitation/3,
    get_pending_invitations/1,
    get_user_invitations/1,
    is_invitation_valid/2,

    transfer_captaincy/3,
    promote_to_captain/2,
    demote_from_captain/2,

    update_team_name/3,
    update_team_metadata/2,
    set_team_avatar/2,
    get_team_avatar/1,

    get_team_submissions/1,
    get_team_best_submission/1,
    get_team_score/1,
    update_team_score/2,
    get_team_rank/2,

    link_notebook/2,
    unlink_notebook/2,
    get_team_notebooks/1,

    add_discussion/3,
    get_team_discussions/1,

    request_merge/3,
    approve_merge/3,
    reject_merge/3,
    execute_merge/3,
    get_merge_requests/1,

    get_team_statistics/1,
    get_team_activity/1,
    get_team_performance_history/1,

    check_team_size_limit/2,
    can_add_member/2,
    validate_team_name/2,

    search_teams/2,
    get_top_teams/2,
    get_active_teams/1,

    get_user_team_in_competition/2,
    get_user_teams/1,
    has_team_in_competition/2,

    compute_quota_usage/1,
    update_compute_quota/2,
    check_compute_quota/2,

    export_team_data/1,
    import_team_data/2,

    get_team_leaderboard_position/1,
    compare_teams/2,

    clone_team/3,
    archive_team/1,
    restore_team/1,
    is_archived/1
]).

-include("../ml_records.hrl").
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(DEFAULT_INVITATION_EXPIRY_DAYS, 7).
-define(MAX_TEAM_SIZE, 10).
-define(MAX_RETRIES, 10).
-define(BACKOFF_TIME, 20).

create_team(CompetitionId, CreatorId, TeamName, InitialMembers) ->
    Fun = fun() ->
        case mnesia:read({competition, CompetitionId}) of
            [] ->
                {error, competition_not_found};
            [Competition] ->
                case Competition#competition.status of
                    active ->
                        case lists:member(CreatorId, Competition#competition.participants) of
                            false ->
                                {error, not_a_participant};
                            true ->
                                case get_user_team_in_competition_internal(CompetitionId, CreatorId) of
                                    {ok, _} ->
                                        {error, already_in_team};
                                    {error, _} ->
                                        case validate_team_name_internal(CompetitionId, TeamName) of
                                            {error, Reason} ->
                                                {error, Reason};
                                            ok ->
                                                TeamId = nanoid:gen(),
                                                Now = calendar:universal_time(),

                                                AllMembers = case lists:member(CreatorId, InitialMembers) of
                                                    true -> InitialMembers;
                                                    false -> [CreatorId | InitialMembers]
                                                end,

                                                TeamSizeLimit = Competition#competition.team_size_limit,
                                                case length(AllMembers) > TeamSizeLimit of
                                                    true ->
                                                        {error, team_size_exceeds_limit};
                                                    false ->
                                                        MemberRecords = [{CreatorId, captain, Now}] ++
                                                            [{MemberId, member, Now} || MemberId <- InitialMembers, MemberId =/= CreatorId],

                                                        Team = #team{
                                                            id = TeamId,
                                                            competition_id = CompetitionId,
                                                            name = TeamName,
                                                            creator_id = CreatorId,
                                                            members = MemberRecords,
                                                            invitations = [],
                                                            merge_requests = [],
                                                            submission_ids = [],
                                                            team_score = undefined,
                                                            rank = undefined,
                                                            disbanded = false,
                                                            date_created = Now,
                                                            date_updated = Now,
                                                            discussion_cids = [],
                                                            notebook_ids = [],
                                                            compute_quota_used = #{},
                                                            total_submissions = 0,
                                                            best_submission_id = undefined,
                                                            team_avatar_cid = undefined,
                                                            metadata = #{}
                                                        },

                                                        mnesia:write(Team),

                                                        UpdatedTeamIds = [TeamId | Competition#competition.team_ids],
                                                        mnesia:write(Competition#competition{
                                                            team_ids = UpdatedTeamIds,
                                                            date_updated = Now
                                                        }),

                                                        {ok, TeamId}
                                                end
                                        end
                                end
                        end;
                    _ ->
                        {error, competition_not_active}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

create_team_with_members(CompetitionId, CreatorId, TeamName, MemberList) ->
    create_team(CompetitionId, CreatorId, TeamName, MemberList).

delete_team(TeamId, UserId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] ->
                {error, team_not_found};
            [Team] ->
                case Team#team.creator_id of
                    UserId ->
                        CompetitionId = Team#team.competition_id,

                        mnesia:delete({team, TeamId}),

                        case mnesia:read({competition, CompetitionId}) of
                            [Competition] ->
                                UpdatedTeamIds = lists:delete(TeamId, Competition#competition.team_ids),
                                mnesia:write(Competition#competition{
                                    team_ids = UpdatedTeamIds,
                                    date_updated = calendar:universal_time()
                                });
                            [] -> ok
                        end,

                        ok;
                    _ ->
                        {error, unauthorized}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

disband_team(TeamId, CaptainId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] ->
                {error, team_not_found};
            [Team] ->
                IsCaptain = lists:any(fun({MemberId, Role, _}) ->
                    MemberId =:= CaptainId andalso Role =:= captain
                end, Team#team.members),

                case IsCaptain of
                    false ->
                        {error, not_captain};
                    true ->
                        Now = calendar:universal_time(),
                        mnesia:write(Team#team{
                            disbanded = true,
                            date_updated = Now
                        }),

                        case mnesia:read({competition, Team#team.competition_id}) of
                            [Competition] ->
                                UpdatedTeamIds = lists:delete(TeamId, Competition#competition.team_ids),
                                mnesia:write(Competition#competition{
                                    team_ids = UpdatedTeamIds,
                                    date_updated = Now
                                });
                            [] -> ok
                        end,

                        ok
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_by_id(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] -> Team
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_teams_by_competition(CompetitionId) ->
    Fun = fun() ->
        mnesia:match_object(#team{competition_id = CompetitionId, disbanded = false, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_team_by_name(CompetitionId, TeamName) ->
    Fun = fun() ->
        Teams = mnesia:match_object(#team{
            competition_id = CompetitionId,
            name = TeamName,
            disbanded = false,
            _ = '_'
        }),
        case Teams of
            [] -> {error, team_not_found};
            [Team] -> Team;
            [Team | _] -> Team
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_member(TeamId, UserId, InviterId) ->
    add_member_with_role(TeamId, UserId, InviterId, member).

add_member_with_role(TeamId, UserId, InviterId, Role) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] ->
                {error, team_not_found};
            [Team] ->
                IsMember = lists:any(fun({MemberId, _, _}) -> MemberId =:= InviterId end, Team#team.members),
                case IsMember of
                    false ->
                        {error, not_team_member};
                    true ->
                        AlreadyMember = lists:any(fun({MemberId, _, _}) -> MemberId =:= UserId end, Team#team.members),
                        case AlreadyMember of
                            true ->
                                {error, already_team_member};
                            false ->
                                case mnesia:read({competition, Team#team.competition_id}) of
                                    [] ->
                                        {error, competition_not_found};
                                    [Competition] ->
                                        CurrentMemberCount = length(Team#team.members),
                                        case CurrentMemberCount >= Competition#competition.team_size_limit of
                                            true ->
                                                {error, team_full};
                                            false ->
                                                Now = calendar:universal_time(),
                                                NewMember = {UserId, Role, Now},
                                                UpdatedMembers = [NewMember | Team#team.members],
                                                mnesia:write(Team#team{
                                                    members = UpdatedMembers,
                                                    date_updated = Now
                                                }),
                                                ok
                                        end
                                end
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

remove_member(TeamId, MemberToRemove, RemoverId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] ->
                {error, team_not_found};
            [Team] ->
                IsCaptain = lists:any(fun({MemberId, Role, _}) ->
                    MemberId =:= RemoverId andalso Role =:= captain
                end, Team#team.members),

                IsSelfRemoval = RemoverId =:= MemberToRemove,

                case IsCaptain orelse IsSelfRemoval of
                    false ->
                        {error, unauthorized};
                    true ->
                        case MemberToRemove =:= Team#team.creator_id andalso not IsSelfRemoval of
                            true ->
                                {error, cannot_remove_creator};
                            false ->
                                UpdatedMembers = lists:filter(fun({MemberId, _, _}) ->
                                    MemberId =/= MemberToRemove
                                end, Team#team.members),

                                case length(UpdatedMembers) of
                                    0 ->
                                        mnesia:write(Team#team{
                                            disbanded = true,
                                            date_updated = calendar:universal_time()
                                        });
                                    _ ->
                                        mnesia:write(Team#team{
                                            members = UpdatedMembers,
                                            date_updated = calendar:universal_time()
                                        })
                                end,
                                ok
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_member_role(TeamId, MemberId, NewRole, UpdaterId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] ->
                {error, team_not_found};
            [Team] ->
                IsCaptain = lists:any(fun({Mid, Role, _}) ->
                    Mid =:= UpdaterId andalso Role =:= captain
                end, Team#team.members),

                case IsCaptain of
                    false ->
                        {error, not_captain};
                    true ->
                        UpdatedMembers = lists:map(fun({Mid, Role, JoinedAt} = Member) ->
                            if Mid =:= MemberId ->
                                {Mid, NewRole, JoinedAt};
                               true ->
                                Member
                            end
                        end, Team#team.members),

                        mnesia:write(Team#team{
                            members = UpdatedMembers,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_members(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] -> {ok, Team#team.members}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_member_count(TeamId) ->
    case get_team_members(TeamId) of
        {ok, Members} -> {ok, length(Members)};
        Error -> Error
    end.

is_member(TeamId, UserId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> false;
            [Team] ->
                lists:any(fun({MemberId, _, _}) -> MemberId =:= UserId end, Team#team.members)
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> false
    end.

get_member_role(TeamId, UserId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                case lists:keyfind(UserId, 1, Team#team.members) of
                    false -> {error, not_a_member};
                    {_, Role, _} -> {ok, Role}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

invite_user(TeamId, InviteeId, InviterId) ->
    Now = calendar:universal_time(),
    ExpiresAt = add_days_to_datetime(Now, ?DEFAULT_INVITATION_EXPIRY_DAYS),
    invite_user_with_expiry(TeamId, InviteeId, InviterId, ExpiresAt).

invite_user_with_expiry(TeamId, InviteeId, InviterId, ExpiresAt) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] ->
                {error, team_not_found};
            [Team] ->
                IsMember = lists:any(fun({MemberId, _, _}) -> MemberId =:= InviterId end, Team#team.members),
                case IsMember of
                    false ->
                        {error, not_team_member};
                    true ->
                        AlreadyMember = lists:any(fun({MemberId, _, _}) -> MemberId =:= InviteeId end, Team#team.members),
                        case AlreadyMember of
                            true ->
                                {error, already_team_member};
                            false ->
                                AlreadyInvited = lists:any(fun({Mid, _, _, _}) -> Mid =:= InviteeId end, Team#team.invitations),
                                case AlreadyInvited of
                                    true ->
                                        {error, already_invited};
                                    false ->
                                        Now = calendar:universal_time(),
                                        NewInvitation = {InviteeId, pending, Now, ExpiresAt},
                                        UpdatedInvitations = [NewInvitation | Team#team.invitations],
                                        mnesia:write(Team#team{
                                            invitations = UpdatedInvitations,
                                            date_updated = Now
                                        }),
                                        ok
                                end
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

accept_invitation(TeamId, UserId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] ->
                {error, team_not_found};
            [Team] ->
                case lists:keyfind(UserId, 1, Team#team.invitations) of
                    false ->
                        {error, invitation_not_found};
                    {UserId, pending, _InvitedAt, ExpiresAt} ->
                        Now = calendar:universal_time(),
                        case Now > ExpiresAt of
                            true ->
                                {error, invitation_expired};
                            false ->
                                case mnesia:read({competition, Team#team.competition_id}) of
                                    [] ->
                                        {error, competition_not_found};
                                    [Competition] ->
                                        CurrentMemberCount = length(Team#team.members),
                                        case CurrentMemberCount >= Competition#competition.team_size_limit of
                                            true ->
                                                {error, team_full};
                                            false ->
                                                NewMember = {UserId, member, Now},
                                                UpdatedMembers = [NewMember | Team#team.members],
                                                UpdatedInvitations = lists:keydelete(UserId, 1, Team#team.invitations),
                                                mnesia:write(Team#team{
                                                    members = UpdatedMembers,
                                                    invitations = UpdatedInvitations,
                                                    date_updated = Now
                                                }),
                                                ok
                                        end
                                end
                        end;
                    _ ->
                        {error, invitation_not_pending}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

reject_invitation(TeamId, UserId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] ->
                {error, team_not_found};
            [Team] ->
                case lists:keyfind(UserId, 1, Team#team.invitations) of
                    false ->
                        {error, invitation_not_found};
                    _ ->
                        UpdatedInvitations = lists:keydelete(UserId, 1, Team#team.invitations),
                        mnesia:write(Team#team{
                            invitations = UpdatedInvitations,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

cancel_invitation(TeamId, InviteeId, CancelerId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] ->
                {error, team_not_found};
            [Team] ->
                IsCaptain = lists:any(fun({MemberId, Role, _}) ->
                    MemberId =:= CancelerId andalso Role =:= captain
                end, Team#team.members),

                case IsCaptain of
                    false ->
                        {error, not_captain};
                    true ->
                        UpdatedInvitations = lists:keydelete(InviteeId, 1, Team#team.invitations),
                        mnesia:write(Team#team{
                            invitations = UpdatedInvitations,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_pending_invitations(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                Now = calendar:universal_time(),
                ValidInvitations = lists:filter(fun({_, Status, _, ExpiresAt}) ->
                    Status =:= pending andalso Now =< ExpiresAt
                end, Team#team.invitations),
                {ok, ValidInvitations}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_user_invitations(UserId) ->
    Fun = fun() ->
        AllTeams = mnesia:match_object(#team{disbanded = false, _ = '_'}),

        UserInvitations = lists:foldl(fun(Team, Acc) ->
            case lists:keyfind(UserId, 1, Team#team.invitations) of
                false -> Acc;
                {UserId, Status, InvitedAt, ExpiresAt} ->
                    [{Team#team.id, Team#team.name, Team#team.competition_id, Status, InvitedAt, ExpiresAt} | Acc]
            end
        end, [], AllTeams),

        {ok, UserInvitations}
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

is_invitation_valid(TeamId, UserId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> false;
            [Team] ->
                case lists:keyfind(UserId, 1, Team#team.invitations) of
                    false -> false;
                    {UserId, pending, _, ExpiresAt} ->
                        Now = calendar:universal_time(),
                        Now =< ExpiresAt;
                    _ -> false
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> false
    end.

transfer_captaincy(TeamId, NewCaptainId, CurrentCaptainId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] ->
                {error, team_not_found};
            [Team] ->
                IsCaptain = lists:any(fun({MemberId, Role, _}) ->
                    MemberId =:= CurrentCaptainId andalso Role =:= captain
                end, Team#team.members),

                case IsCaptain of
                    false ->
                        {error, not_captain};
                    true ->
                        IsNewCaptainMember = lists:any(fun({MemberId, _, _}) ->
                            MemberId =:= NewCaptainId
                        end, Team#team.members),

                        case IsNewCaptainMember of
                            false ->
                                {error, new_captain_not_member};
                            true ->
                                UpdatedMembers = lists:map(fun({MemberId, Role, JoinedAt}) ->
                                    if
                                        MemberId =:= CurrentCaptainId ->
                                            {MemberId, member, JoinedAt};
                                        MemberId =:= NewCaptainId ->
                                            {MemberId, captain, JoinedAt};
                                        true ->
                                            {MemberId, Role, JoinedAt}
                                    end
                                end, Team#team.members),

                                mnesia:write(Team#team{
                                    members = UpdatedMembers,
                                    date_updated = calendar:universal_time()
                                }),
                                ok
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

promote_to_captain(TeamId, MemberId) ->
    update_member_role_internal(TeamId, MemberId, captain).

demote_from_captain(TeamId, MemberId) ->
    update_member_role_internal(TeamId, MemberId, member).

update_member_role_internal(TeamId, MemberId, NewRole) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] ->
                {error, team_not_found};
            [Team] ->
                UpdatedMembers = lists:map(fun({Mid, Role, JoinedAt} = Member) ->
                    if Mid =:= MemberId ->
                        {Mid, NewRole, JoinedAt};
                       true ->
                        Member
                    end
                end, Team#team.members),

                mnesia:write(Team#team{
                    members = UpdatedMembers,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_team_name(TeamId, NewName, UpdaterId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] ->
                {error, team_not_found};
            [Team] ->
                IsCaptain = lists:any(fun({MemberId, Role, _}) ->
                    MemberId =:= UpdaterId andalso Role =:= captain
                end, Team#team.members),

                case IsCaptain of
                    false ->
                        {error, not_captain};
                    true ->
                        case validate_team_name_internal(Team#team.competition_id, NewName) of
                            {error, Reason} ->
                                {error, Reason};
                            ok ->
                                mnesia:write(Team#team{
                                    name = NewName,
                                    date_updated = calendar:universal_time()
                                }),
                                ok
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_team_metadata(TeamId, NewMetadata) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                mnesia:write(Team#team{
                    metadata = NewMetadata,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

set_team_avatar(TeamId, AvatarCID) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                mnesia:write(Team#team{
                    team_avatar_cid = AvatarCID,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_avatar(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] -> {ok, Team#team.team_avatar_cid}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_submissions(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                Submissions = lists:foldl(fun(SubId, Acc) ->
                    case mnesia:read({submission, SubId}) of
                        [Sub] -> [Sub | Acc];
                        [] -> Acc
                    end
                end, [], Team#team.submission_ids),
                {ok, lists:reverse(Submissions)}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_best_submission(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                case Team#team.best_submission_id of
                    undefined -> {error, no_submissions};
                    BestId ->
                        case mnesia:read({submission, BestId}) of
                            [Sub] -> {ok, Sub};
                            [] -> {error, submission_not_found}
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_score(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] -> {ok, Team#team.team_score}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_team_score(TeamId, NewScore) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                mnesia:write(Team#team{
                    team_score = NewScore,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_rank(CompetitionId, TeamId) ->
    case leaderboarddb:get_team_rank(CompetitionId, TeamId) of
        {ok, Rank, Score} -> {ok, Rank, Score};
        Error -> Error
    end.

link_notebook(TeamId, NotebookId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                UpdatedNotebooks = case lists:member(NotebookId, Team#team.notebook_ids) of
                    true -> Team#team.notebook_ids;
                    false -> [NotebookId | Team#team.notebook_ids]
                end,
                mnesia:write(Team#team{
                    notebook_ids = UpdatedNotebooks,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

unlink_notebook(TeamId, NotebookId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                UpdatedNotebooks = lists:delete(NotebookId, Team#team.notebook_ids),
                mnesia:write(Team#team{
                    notebook_ids = UpdatedNotebooks,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_notebooks(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] -> {ok, Team#team.notebook_ids}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_discussion(TeamId, CreatorId, Content) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                IsMember = lists:any(fun({MemberId, _, _}) -> MemberId =:= CreatorId end, Team#team.members),
                case IsMember of
                    false -> {error, not_team_member};
                    true ->
                        DiscussionId = nanoid:gen(),
                        DiscussionCID = ipfs_content:upload_text(Content),

                        UpdatedDiscussions = [DiscussionCID | Team#team.discussion_cids],
                        mnesia:write(Team#team{
                            discussion_cids = UpdatedDiscussions,
                            date_updated = calendar:universal_time()
                        }),
                        {ok, DiscussionId}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_discussions(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] -> {ok, Team#team.discussion_cids}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

request_merge(FromTeamId, ToTeamId, RequesterId) ->
    Fun = fun() ->
        case {mnesia:read({team, FromTeamId}), mnesia:read({team, ToTeamId})} of
            {[], _} -> {error, from_team_not_found};
            {_, []} -> {error, to_team_not_found};
            {[FromTeam], [ToTeam]} ->
                IsCaptain = lists:any(fun({MemberId, Role, _}) ->
                    MemberId =:= RequesterId andalso Role =:= captain
                end, FromTeam#team.members),

                case IsCaptain of
                    false -> {error, not_captain};
                    true ->
                        case FromTeam#team.competition_id =:= ToTeam#team.competition_id of
                            false -> {error, different_competitions};
                            true ->
                                Now = calendar:universal_time(),
                                MergeRequest = {FromTeamId, pending, Now},
                                UpdatedRequests = [MergeRequest | ToTeam#team.merge_requests],
                                mnesia:write(ToTeam#team{
                                    merge_requests = UpdatedRequests,
                                    date_updated = Now
                                }),
                                ok
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

approve_merge(ToTeamId, FromTeamId, ApproverId) ->
    Fun = fun() ->
        case mnesia:read({team, ToTeamId}) of
            [] -> {error, team_not_found};
            [ToTeam] ->
                IsCaptain = lists:any(fun({MemberId, Role, _}) ->
                    MemberId =:= ApproverId andalso Role =:= captain
                end, ToTeam#team.members),

                case IsCaptain of
                    false -> {error, not_captain};
                    true ->
                        case lists:keyfind(FromTeamId, 1, ToTeam#team.merge_requests) of
                            false -> {error, merge_request_not_found};
                            {FromTeamId, pending, RequestedAt} ->
                                UpdatedRequest = {FromTeamId, approved, RequestedAt},
                                UpdatedRequests = lists:keyreplace(FromTeamId, 1,
                                    ToTeam#team.merge_requests, UpdatedRequest),
                                mnesia:write(ToTeam#team{
                                    merge_requests = UpdatedRequests,
                                    date_updated = calendar:universal_time()
                                }),
                                ok;
                            _ -> {error, request_already_processed}
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

reject_merge(ToTeamId, FromTeamId, RejecterId) ->
    Fun = fun() ->
        case mnesia:read({team, ToTeamId}) of
            [] -> {error, team_not_found};
            [ToTeam] ->
                IsCaptain = lists:any(fun({MemberId, Role, _}) ->
                    MemberId =:= RejecterId andalso Role =:= captain
                end, ToTeam#team.members),

                case IsCaptain of
                    false -> {error, not_captain};
                    true ->
                        UpdatedRequests = lists:keydelete(FromTeamId, 1, ToTeam#team.merge_requests),
                        mnesia:write(ToTeam#team{
                            merge_requests = UpdatedRequests,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

execute_merge(ToTeamId, FromTeamId, ExecutorId) ->
    Fun = fun() ->
        case {mnesia:read({team, ToTeamId}), mnesia:read({team, FromTeamId})} of
            {[], _} -> {error, to_team_not_found};
            {_, []} -> {error, from_team_not_found};
            {[ToTeam], [FromTeam]} ->
                IsCaptain = lists:any(fun({MemberId, Role, _}) ->
                    MemberId =:= ExecutorId andalso Role =:= captain
                end, ToTeam#team.members),

                case IsCaptain of
                    false -> {error, not_captain};
                    true ->
                        case mnesia:read({competition, ToTeam#team.competition_id}) of
                            [] -> {error, competition_not_found};
                            [Competition] ->
                                TotalMembers = length(ToTeam#team.members) + length(FromTeam#team.members),
                                case TotalMembers > Competition#competition.team_size_limit of
                                    true -> {error, merged_team_exceeds_limit};
                                    false ->
                                        Now = calendar:universal_time(),
                                        MergedMembers = ToTeam#team.members ++ FromTeam#team.members,
                                        MergedSubmissions = ToTeam#team.submission_ids ++ FromTeam#team.submission_ids,
                                        MergedNotebooks = lists:usort(ToTeam#team.notebook_ids ++ FromTeam#team.notebook_ids),

                                        mnesia:write(ToTeam#team{
                                            members = MergedMembers,
                                            submission_ids = MergedSubmissions,
                                            notebook_ids = MergedNotebooks,
                                            date_updated = Now
                                        }),

                                        mnesia:write(FromTeam#team{
                                            disbanded = true,
                                            date_updated = Now
                                        }),

                                        UpdatedTeamIds = lists:delete(FromTeamId, Competition#competition.team_ids),
                                        mnesia:write(Competition#competition{
                                            team_ids = UpdatedTeamIds,
                                            date_updated = Now
                                        }),

                                        ok
                                end
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_merge_requests(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] -> {ok, Team#team.merge_requests}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_statistics(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                Stats = #{
                    member_count => length(Team#team.members),
                    total_submissions => Team#team.total_submissions,
                    team_score => Team#team.team_score,
                    best_submission_id => Team#team.best_submission_id,
                    notebook_count => length(Team#team.notebook_ids),
                    discussion_count => length(Team#team.discussion_cids),
                    pending_invitations => length(lists:filter(fun({_, Status, _, _}) ->
                        Status =:= pending
                    end, Team#team.invitations)),
                    date_created => Team#team.date_created,
                    date_updated => Team#team.date_updated
                },
                {ok, Stats}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_activity(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                Submissions = mnesia:match_object(#submission{
                    team_id = TeamId,
                    _ = '_'
                }),

                RecentSubmissions = lists:sort(fun(A, B) ->
                    A#submission.submission_time > B#submission.submission_time
                end, Submissions),

                Activity = #{
                    recent_submissions => lists:sublist(RecentSubmissions, 10),
                    last_updated => Team#team.date_updated,
                    active_members => length(Team#team.members)
                },

                {ok, Activity}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_performance_history(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                Submissions = lists:foldl(fun(SubId, Acc) ->
                    case mnesia:read({submission, SubId}) of
                        [Sub] when Sub#submission.score_public =/= undefined ->
                            [{Sub#submission.submission_time, Sub#submission.score_public} | Acc];
                        _ -> Acc
                    end
                end, [], Team#team.submission_ids),

                SortedHistory = lists:sort(fun({TimeA, _}, {TimeB, _}) ->
                    TimeA < TimeB
                end, Submissions),

                {ok, SortedHistory}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

check_team_size_limit(TeamId, CompetitionId) ->
    Fun = fun() ->
        case {mnesia:read({team, TeamId}), mnesia:read({competition, CompetitionId})} of
            {[], _} -> {error, team_not_found};
            {_, []} -> {error, competition_not_found};
            {[Team], [Competition]} ->
                CurrentSize = length(Team#team.members),
                Limit = Competition#competition.team_size_limit,
                {ok, CurrentSize, Limit, CurrentSize < Limit}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

can_add_member(TeamId, CompetitionId) ->
    case check_team_size_limit(TeamId, CompetitionId) of
        {ok, _Current, _Limit, CanAdd} -> CanAdd;
        _ -> false
    end.

validate_team_name(CompetitionId, TeamName) ->
    validate_team_name_internal(CompetitionId, TeamName).

validate_team_name_internal(CompetitionId, TeamName) ->
    ExistingTeams = mnesia:match_object(#team{
        competition_id = CompetitionId,
        name = TeamName,
        disbanded = false,
        _ = '_'
    }),
    case ExistingTeams of
        [] -> ok;
        _ -> {error, team_name_already_exists}
    end.

search_teams(CompetitionId, Query) ->
    Fun = fun() ->
        AllTeams = mnesia:match_object(#team{
            competition_id = CompetitionId,
            disbanded = false,
            _ = '_'
        }),

        QueryLower = string:to_lower(Query),

        lists:filter(fun(Team) ->
            NameMatch = string:find(string:to_lower(Team#team.name), QueryLower) =/= nomatch,
            NameMatch
        end, AllTeams)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_top_teams(CompetitionId, Limit) ->
    case leaderboarddb:get_top_n_teams(CompetitionId, Limit) of
        {ok, TopTeams} -> {ok, TopTeams};
        Error -> Error
    end.

get_active_teams(CompetitionId) ->
    Fun = fun() ->
        AllTeams = mnesia:match_object(#team{
            competition_id = CompetitionId,
            disbanded = false,
            _ = '_'
        }),

        Now = calendar:universal_time(),
        ThresholdDate = subtract_days_from_datetime(Now, 7),

        lists:filter(fun(Team) ->
            Team#team.date_updated > ThresholdDate
        end, AllTeams)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_user_team_in_competition(CompetitionId, UserId) ->
    get_user_team_in_competition_internal(CompetitionId, UserId).

get_user_team_in_competition_internal(CompetitionId, UserId) ->
    AllTeams = mnesia:match_object(#team{
        competition_id = CompetitionId,
        disbanded = false,
        _ = '_'
    }),

    find_user_team(UserId, AllTeams).

find_user_team(_UserId, []) ->
    {error, not_in_team};
find_user_team(UserId, [Team | Rest]) ->
    IsMember = lists:any(fun({MemberId, _, _}) -> MemberId =:= UserId end, Team#team.members),
    case IsMember of
        true -> {ok, Team};
        false -> find_user_team(UserId, Rest)
    end.

get_user_teams(UserId) ->
    Fun = fun() ->
        AllTeams = mnesia:match_object(#team{disbanded = false, _ = '_'}),

        lists:filter(fun(Team) ->
            lists:any(fun({MemberId, _, _}) -> MemberId =:= UserId end, Team#team.members)
        end, AllTeams)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

has_team_in_competition(CompetitionId, UserId) ->
    case get_user_team_in_competition(CompetitionId, UserId) of
        {ok, _Team} -> true;
        {error, _} -> false
    end.

compute_quota_usage(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] -> {ok, Team#team.compute_quota_used}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_compute_quota(TeamId, QuotaUpdate) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                CurrentQuota = Team#team.compute_quota_used,
                UpdatedQuota = maps:merge(CurrentQuota, QuotaUpdate),
                mnesia:write(Team#team{
                    compute_quota_used = UpdatedQuota,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

check_compute_quota(TeamId, ResourceType) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                QuotaUsed = Team#team.compute_quota_used,
                Used = maps:get(ResourceType, QuotaUsed, 0),

                case mnesia:read({competition, Team#team.competition_id}) of
                    [] -> {error, competition_not_found};
                    [Competition] ->
                        Quota = Competition#competition.compute_quota,
                        Limit = maps:get(ResourceType, Quota, 0),
                        Available = max(0, Limit - Used),
                        {ok, #{used => Used, limit => Limit, available => Available}}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

export_team_data(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                Data = #{
                    id => Team#team.id,
                    competition_id => Team#team.competition_id,
                    name => Team#team.name,
                    creator_id => Team#team.creator_id,
                    members => Team#team.members,
                    submission_ids => Team#team.submission_ids,
                    team_score => Team#team.team_score,
                    rank => Team#team.rank,
                    total_submissions => Team#team.total_submissions,
                    best_submission_id => Team#team.best_submission_id,
                    notebook_ids => Team#team.notebook_ids,
                    date_created => Team#team.date_created,
                    date_updated => Team#team.date_updated,
                    metadata => Team#team.metadata
                },
                {ok, Data}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

import_team_data(TeamId, Data) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                UpdatedTeam = Team#team{
                    team_score = maps:get(team_score, Data, Team#team.team_score),
                    rank = maps:get(rank, Data, Team#team.rank),
                    metadata = maps:get(metadata, Data, Team#team.metadata)
                },
                mnesia:write(UpdatedTeam),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_leaderboard_position(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                case leaderboarddb:get_team_rank(Team#team.competition_id, TeamId) of
                    {ok, Rank, Score} ->
                        {ok, #{rank => Rank, score => Score, team_score => Team#team.team_score}};
                    Error -> Error
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

compare_teams(TeamId1, TeamId2) ->
    Fun = fun() ->
        case {mnesia:read({team, TeamId1}), mnesia:read({team, TeamId2})} of
            {[], _} -> {error, team1_not_found};
            {_, []} -> {error, team2_not_found};
            {[Team1], [Team2]} ->
                Score1 = Team1#team.team_score,
                Score2 = Team2#team.team_score,

                case {Score1, Score2} of
                    {undefined, _} -> {error, team1_not_scored};
                    {_, undefined} -> {error, team2_not_scored};
                    _ ->
                        Comparison = #{
                            team1 => #{
                                id => TeamId1,
                                name => Team1#team.name,
                                score => Score1,
                                member_count => length(Team1#team.members),
                                submission_count => Team1#team.total_submissions
                            },
                            team2 => #{
                                id => TeamId2,
                                name => Team2#team.name,
                                score => Score2,
                                member_count => length(Team2#team.members),
                                submission_count => Team2#team.total_submissions
                            },
                            score_difference => Score1 - Score2,
                            better_team => if Score1 > Score2 -> team1;
                                             Score2 > Score1 -> team2;
                                             true -> tie
                                          end
                        },
                        {ok, Comparison}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

clone_team(SourceTeamId, NewCompetitionId, CreatorId) ->
    Fun = fun() ->
        case mnesia:read({team, SourceTeamId}) of
            [] -> {error, source_team_not_found};
            [SourceTeam] ->
                NewTeamId = nanoid:gen(),
                Now = calendar:universal_time(),

                ClonedTeam = SourceTeam#team{
                    id = NewTeamId,
                    competition_id = NewCompetitionId,
                    creator_id = CreatorId,
                    members = [{CreatorId, captain, Now}],
                    invitations = [],
                    merge_requests = [],
                    submission_ids = [],
                    team_score = undefined,
                    rank = undefined,
                    disbanded = false,
                    date_created = Now,
                    date_updated = Now,
                    total_submissions = 0,
                    best_submission_id = undefined
                },

                mnesia:write(ClonedTeam),
                {ok, NewTeamId}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

archive_team(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                Metadata = Team#team.metadata,
                UpdatedMetadata = maps:put(archived, true, Metadata),
                mnesia:write(Team#team{
                    metadata = UpdatedMetadata,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

restore_team(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> {error, team_not_found};
            [Team] ->
                Metadata = Team#team.metadata,
                UpdatedMetadata = maps:put(archived, false, Metadata),
                mnesia:write(Team#team{
                    metadata = UpdatedMetadata,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

is_archived(TeamId) ->
    Fun = fun() ->
        case mnesia:read({team, TeamId}) of
            [] -> false;
            [Team] ->
                Metadata = Team#team.metadata,
                maps:get(archived, Metadata, false)
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> false
    end.

add_days_to_datetime(DateTime, Days) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    NewSeconds = Seconds + (Days * 86400),
    calendar:gregorian_seconds_to_datetime(NewSeconds).

subtract_days_from_datetime(DateTime, Days) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    NewSeconds = Seconds - (Days * 86400),
    calendar:gregorian_seconds_to_datetime(NewSeconds).
