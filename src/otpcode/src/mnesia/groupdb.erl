-module(groupdb).
-author("Zaryn Technologies").
-export([
    create_group/7, get_group/1, update_group/2, delete_group/1,
    join_group/2, leave_group/2, remove_member/3,
    add_admin/4, remove_admin/2, update_admin_permissions/3,
    send_invite/4, accept_invite/2, reject_invite/2, cancel_invite/2,
    send_message/4, edit_message/3, delete_message/2, pin_message/2, unpin_message/2,
    react_to_message/3, get_group_messages/2, get_member_groups/1,
    ban_user/3, unban_user/2, mute_member/2, unmute_member/2,
    search_groups/2, get_group_members/1, update_member_settings/3,
    create_channel/7, get_channel/1, update_channel/2, delete_channel/1,
    subscribe_channel/2, unsubscribe_channel/2,
    create_channel_post/4, edit_channel_post/3, delete_channel_post/2,
    pin_channel_post/2, unpin_channel_post/2,
    react_to_channel_post/3, comment_on_channel_post/4,
    get_channel_posts/2, search_channels/2, get_user_channels/1,
    add_channel_admin/3, remove_channel_admin/2, send_channel_invite/4
]).
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

is_valid_category(Category) when is_list(Category) ->
    Length = length(Category),
    Length > 0 andalso Length < 100 andalso
    not lists:all(fun(C) -> (C >= $a andalso C =< $z) orelse
                            (C >= $A andalso C =< $Z) orelse
                            (C >= $0 andalso C =< $9) orelse
                            C =:= $_ orelse C =:= $- end, Category) orelse
    lists:any(fun(C) -> C =:= $\s orelse C =:= $, end, Category);
is_valid_category(Category) when is_binary(Category) ->
    is_valid_category(binary_to_list(Category));
is_valid_category(_) ->
    false.

normalize_category(undefined) -> undefined;
normalize_category([]) -> undefined;
normalize_category(Category) when is_list(Category) ->
    case length(Category) > 30 andalso
         lists:all(fun(C) -> (C >= $a andalso C =< $z) orelse
                            (C >= $A andalso C =< $Z) orelse
                            (C >= $0 andalso C =< $9) orelse
                            C =:= $_ orelse C =:= $- end, Category) of
        true -> undefined;
        false -> Category
    end;
normalize_category(Category) when is_binary(Category) ->
    normalize_category(binary_to_list(Category));
normalize_category(_) ->
    undefined.

create_group(OwnerID, UniqueName, Name, Description, Type, Privacy, Settings) ->
    Fun = fun() ->
        case mnesia:read({user, OwnerID}) of
            [Owner] ->
                case mnesia:match_object(#group{unique_name = UniqueName, _ = '_'}) of
                    [] -> ok;
                    [_|_] -> throw({error, unique_name_already_taken})
                end,
                GroupID = nanoid:gen(),
                Date = calendar:universal_time(),
                Group = #group{
                    id = GroupID,
                    unique_name = UniqueName,
                    name = Name,
                    description = Description,
                    type = Type,
                    privacy = Privacy,
                    owner_id = OwnerID,
                    admins = [OwnerID],
                    members = [OwnerID],
                    settings = maps:merge(#{
                        allow_member_invite => false,
                        message_history_visible => true,
                        approval_required => false,
                        mute_all => false
                    }, Settings),
                    member_count = 1,
                    date_created = Date,
                    date_updated = Date,
                    last_activity = Date
                },
                Member = #group_member{
                    id = nanoid:gen(),
                    group_id = GroupID,
                    user_id = OwnerID,
                    role = owner,
                    permissions = [all],
                    join_date = Date
                },
                mnesia:write(Group),
                mnesia:write(Member),
                UpdatedGroups = case Owner#user.groups of
                    undefined -> [GroupID];
                    Groups when is_list(Groups) -> [GroupID | Groups];
                    _ -> [GroupID]
                end,
                mnesia:write(Owner#user{groups = UpdatedGroups}),
                GroupID;
            [] ->
                throw({error, user_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, GroupID} -> GroupID;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

get_group(GroupID) ->
    Fun = fun() ->
        case mnesia:read({group, GroupID}) of
            [Group] -> Group;
            [] -> throw(group_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Group} -> Group;
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

update_group(GroupID, UpdateMap) ->
    Fun = fun() ->
        case mnesia:read({group, GroupID}) of
            [Group] ->
                NewUniqueName = maps:get(unique_name, UpdateMap, Group#group.unique_name),
                case NewUniqueName =/= Group#group.unique_name of
                    true ->
                        case mnesia:match_object(#group{unique_name = NewUniqueName, _ = '_'}) of
                            [] -> ok;
                            [_|_] -> throw({error, unique_name_already_taken})
                        end;
                    false -> ok
                end,
                UpdatedGroup = Group#group{
                    unique_name = NewUniqueName,
                    name = maps:get(name, UpdateMap, Group#group.name),
                    description = maps:get(description, UpdateMap, Group#group.description),
                    privacy = maps:get(privacy, UpdateMap, Group#group.privacy),
                    avatar_url = maps:get(avatar_url, UpdateMap, Group#group.avatar_url),
                    banner_url = maps:get(banner_url, UpdateMap, Group#group.banner_url),
                    category = maps:get(category, UpdateMap, Group#group.category),
                    tags = maps:get(tags, UpdateMap, Group#group.tags),
                    settings = maps:merge(Group#group.settings, maps:get(settings, UpdateMap, #{})),
                    date_updated = calendar:universal_time()
                },
                mnesia:write(UpdatedGroup),
                GroupID;
            [] ->
                throw(group_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, GroupID} -> GroupID;
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, {error, Error}} -> throw(Error);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

delete_group(GroupID) ->
    Fun = fun() ->
        case mnesia:read({group, GroupID}) of
            [Group] ->
                Members = Group#group.members,
                lists:foreach(fun(MemberID) ->
                    case mnesia:read({user, MemberID}) of
                        [User] ->
                            UpdatedGroups = lists:delete(GroupID, User#user.groups),
                            mnesia:write(User#user{groups = UpdatedGroups});
                        [] -> ok
                    end
                end, Members),
                MemberRecords = mnesia:match_object(#group_member{group_id = GroupID, _ = '_'}),
                lists:foreach(fun(M) -> mnesia:delete_object(M) end, MemberRecords),
                Messages = mnesia:match_object(#group_message{group_id = GroupID, _ = '_'}),
                lists:foreach(fun(Msg) -> mnesia:delete_object(Msg) end, Messages),
                Invites = mnesia:match_object(#group_invite{group_id = GroupID, _ = '_'}),
                lists:foreach(fun(Inv) -> mnesia:delete_object(Inv) end, Invites),
                mnesia:delete({group, GroupID}),
                ok;
            [] ->
                throw(group_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

join_group(UserID, GroupID) ->
    Fun = fun() ->
        case mnesia:read({group, GroupID}) of
            [Group] ->
                case mnesia:read({user, UserID}) of
                    [User] ->
                        case lists:member(UserID, Group#group.members) of
                            true ->
                                throw({error, already_member});
                            false ->
                                case lists:member(UserID, Group#group.banned_users) of
                                    true ->
                                        throw({error, user_banned});
                                    false ->
                                        Settings = Group#group.settings,
                                        ApprovalRequired = maps:get(approval_required, Settings, false),
                                        case {Group#group.privacy, ApprovalRequired} of
                                            {private, false} ->
                                                throw({error, invite_required});
                                            _ ->
                                                Date = calendar:universal_time(),
                                                Member = #group_member{
                                                    id = nanoid:gen(),
                                                    group_id = GroupID,
                                                    user_id = UserID,
                                                    role = member,
                                                    join_date = Date
                                                },
                                                UpdatedGroup = Group#group{
                                                    members = [UserID | Group#group.members],
                                                    member_count = Group#group.member_count + 1,
                                                    last_activity = Date
                                                },
                                                mnesia:write(Member),
                                                mnesia:write(UpdatedGroup),
                                                mnesia:write(User#user{groups = [GroupID | User#user.groups]}),
                                                GroupID
                                        end
                                end
                        end;
                    [] ->
                        throw({error, user_not_found})
                end;
            [] ->
                throw(group_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, GroupID} -> GroupID;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

leave_group(UserID, GroupID) ->
    Fun = fun() ->
        case mnesia:read({group, GroupID}) of
            [Group] ->
                case Group#group.owner_id of
                    UserID ->
                        throw({error, owner_cannot_leave});
                    _ ->
                        case lists:member(UserID, Group#group.members) of
                            true ->
                                MemberRecords = mnesia:match_object(
                                    #group_member{group_id = GroupID, user_id = UserID, _ = '_'}
                                ),
                                lists:foreach(fun(M) -> mnesia:delete_object(M) end, MemberRecords),
                                UpdatedMembers = lists:delete(UserID, Group#group.members),
                                UpdatedAdmins = lists:delete(UserID, Group#group.admins),
                                UpdatedGroup = Group#group{
                                    members = UpdatedMembers,
                                    admins = UpdatedAdmins,
                                    member_count = Group#group.member_count - 1
                                },
                                mnesia:write(UpdatedGroup),
                                case mnesia:read({user, UserID}) of
                                    [User] ->
                                        UpdatedGroups = lists:delete(GroupID, User#user.groups),
                                        mnesia:write(User#user{groups = UpdatedGroups});
                                    [] -> ok
                                end,
                                ok;
                            false ->
                                throw({error, not_member})
                        end
                end;
            [] ->
                throw(group_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

remove_member(GroupID, MemberID, AdminID) ->
    Fun = fun() ->
        case mnesia:read({group, GroupID}) of
            [Group] ->
                IsAuthorized = lists:member(AdminID, Group#group.admins) orelse AdminID =:= Group#group.owner_id,
                case IsAuthorized of
                    true ->
                        case MemberID =:= Group#group.owner_id of
                            true ->
                                throw({error, cannot_remove_owner});
                            false ->
                                case lists:member(MemberID, Group#group.members) of
                                    true ->
                                        MemberRecords = mnesia:match_object(
                                            #group_member{group_id = GroupID, user_id = MemberID, _ = '_'}
                                        ),
                                        lists:foreach(fun(M) -> mnesia:delete_object(M) end, MemberRecords),
                                        UpdatedMembers = lists:delete(MemberID, Group#group.members),
                                        UpdatedAdmins = lists:delete(MemberID, Group#group.admins),
                                        UpdatedGroup = Group#group{
                                            members = UpdatedMembers,
                                            admins = UpdatedAdmins,
                                            member_count = Group#group.member_count - 1
                                        },
                                        mnesia:write(UpdatedGroup),
                                        case mnesia:read({user, MemberID}) of
                                            [User] ->
                                                UpdatedGroups = lists:delete(GroupID, User#user.groups),
                                                mnesia:write(User#user{groups = UpdatedGroups});
                                            [] -> ok
                                        end,
                                        ok;
                                    false ->
                                        throw({error, not_member})
                                end
                        end;
                    false ->
                        throw({error, permission_denied})
                end;
            [] ->
                throw(group_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

add_admin(GroupID, UserID, AssignedBy, Permissions) ->
    Fun = fun() ->
        case mnesia:read({group, GroupID}) of
            [Group] ->
                IsAuthorized = Group#group.owner_id =:= AssignedBy orelse lists:member(AssignedBy, Group#group.admins),
                case IsAuthorized of
                    true ->
                        case lists:member(UserID, Group#group.members) of
                            true ->
                                case lists:member(UserID, Group#group.admins) of
                                    false ->
                                        Admin = #group_admin{
                                            id = nanoid:gen(),
                                            group_id = GroupID,
                                            user_id = UserID,
                                            assigned_by = AssignedBy,
                                            permissions = Permissions,
                                            date_assigned = calendar:universal_time()
                                        },
                                        UpdatedGroup = Group#group{
                                            admins = [UserID | Group#group.admins]
                                        },
                                        MemberRecords = mnesia:match_object(
                                            #group_member{group_id = GroupID, user_id = UserID, _ = '_'}
                                        ),
                                        case MemberRecords of
                                            [Member] ->
                                                UpdatedMember = Member#group_member{
                                                    role = admin,
                                                    permissions = Permissions
                                                },
                                                mnesia:write(UpdatedMember);
                                            _ -> ok
                                        end,
                                        mnesia:write(Admin),
                                        mnesia:write(UpdatedGroup),
                                        ok;
                                    true ->
                                        throw({error, already_admin})
                                end;
                            false ->
                                throw({error, not_member})
                        end;
                    false ->
                        throw({error, permission_denied})
                end;
            [] ->
                throw(group_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

remove_admin(GroupID, UserID) ->
    Fun = fun() ->
        case mnesia:read({group, GroupID}) of
            [Group] ->
                case UserID =:= Group#group.owner_id of
                    true ->
                        throw({error, cannot_remove_owner});
                    false ->
                        AdminRecords = mnesia:match_object(
                            #group_admin{group_id = GroupID, user_id = UserID, _ = '_'}
                        ),
                        lists:foreach(fun(A) -> mnesia:delete_object(A) end, AdminRecords),
                        UpdatedAdmins = lists:delete(UserID, Group#group.admins),
                        UpdatedGroup = Group#group{admins = UpdatedAdmins},
                        mnesia:write(UpdatedGroup),
                        MemberRecords = mnesia:match_object(
                            #group_member{group_id = GroupID, user_id = UserID, _ = '_'}
                        ),
                        case MemberRecords of
                            [Member] ->
                                UpdatedMember = Member#group_member{
                                    role = member,
                                    permissions = []
                                },
                                mnesia:write(UpdatedMember);
                            _ -> ok
                        end,
                        ok
                end;
            [] ->
                throw(group_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

update_admin_permissions(GroupID, UserID, NewPermissions) ->
    Fun = fun() ->
        AdminRecords = mnesia:match_object(
            #group_admin{group_id = GroupID, user_id = UserID, _ = '_'}
        ),
        case AdminRecords of
            [Admin] ->
                UpdatedAdmin = Admin#group_admin{permissions = NewPermissions},
                mnesia:write(UpdatedAdmin),
                MemberRecords = mnesia:match_object(
                    #group_member{group_id = GroupID, user_id = UserID, _ = '_'}
                ),
                case MemberRecords of
                    [Member] ->
                        UpdatedMember = Member#group_member{permissions = NewPermissions},
                        mnesia:write(UpdatedMember);
                    _ -> ok
                end,
                ok;
            [] ->
                throw({error, admin_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

send_invite(GroupID, InviterID, InviteeID, Message) ->
    Fun = fun() ->
        case mnesia:read({group, GroupID}) of
            [Group] ->
                Settings = Group#group.settings,
                AllowMemberInvite = maps:get(allow_member_invite, Settings, false),
                IsAuthorized = lists:member(InviterID, Group#group.admins) orelse
                             InviterID =:= Group#group.owner_id orelse
                             (lists:member(InviterID, Group#group.members) andalso AllowMemberInvite),
                case IsAuthorized of
                    true ->
                        case mnesia:read({user, InviteeID}) of
                            [_User] ->
                                case lists:member(InviteeID, Group#group.members) of
                                    true ->
                                        throw({error, already_member});
                                    false ->
                                        case lists:member(InviteeID, Group#group.banned_users) of
                                            true ->
                                                throw({error, user_banned});
                                            false ->
                                                InviteID = nanoid:gen(),
                                                Date = calendar:universal_time(),
                                                Invite = #group_invite{
                                                    id = InviteID,
                                                    group_id = GroupID,
                                                    inviter_id = InviterID,
                                                    invitee_id = InviteeID,
                                                    status = pending,
                                                    message = Message,
                                                    date_created = Date
                                                },
                                                UpdatedGroup = Group#group{
                                                    pending_invites = [InviteID | Group#group.pending_invites]
                                                },
                                                mnesia:write(Invite),
                                                mnesia:write(UpdatedGroup),
                                                Notif = #notif{
                                                    id = nanoid:gen(),
                                                    user_id = InviteeID,
                                                    message = "Group invitation",
                                                    type = group_invite,
                                                    date_created = Date,
                                                    data = #{group_id => GroupID, invite_id => InviteID}
                                                },
                                                mnesia:write(Notif),
                                                InviteID
                                        end
                                end;
                            [] ->
                                throw({error, invitee_not_found})
                        end;
                    false ->
                        throw({error, permission_denied})
                end;
            [] ->
                throw(group_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, InviteID} -> InviteID;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

accept_invite(InviteID, UserID) ->
    Fun = fun() ->
        case mnesia:read({group_invite, InviteID}) of
            [Invite] ->
                case Invite#group_invite.invitee_id of
                    UserID ->
                        case Invite#group_invite.status of
                            pending ->
                                GroupID = Invite#group_invite.group_id,
                                case mnesia:read({group, GroupID}) of
                                    [Group] ->
                                        Date = calendar:universal_time(),
                                        Member = #group_member{
                                            id = nanoid:gen(),
                                            group_id = GroupID,
                                            user_id = UserID,
                                            role = member,
                                            join_date = Date,
                                            invited_by = Invite#group_invite.inviter_id
                                        },
                                        UpdatedInvite = Invite#group_invite{
                                            status = accepted,
                                            date_responded = Date
                                        },
                                        UpdatedGroup = Group#group{
                                            members = [UserID | Group#group.members],
                                            member_count = Group#group.member_count + 1,
                                            pending_invites = lists:delete(InviteID, Group#group.pending_invites),
                                            last_activity = Date
                                        },
                                        case mnesia:read({user, UserID}) of
                                            [User] ->
                                                mnesia:write(User#user{groups = [GroupID | User#user.groups]});
                                            [] -> ok
                                        end,
                                        mnesia:write(Member),
                                        mnesia:write(UpdatedInvite),
                                        mnesia:write(UpdatedGroup),
                                        GroupID;
                                    [] ->
                                        throw(group_not_found)
                                end;
                            _ ->
                                throw({error, invite_already_responded})
                        end;
                    _ ->
                        throw({error, unauthorized})
                end;
            [] ->
                throw({error, invite_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, GroupID} -> GroupID;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

reject_invite(InviteID, UserID) ->
    Fun = fun() ->
        case mnesia:read({group_invite, InviteID}) of
            [Invite] ->
                case Invite#group_invite.invitee_id of
                    UserID ->
                        case Invite#group_invite.status of
                            pending ->
                                Date = calendar:universal_time(),
                                UpdatedInvite = Invite#group_invite{
                                    status = rejected,
                                    date_responded = Date
                                },
                                GroupID = Invite#group_invite.group_id,
                                case mnesia:read({group, GroupID}) of
                                    [Group] ->
                                        UpdatedGroup = Group#group{
                                            pending_invites = lists:delete(InviteID, Group#group.pending_invites)
                                        },
                                        mnesia:write(UpdatedGroup);
                                    [] -> ok
                                end,
                                mnesia:write(UpdatedInvite),
                                ok;
                            _ ->
                                throw({error, invite_already_responded})
                        end;
                    _ ->
                        throw({error, unauthorized})
                end;
            [] ->
                throw({error, invite_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

cancel_invite(InviteID, InviterID) ->
    Fun = fun() ->
        case mnesia:read({group_invite, InviteID}) of
            [Invite] ->
                GroupID = Invite#group_invite.group_id,
                case mnesia:read({group, GroupID}) of
                    [Group] ->
                        IsAuthorized = Invite#group_invite.inviter_id =:= InviterID orelse
                                     Group#group.owner_id =:= InviterID orelse
                                     lists:member(InviterID, Group#group.admins),
                        case IsAuthorized of
                            true ->
                                UpdatedGroup = Group#group{
                                    pending_invites = lists:delete(InviteID, Group#group.pending_invites)
                                },
                                mnesia:write(UpdatedGroup),
                                mnesia:delete({group_invite, InviteID}),
                                ok;
                            false ->
                                throw({error, unauthorized})
                        end;
                    [] ->
                        throw(group_not_found)
                end;
            [] ->
                throw({error, invite_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

send_message(GroupID, UserID, Content, Media) ->
    Fun = fun() ->
        case mnesia:read({group, GroupID}) of
            [Group] ->
                case lists:member(UserID, Group#group.members) of
                    true ->
                        Settings = Group#group.settings,
                        MuteAll = maps:get(mute_all, Settings, false),
                        MemberRecords = mnesia:match_object(
                            #group_member{group_id = GroupID, user_id = UserID, _ = '_'}
                        ),
                        case MemberRecords of
                            [Member] when Member#group_member.muted =:= true ->
                                throw({error, user_muted});
                            _ when MuteAll =:= true ->
                                IsAdmin = lists:member(UserID, Group#group.admins) orelse
                                         UserID =:= Group#group.owner_id,
                                case IsAdmin of
                                    false ->
                                        throw({error, group_muted});
                                    true ->
                                        create_group_message(GroupID, UserID, Content, Media, Group)
                                end;
                            _ ->
                                create_group_message(GroupID, UserID, Content, Media, Group)
                        end;
                    false ->
                        throw({error, not_member})
                end;
            [] ->
                throw(group_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, MessageID} -> MessageID;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

create_group_message(GroupID, UserID, Content, Media, Group) ->
    MessageID = nanoid:gen(),
    Date = calendar:universal_time(),
    Message = #group_message{
        id = MessageID,
        group_id = GroupID,
        user_id = UserID,
        content = Content,
        media = Media,
        date_created = Date
    },
    UpdatedGroup = Group#group{
        messages = [MessageID | Group#group.messages],
        last_activity = Date
    },
    mnesia:write(Message),
    mnesia:write(UpdatedGroup),
    MessageID.

edit_message(MessageID, UserID, NewContent) ->
    Fun = fun() ->
        case mnesia:read({group_message, MessageID}) of
            [Message] ->
                case Message#group_message.user_id of
                    UserID ->
                        case Message#group_message.deleted of
                            true ->
                                throw({error, message_deleted});
                            false ->
                                UpdatedMessage = Message#group_message{
                                    content = NewContent,
                                    edited = true,
                                    date_updated = calendar:universal_time()
                                },
                                mnesia:write(UpdatedMessage),
                                MessageID
                        end;
                    _ ->
                        throw({error, unauthorized})
                end;
            [] ->
                throw({error, message_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, MessageID} -> MessageID;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

delete_message(MessageID, UserID) ->
    Fun = fun() ->
        case mnesia:read({group_message, MessageID}) of
            [Message] ->
                GroupID = Message#group_message.group_id,
                case mnesia:read({group, GroupID}) of
                    [Group] ->
                        IsOwner = Message#group_message.user_id =:= UserID,
                        IsGroupAdmin = lists:member(UserID, Group#group.admins) orelse
                                      UserID =:= Group#group.owner_id,
                        case IsOwner orelse IsGroupAdmin of
                            true ->
                                UpdatedMessage = Message#group_message{
                                    deleted = true,
                                    content = "",
                                    date_updated = calendar:universal_time()
                                },
                                mnesia:write(UpdatedMessage),
                                ok;
                            false ->
                                throw({error, unauthorized})
                        end;
                    [] ->
                        throw(group_not_found)
                end;
            [] ->
                throw({error, message_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

pin_message(MessageID, UserID) ->
    Fun = fun() ->
        case mnesia:read({group_message, MessageID}) of
            [Message] ->
                GroupID = Message#group_message.group_id,
                case mnesia:read({group, GroupID}) of
                    [Group] ->
                        IsAdmin = lists:member(UserID, Group#group.admins) orelse
                                 UserID =:= Group#group.owner_id,
                        case IsAdmin of
                            true ->
                                UpdatedMessage = Message#group_message{pinned = true},
                                UpdatedGroup = Group#group{
                                    pinned_messages = [MessageID | Group#group.pinned_messages]
                                },
                                mnesia:write(UpdatedMessage),
                                mnesia:write(UpdatedGroup),
                                ok;
                            false ->
                                throw({error, permission_denied})
                        end;
                    [] ->
                        throw(group_not_found)
                end;
            [] ->
                throw({error, message_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

unpin_message(MessageID, UserID) ->
    Fun = fun() ->
        case mnesia:read({group_message, MessageID}) of
            [Message] ->
                GroupID = Message#group_message.group_id,
                case mnesia:read({group, GroupID}) of
                    [Group] ->
                        IsAdmin = lists:member(UserID, Group#group.admins) orelse
                                 UserID =:= Group#group.owner_id,
                        case IsAdmin of
                            true ->
                                UpdatedMessage = Message#group_message{pinned = false},
                                UpdatedGroup = Group#group{
                                    pinned_messages = lists:delete(MessageID, Group#group.pinned_messages)
                                },
                                mnesia:write(UpdatedMessage),
                                mnesia:write(UpdatedGroup),
                                ok;
                            false ->
                                throw({error, permission_denied})
                        end;
                    [] ->
                        throw(group_not_found)
                end;
            [] ->
                throw({error, message_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

react_to_message(MessageID, UserID, ReactionType) ->
    Fun = fun() ->
        case mnesia:read({group_message, MessageID}) of
            [Message] ->
                Reactions = Message#group_message.reactions,
                ReactionCounts = Message#group_message.reaction_counts,
                CurrentReactionList = maps:get(ReactionType, Reactions, []),
                case lists:member(UserID, CurrentReactionList) of
                    true ->
                        UpdatedList = lists:delete(UserID, CurrentReactionList),
                        UpdatedReactions = maps:put(ReactionType, UpdatedList, Reactions),
                        CurrentCount = maps:get(ReactionType, ReactionCounts, 0),
                        UpdatedCounts = maps:put(ReactionType, max(0, CurrentCount - 1), ReactionCounts),
                        UpdatedMessage = Message#group_message{
                            reactions = UpdatedReactions,
                            reaction_counts = UpdatedCounts
                        },
                        mnesia:write(UpdatedMessage),
                        {ok, removed};
                    false ->
                        UpdatedList = [UserID | CurrentReactionList],
                        UpdatedReactions = maps:put(ReactionType, UpdatedList, Reactions),
                        CurrentCount = maps:get(ReactionType, ReactionCounts, 0),
                        UpdatedCounts = maps:put(ReactionType, CurrentCount + 1, ReactionCounts),
                        UpdatedMessage = Message#group_message{
                            reactions = UpdatedReactions,
                            reaction_counts = UpdatedCounts
                        },
                        mnesia:write(UpdatedMessage),
                        {ok, added}
                end;
            [] ->
                throw({error, message_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

get_group_messages(GroupID, Limit) ->
    Fun = fun() ->
        case mnesia:read({group, GroupID}) of
            [Group] ->
                MessageIDs = lists:sublist(Group#group.messages, Limit),
                Messages = lists:foldl(fun(MsgID, Acc) ->
                    case mnesia:read({group_message, MsgID}) of
                        [Msg] -> [Msg | Acc];
                        [] -> Acc
                    end
                end, [], MessageIDs),
                lists:reverse(Messages);
            [] ->
                throw(group_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Messages} -> Messages;
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

    get_member_groups(UserID) ->
        Fun = fun() ->
            case mnesia:read({user, UserID}) of
                [User] ->
                    GroupIDs = case User#user.groups of
                        undefined -> [];
                        Groups when is_list(Groups) -> Groups;
                        _ -> []
                    end,
                    lists:foldl(fun(GID, Acc) ->
                        case mnesia:read({group, GID}) of
                            [G] -> [G | Acc];
                            [] -> Acc
                        end
                    end, [], GroupIDs);
                [] ->
                    []
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, Groups} -> Groups;
            {aborted, Reason} ->
                error_logger:error_msg("Failed to get member groups: ~p~n", [Reason]),
                []
        end.


ban_user(GroupID, UserID, AdminID) ->
    Fun = fun() ->
        case mnesia:read({group, GroupID}) of
            [Group] ->
                IsAdmin = lists:member(AdminID, Group#group.admins) orelse
                         AdminID =:= Group#group.owner_id,
                case IsAdmin of
                    true ->
                        case UserID =:= Group#group.owner_id of
                            true ->
                                throw({error, cannot_ban_owner});
                            false ->
                                MemberRecords = mnesia:match_object(
                                    #group_member{group_id = GroupID, user_id = UserID, _ = '_'}
                                ),
                                lists:foreach(fun(M) -> mnesia:delete_object(M) end, MemberRecords),
                                UpdatedMembers = lists:delete(UserID, Group#group.members),
                                UpdatedAdmins = lists:delete(UserID, Group#group.admins),
                                UpdatedGroup = Group#group{
                                    members = UpdatedMembers,
                                    admins = UpdatedAdmins,
                                    banned_users = [UserID | Group#group.banned_users],
                                    member_count = case lists:member(UserID, Group#group.members) of
                                        true -> Group#group.member_count - 1;
                                        false -> Group#group.member_count
                                    end
                                },
                                mnesia:write(UpdatedGroup),
                                case mnesia:read({user, UserID}) of
                                    [User] ->
                                        UpdatedGroups = lists:delete(GroupID, User#user.groups),
                                        mnesia:write(User#user{groups = UpdatedGroups});
                                    [] -> ok
                                end,
                                ok
                        end;
                    false ->
                        throw({error, permission_denied})
                end;
            [] ->
                throw(group_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

unban_user(GroupID, UserID) ->
    Fun = fun() ->
        case mnesia:read({group, GroupID}) of
            [Group] ->
                UpdatedGroup = Group#group{
                    banned_users = lists:delete(UserID, Group#group.banned_users)
                },
                mnesia:write(UpdatedGroup),
                ok;
            [] ->
                throw(group_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, group_not_found} -> throw(group_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

mute_member(GroupID, UserID) ->
    Fun = fun() ->
        MemberRecords = mnesia:match_object(
            #group_member{group_id = GroupID, user_id = UserID, _ = '_'}
        ),
        case MemberRecords of
            [Member] ->
                UpdatedMember = Member#group_member{muted = true},
                mnesia:write(UpdatedMember),
                ok;
            [] ->
                throw({error, member_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

unmute_member(GroupID, UserID) ->
    Fun = fun() ->
        MemberRecords = mnesia:match_object(
            #group_member{group_id = GroupID, user_id = UserID, _ = '_'}
        ),
        case MemberRecords of
            [Member] ->
                UpdatedMember = Member#group_member{muted = false},
                mnesia:write(UpdatedMember),
                ok;
            [] ->
                throw({error, member_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

search_groups(Query, Privacy) ->
    LowerQuery = string:lowercase(Query),
    Fun = fun() ->
        AllGroups = mnesia:match_object(#group{_ = '_'}),
        lists:filter(fun(G) ->
            UniqueName = G#group.unique_name,
            Name = G#group.name,
            Desc = G#group.description,
            UniqueNameMatch = UniqueName =/= undefined andalso
                             string:find(string:lowercase(UniqueName), LowerQuery) =/= nomatch,
            NameMatch = string:find(string:lowercase(Name), LowerQuery) =/= nomatch,
            DescMatch = case Desc of
                undefined -> false;
                D -> string:find(string:lowercase(D), LowerQuery) =/= nomatch
            end,
            PrivacyMatch = case Privacy of
                all -> true;
                _ -> G#group.privacy =:= Privacy
            end,
            (UniqueNameMatch orelse NameMatch orelse DescMatch) andalso PrivacyMatch
        end, AllGroups)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Groups} -> Groups;
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

get_group_members(GroupID) ->
    Fun = fun() ->
        MemberRecords = mnesia:match_object(#group_member{group_id = GroupID, _ = '_'}),
        MemberRecords
    end,
    case mnesia:transaction(Fun) of
        {atomic, Members} -> Members;
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

update_member_settings(GroupID, UserID, SettingsMap) ->
    Fun = fun() ->
        MemberRecords = mnesia:match_object(
            #group_member{group_id = GroupID, user_id = UserID, _ = '_'}
        ),
        case MemberRecords of
            [Member] ->
                UpdatedMember = Member#group_member{
                    notifications = maps:get(notifications, SettingsMap, Member#group_member.notifications),
                    muted = maps:get(muted, SettingsMap, Member#group_member.muted)
                },
                mnesia:write(UpdatedMember),
                ok;
            [] ->
                throw({error, member_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

    create_channel(OwnerID, UniqueName, Name, Description, Privacy, Category, Settings) ->
        Fun = fun() ->
            case mnesia:read({user, OwnerID}) of
                [_Owner] ->
                    case mnesia:match_object(#channel{unique_name = UniqueName, _ = '_'}) of
                        [] -> ok;
                        [_|_] -> throw({error, unique_name_already_taken})
                    end,
                    ChannelID = nanoid:gen(),
                    Date = calendar:universal_time(),

                    NormalizedCategory = normalize_category(Category),

                    Channel = #channel{
                        id = ChannelID,
                        unique_name = UniqueName,
                        name = Name,
                        description = Description,
                        privacy = Privacy,
                        owner_id = OwnerID,
                        admins = [OwnerID],
                        subscribers = [OwnerID],
                        category = NormalizedCategory,
                        settings = maps:merge(#{
                            allow_comments => true,
                            allow_reactions => true,
                            subscriber_posts => false,
                            moderation_required => false
                        }, Settings),
                        subscriber_count = 1,
                        date_created = Date,
                        date_updated = Date,
                        last_activity = Date
                    },
                    Subscriber = #channel_subscriber{
                        id = nanoid:gen(),
                        channel_id = ChannelID,
                        user_id = OwnerID,
                        subscribe_date = Date
                    },
                    mnesia:write(Channel),
                    mnesia:write(Subscriber),
                    ChannelID;
                [] ->
                    throw({error, user_not_found})
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ChannelID} -> ChannelID;
            {aborted, {error, Error}} -> throw(Error);
            {aborted, Reason} -> throw({transaction_failed, Reason})
        end.

get_channel(ChannelID) ->
    Fun = fun() ->
        case mnesia:read({channel, ChannelID}) of
            [Channel] -> Channel;
            [] -> throw(channel_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Channel} -> Channel;
        {aborted, channel_not_found} -> throw(channel_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

    update_channel(ChannelID, UpdateMap) ->
        Fun = fun() ->
            case mnesia:read({channel, ChannelID}) of
                [Channel] ->
                    NewUniqueName = maps:get(unique_name, UpdateMap, Channel#channel.unique_name),
                    case NewUniqueName =/= Channel#channel.unique_name of
                        true ->
                            case mnesia:match_object(#channel{unique_name = NewUniqueName, _ = '_'}) of
                                [] -> ok;
                                [_|_] -> throw({error, unique_name_already_taken})
                            end;
                        false -> ok
                    end,

                    NewCategory = case maps:get(category, UpdateMap, undefined) of
                        undefined -> Channel#channel.category;
                        Cat -> normalize_category(Cat)
                    end,

                    UpdatedChannel = Channel#channel{
                        unique_name = NewUniqueName,
                        name = maps:get(name, UpdateMap, Channel#channel.name),
                        description = maps:get(description, UpdateMap, Channel#channel.description),
                        privacy = maps:get(privacy, UpdateMap, Channel#channel.privacy),
                        avatar_url = maps:get(avatar_url, UpdateMap, Channel#channel.avatar_url),
                        banner_url = maps:get(banner_url, UpdateMap, Channel#channel.banner_url),
                        category = NewCategory,
                        tags = maps:get(tags, UpdateMap, Channel#channel.tags),
                        settings = maps:merge(Channel#channel.settings, maps:get(settings, UpdateMap, #{})),
                        date_updated = calendar:universal_time()
                    },
                    mnesia:write(UpdatedChannel),
                    ChannelID;
                [] ->
                    throw(channel_not_found)
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ChannelID} -> ChannelID;
            {aborted, channel_not_found} -> throw(channel_not_found);
            {aborted, {error, Error}} -> throw(Error);
            {aborted, Reason} -> throw({transaction_failed, Reason})
        end.

delete_channel(ChannelID) ->
    Fun = fun() ->
        case mnesia:read({channel, ChannelID}) of
            [_Channel] ->
                SubRecords = mnesia:match_object(#channel_subscriber{channel_id = ChannelID, _ = '_'}),
                lists:foreach(fun(S) -> mnesia:delete_object(S) end, SubRecords),
                Posts = mnesia:match_object(#channel_post{channel_id = ChannelID, _ = '_'}),
                lists:foreach(fun(P) -> mnesia:delete_object(P) end, Posts),
                mnesia:delete({channel, ChannelID}),
                ok;
            [] ->
                throw(channel_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, channel_not_found} -> throw(channel_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

subscribe_channel(UserID, ChannelID) ->
    Fun = fun() ->
        case mnesia:read({channel, ChannelID}) of
            [Channel] ->
                case lists:member(UserID, Channel#channel.subscribers) of
                    true ->
                        throw({error, already_subscribed});
                    false ->
                        Date = calendar:universal_time(),
                        Subscriber = #channel_subscriber{
                            id = nanoid:gen(),
                            channel_id = ChannelID,
                            user_id = UserID,
                            subscribe_date = Date
                        },
                        UpdatedChannel = Channel#channel{
                            subscribers = [UserID | Channel#channel.subscribers],
                            subscriber_count = Channel#channel.subscriber_count + 1
                        },
                        mnesia:write(Subscriber),
                        mnesia:write(UpdatedChannel),
                        ChannelID
                end;
            [] ->
                throw(channel_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ChannelID} -> ChannelID;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, channel_not_found} -> throw(channel_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

unsubscribe_channel(UserID, ChannelID) ->
    Fun = fun() ->
        case mnesia:read({channel, ChannelID}) of
            [Channel] ->
                SubRecords = mnesia:match_object(
                    #channel_subscriber{channel_id = ChannelID, user_id = UserID, _ = '_'}
                ),
                lists:foreach(fun(S) -> mnesia:delete_object(S) end, SubRecords),
                UpdatedChannel = Channel#channel{
                    subscribers = lists:delete(UserID, Channel#channel.subscribers),
                    subscriber_count = max(0, Channel#channel.subscriber_count - 1)
                },
                mnesia:write(UpdatedChannel),
                ok;
            [] ->
                throw(channel_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, channel_not_found} -> throw(channel_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

create_channel_post(ChannelID, UserID, Content, Media) ->
    Fun = fun() ->
        case mnesia:read({channel, ChannelID}) of
            [Channel] ->
                Settings = Channel#channel.settings,
                SubscriberPosts = maps:get(subscriber_posts, Settings, false),
                IsAdmin = lists:member(UserID, Channel#channel.admins) orelse
                         UserID =:= Channel#channel.owner_id,
                case IsAdmin orelse SubscriberPosts of
                    true ->
                        PostID = nanoid:gen(),
                        Date = calendar:universal_time(),
                        Post = #channel_post{
                            id = PostID,
                            channel_id = ChannelID,
                            user_id = UserID,
                            content = Content,
                            media = Media,
                            date_created = Date
                        },
                        UpdatedChannel = Channel#channel{
                            posts = [PostID | Channel#channel.posts],
                            last_activity = Date
                        },
                        mnesia:write(Post),
                        mnesia:write(UpdatedChannel),
                        PostID;
                    false ->
                        throw({error, permission_denied})
                end;
            [] ->
                throw(channel_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, PostID} -> PostID;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, channel_not_found} -> throw(channel_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

edit_channel_post(PostID, UserID, NewContent) ->
    Fun = fun() ->
        case mnesia:read({channel_post, PostID}) of
            [Post] ->
                case Post#channel_post.user_id of
                    UserID ->
                        UpdatedPost = Post#channel_post{
                            content = NewContent,
                            edited = true,
                            date_updated = calendar:universal_time()
                        },
                        mnesia:write(UpdatedPost),
                        PostID;
                    _ ->
                        throw({error, unauthorized})
                end;
            [] ->
                throw({error, post_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, PostID} -> PostID;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

delete_channel_post(PostID, UserID) ->
    Fun = fun() ->
        case mnesia:read({channel_post, PostID}) of
            [Post] ->
                ChannelID = Post#channel_post.channel_id,
                case mnesia:read({channel, ChannelID}) of
                    [Channel] ->
                        IsOwner = Post#channel_post.user_id =:= UserID,
                        IsChannelAdmin = lists:member(UserID, Channel#channel.admins) orelse
                                        UserID =:= Channel#channel.owner_id,
                        case IsOwner orelse IsChannelAdmin of
                            true ->
                                UpdatedPost = Post#channel_post{
                                    deleted = true,
                                    content = "",
                                    date_updated = calendar:universal_time()
                                },
                                mnesia:write(UpdatedPost),
                                ok;
                            false ->
                                throw({error, unauthorized})
                        end;
                    [] ->
                        throw(channel_not_found)
                end;
            [] ->
                throw({error, post_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, channel_not_found} -> throw(channel_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

pin_channel_post(PostID, UserID) ->
    Fun = fun() ->
        case mnesia:read({channel_post, PostID}) of
            [Post] ->
                ChannelID = Post#channel_post.channel_id,
                case mnesia:read({channel, ChannelID}) of
                    [Channel] ->
                        IsAdmin = lists:member(UserID, Channel#channel.admins) orelse
                                 UserID =:= Channel#channel.owner_id,
                        case IsAdmin of
                            true ->
                                UpdatedPost = Post#channel_post{pinned = true},
                                UpdatedChannel = Channel#channel{
                                    pinned_posts = [PostID | Channel#channel.pinned_posts]
                                },
                                mnesia:write(UpdatedPost),
                                mnesia:write(UpdatedChannel),
                                ok;
                            false ->
                                throw({error, permission_denied})
                        end;
                    [] ->
                        throw(channel_not_found)
                end;
            [] ->
                throw({error, post_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, channel_not_found} -> throw(channel_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

unpin_channel_post(PostID, UserID) ->
    Fun = fun() ->
        case mnesia:read({channel_post, PostID}) of
            [Post] ->
                ChannelID = Post#channel_post.channel_id,
                case mnesia:read({channel, ChannelID}) of
                    [Channel] ->
                        IsAdmin = lists:member(UserID, Channel#channel.admins) orelse
                                 UserID =:= Channel#channel.owner_id,
                        case IsAdmin of
                            true ->
                                UpdatedPost = Post#channel_post{pinned = false},
                                UpdatedChannel = Channel#channel{
                                    pinned_posts = lists:delete(PostID, Channel#channel.pinned_posts)
                                },
                                mnesia:write(UpdatedPost),
                                mnesia:write(UpdatedChannel),
                                ok;
                            false ->
                                throw({error, permission_denied})
                        end;
                    [] ->
                        throw(channel_not_found)
                end;
            [] ->
                throw({error, post_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, channel_not_found} -> throw(channel_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

react_to_channel_post(PostID, UserID, ReactionType) ->
    Fun = fun() ->
        case mnesia:read({channel_post, PostID}) of
            [Post] ->
                Reactions = Post#channel_post.reactions,
                ReactionCounts = Post#channel_post.reaction_counts,
                CurrentReactionList = maps:get(ReactionType, Reactions, []),
                case lists:member(UserID, CurrentReactionList) of
                    true ->
                        UpdatedList = lists:delete(UserID, CurrentReactionList),
                        UpdatedReactions = maps:put(ReactionType, UpdatedList, Reactions),
                        CurrentCount = maps:get(ReactionType, ReactionCounts, 0),
                        UpdatedCounts = maps:put(ReactionType, max(0, CurrentCount - 1), ReactionCounts),
                        UpdatedPost = Post#channel_post{
                            reactions = UpdatedReactions,
                            reaction_counts = UpdatedCounts
                        },
                        mnesia:write(UpdatedPost),
                        {ok, removed};
                    false ->
                        UpdatedList = [UserID | CurrentReactionList],
                        UpdatedReactions = maps:put(ReactionType, UpdatedList, Reactions),
                        CurrentCount = maps:get(ReactionType, ReactionCounts, 0),
                        UpdatedCounts = maps:put(ReactionType, CurrentCount + 1, ReactionCounts),
                        UpdatedPost = Post#channel_post{
                            reactions = UpdatedReactions,
                            reaction_counts = UpdatedCounts
                        },
                        mnesia:write(UpdatedPost),
                        {ok, added}
                end;
            [] ->
                throw({error, post_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

comment_on_channel_post(PostID, UserID, Content, _Media) ->
    Fun = fun() ->
        case mnesia:read({channel_post, PostID}) of
            [Post] ->
                ChannelID = Post#channel_post.channel_id,
                case mnesia:read({channel, ChannelID}) of
                    [Channel] ->
                        Settings = Channel#channel.settings,
                        AllowComments = maps:get(allow_comments, Settings, true),
                        case AllowComments of
                            true ->
                                CommentID = nanoid:gen(),
                                Date = calendar:universal_time(),
                                Comment = #comment{
                                    id = CommentID,
                                    user_id = UserID,
                                    post = PostID,
                                    author = UserID,
                                    content = Content,
                                    date_created = Date
                                },
                                UpdatedPost = Post#channel_post{
                                    comments = [CommentID | Post#channel_post.comments]
                                },
                                mnesia:write(Comment),
                                mnesia:write(UpdatedPost),
                                CommentID;
                            false ->
                                throw({error, comments_disabled})
                        end;
                    [] ->
                        throw(channel_not_found)
                end;
            [] ->
                throw({error, post_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, CommentID} -> CommentID;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, channel_not_found} -> throw(channel_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

get_channel_posts(ChannelID, Limit) ->
    Fun = fun() ->
        case mnesia:read({channel, ChannelID}) of
            [Channel] ->
                PostIDs = lists:sublist(Channel#channel.posts, Limit),
                Posts = lists:foldl(fun(PID, Acc) ->
                    case mnesia:read({channel_post, PID}) of
                        [P] -> [P | Acc];
                        [] -> Acc
                    end
                end, [], PostIDs),
                lists:reverse(Posts);
            [] ->
                throw(channel_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Posts} -> Posts;
        {aborted, channel_not_found} -> throw(channel_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

search_channels(Query, Privacy) ->
    LowerQuery = string:lowercase(Query),
    Fun = fun() ->
        AllChannels = mnesia:match_object(#channel{_ = '_'}),
        lists:filter(fun(C) ->
            UniqueName = C#channel.unique_name,
            Name = C#channel.name,
            Desc = C#channel.description,
            UniqueNameMatch = UniqueName =/= undefined andalso
                             string:find(string:lowercase(UniqueName), LowerQuery) =/= nomatch,
            NameMatch = string:find(string:lowercase(Name), LowerQuery) =/= nomatch,
            DescMatch = case Desc of
                undefined -> false;
                D -> string:find(string:lowercase(D), LowerQuery) =/= nomatch
            end,
            PrivacyMatch = case Privacy of
                all -> true;
                _ -> C#channel.privacy =:= Privacy
            end,
            (UniqueNameMatch orelse NameMatch orelse DescMatch) andalso PrivacyMatch
        end, AllChannels)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Channels} -> Channels;
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

get_user_channels(UserID) ->
    Fun = fun() ->
        SubRecords = mnesia:match_object(#channel_subscriber{user_id = UserID, _ = '_'}),
        ChannelIDs = [S#channel_subscriber.channel_id || S <- SubRecords],
        Channels = lists:foldl(fun(CID, Acc) ->
            case mnesia:read({channel, CID}) of
                [C] -> [C | Acc];
                [] -> Acc
            end
        end, [], ChannelIDs),
        lists:reverse(Channels)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Channels} -> Channels;
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

add_channel_admin(ChannelID, UserID, AssignedBy) ->
    Fun = fun() ->
        case mnesia:read({channel, ChannelID}) of
            [Channel] ->
                IsAuthorized = Channel#channel.owner_id =:= AssignedBy orelse
                             lists:member(AssignedBy, Channel#channel.admins),
                case IsAuthorized of
                    true ->
                        case lists:member(UserID, Channel#channel.subscribers) of
                            true ->
                                case lists:member(UserID, Channel#channel.admins) of
                                    false ->
                                        UpdatedChannel = Channel#channel{
                                            admins = [UserID | Channel#channel.admins]
                                        },
                                        mnesia:write(UpdatedChannel),
                                        ok;
                                    true ->
                                        throw({error, already_admin})
                                end;
                            false ->
                                throw({error, not_subscribed})
                        end;
                    false ->
                        throw({error, permission_denied})
                end;
            [] ->
                throw(channel_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, channel_not_found} -> throw(channel_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

remove_channel_admin(ChannelID, UserID) ->
    Fun = fun() ->
        case mnesia:read({channel, ChannelID}) of
            [Channel] ->
                case UserID =:= Channel#channel.owner_id of
                    true ->
                        throw({error, cannot_remove_owner});
                    false ->
                        UpdatedAdmins = lists:delete(UserID, Channel#channel.admins),
                        UpdatedChannel = Channel#channel{admins = UpdatedAdmins},
                        mnesia:write(UpdatedChannel),
                        ok
                end;
            [] ->
                throw(channel_not_found)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, {error, Error}} -> throw(Error);
        {aborted, channel_not_found} -> throw(channel_not_found);
        {aborted, Reason} -> throw({transaction_failed, Reason})
    end.

    send_channel_invite(ChannelID, InviterID, InviteeID, Message) ->
        Fun = fun() ->
            case mnesia:read({channel, ChannelID}) of
                [Channel] ->
                    IsAdmin = lists:member(InviterID, Channel#channel.admins) orelse
                             InviterID =:= Channel#channel.owner_id,
                    case IsAdmin of
                        true ->
                            case mnesia:read({user, InviteeID}) of
                                [_User] ->
                                    case lists:member(InviteeID, Channel#channel.subscribers) of
                                        true ->
                                            throw({error, already_subscribed});
                                        false ->
                                            InviteID = nanoid:gen(),
                                            Date = calendar:universal_time(),
                                            Invite = #channel_invite{
                                                id = InviteID,
                                                channel_id = ChannelID,
                                                inviter_id = InviterID,
                                                invitee_id = InviteeID,
                                                status = pending,
                                                message = Message,
                                                date_created = Date
                                            },
                                            mnesia:write(Invite),
                                            Notif = #notif{
                                                id = nanoid:gen(),
                                                user_id = InviteeID,
                                                message = "Channel invitation",
                                                type = channel_invite,
                                                date_created = Date,
                                                data = #{channel_id => ChannelID, invite_id => InviteID}
                                            },
                                            mnesia:write(Notif),
                                            InviteID
                                    end;
                                [] ->
                                    throw({error, invitee_not_found})
                            end;
                        false ->
                            throw({error, permission_denied})
                    end;
                [] ->
                    throw(channel_not_found)
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, InviteID} -> InviteID;
            {aborted, {error, Error}} -> throw(Error);
            {aborted, channel_not_found} -> throw(channel_not_found);
            {aborted, Reason} -> throw({transaction_failed, Reason})
        end.
