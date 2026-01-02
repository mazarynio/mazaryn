-module(instructordb).
-include("../ml_records.hrl").
-include("../records.hrl").

-export([
    is_verified_instructor/1,
    verify_instructor/2,
    unverify_instructor/2,
    list_verified_instructors/0,
    get_instructor_info/1,
    request_verification/4,
    get_pending_requests/0,
    approve_request/2,
    reject_request/3,
    get_request/1
]).

normalize_user_id(UserId) when is_binary(UserId) ->
    binary_to_list(UserId);
normalize_user_id(UserId) when is_list(UserId) ->
    UserId;
normalize_user_id(_) ->
    undefined.

is_verified_instructor(UserId) ->
    io:format("🔍 [instructordb] is_verified_instructor called~n"),
    io:format("   User ID (raw): ~p~n", [UserId]),

    NormalizedId = normalize_user_id(UserId),
    io:format("   Normalized ID: ~p~n", [NormalizedId]),

    case NormalizedId of
        undefined ->
            io:format("❌ Invalid user ID type~n"),
            false;
        _ ->
            F = fun() ->
                mnesia:read({verified_instructor, NormalizedId})
            end,

            case mnesia:transaction(F) of
                {atomic, []} ->
                    io:format("   ❌ Not verified~n"),
                    false;
                {atomic, [_Record]} ->
                    io:format("   ✅ Verified~n"),
                    true;
                {aborted, Reason} ->
                    io:format("   🔴 Error: ~p~n", [Reason]),
                    false
            end
    end.

verify_instructor(AdminUsername, UserId) ->
    io:format("🔵 [instructordb] verify_instructor called~n"),
    io:format("   Admin: ~p~n", [AdminUsername]),
    io:format("   User ID (raw): ~p~n", [UserId]),

    NormalizedId = normalize_user_id(UserId),
    io:format("   Normalized ID: ~p~n", [NormalizedId]),

    case admindb:is_admin(AdminUsername) of
        false ->
            io:format("🔴 [instructordb] Admin check FAILED~n"),
            {error, unauthorized};
        true ->
            io:format("✅ [instructordb] Admin check PASSED~n"),

            F = fun() ->
                case mnesia:read({verified_instructor, NormalizedId}) of
                    [_] ->
                        io:format("⚠️  [instructordb] Already verified~n"),
                        {error, already_verified};
                    [] ->
                        io:format("🔵 [instructordb] Writing to Mnesia~n"),
                        Record = #verified_instructor{
                            user_id = NormalizedId,
                            verified_at = calendar:universal_time(),
                            verified_by = AdminUsername
                        },
                        mnesia:write(Record),
                        ok
                end
            end,

            case mnesia:transaction(F) of
                {atomic, ok} ->
                    io:format("✅ [instructordb] User verified successfully~n"),
                    remove_pending_request(NormalizedId),
                    ok;
                {atomic, Error} ->
                    Error;
                {aborted, Reason} ->
                    io:format("🔴 [instructordb] Mnesia error: ~p~n", [Reason]),
                    {error, Reason}
            end
    end.

unverify_instructor(AdminUsername, UserId) ->
    io:format("🔵 [instructordb] unverify_instructor called~n"),

    NormalizedId = normalize_user_id(UserId),

    case admindb:is_admin(AdminUsername) of
        false -> {error, unauthorized};
        true ->
            F = fun() ->
                mnesia:delete({verified_instructor, NormalizedId})
            end,

            case mnesia:transaction(F) of
                {atomic, ok} ->
                    io:format("✅ [instructordb] User unverified~n"),
                    ok;
                {aborted, Reason} ->
                    {error, Reason}
            end
    end.

list_verified_instructors() ->
    F = fun() ->
        mnesia:foldl(
            fun(#verified_instructor{user_id = UserId}, Acc) ->
                [UserId | Acc]
            end,
            [],
            verified_instructor
        )
    end,

    case mnesia:transaction(F) of
        {atomic, List} -> List;
        {aborted, _} -> []
    end.

request_verification(UserId, Name, Family, Reason) ->
    io:format("🔵 [instructordb] request_verification called~n"),
    io:format("   User ID: ~p~n", [UserId]),
    io:format("   Name: ~p ~p~n", [Name, Family]),

    NormalizedId = normalize_user_id(UserId),

    F = fun() ->
        Record = #instructor_request{
            user_id = NormalizedId,
            name = Name,
            family = Family,
            reason = Reason,
            requested_at = calendar:universal_time(),
            status = pending,
            rejection_reason = undefined
        },
        mnesia:write(Record)
    end,

    case mnesia:transaction(F) of
        {atomic, ok} ->
            io:format("✅ [instructordb] Request submitted~n"),
            ok;
        {aborted, Reason} ->
            io:format("🔴 [instructordb] Error: ~p~n", [Reason]),
            {error, Reason}
    end.

get_pending_requests() ->
    io:format("🔍 [instructordb] get_pending_requests called~n"),

    F = fun() ->
        mnesia:foldl(
            fun(#instructor_request{status = pending} = Req, Acc) ->
                Map = #{
                    user_id => Req#instructor_request.user_id,
                    name => Req#instructor_request.name,
                    family => Req#instructor_request.family,
                    reason => Req#instructor_request.reason,
                    requested_at => Req#instructor_request.requested_at,
                    status => Req#instructor_request.status
                },
                [Map | Acc];
               (_, Acc) -> Acc
            end,
            [],
            instructor_request
        )
    end,

    case mnesia:transaction(F) of
        {atomic, List} ->
            io:format("   Found ~p pending requests~n", [length(List)]),
            List;
        {aborted, _} -> []
    end.

approve_request(AdminUsername, UserId) ->
    io:format("~n"),
    io:format("================================================================================~n"),
    io:format("✅ [instructordb] approve_request called~n"),
    io:format("================================================================================~n"),
    io:format("   Admin Username: ~p~n", [AdminUsername]),
    io:format("   User ID (raw): ~p~n", [UserId]),

    NormalizedId = normalize_user_id(UserId),
    io:format("   Normalized ID: ~p~n", [NormalizedId]),

    case admindb:is_admin(AdminUsername) of
        false ->
            io:format("🔴 Admin check FAILED~n"),
            {error, unauthorized};
        true ->
            io:format("✅ Admin check PASSED~n"),

            case verify_instructor(AdminUsername, NormalizedId) of
                ok ->
                    io:format("~n✅✅✅ APPROVAL SUCCESSFUL ✅✅✅~n"),
                    io:format("================================================================================~n~n"),
                    ok;
                Error ->
                    io:format("~n🔴 Verification failed: ~p~n", [Error]),
                    io:format("================================================================================~n~n"),
                    Error
            end
    end.

reject_request(AdminUsername, UserId, Reason) ->
    io:format("~n"),
    io:format("================================================================================~n"),
    io:format("❌ [instructordb] reject_request called~n"),
    io:format("================================================================================~n"),

    NormalizedId = normalize_user_id(UserId),

    case admindb:is_admin(AdminUsername) of
        false ->
            {error, unauthorized};
        true ->
            F = fun() ->
                case mnesia:read({instructor_request, NormalizedId}) of
                    [Req] ->
                        UpdatedReq = Req#instructor_request{
                            status = rejected,
                            rejection_reason = Reason
                        },
                        mnesia:write(UpdatedReq),
                        ok;
                    [] ->
                        {error, not_found}
                end
            end,

            case mnesia:transaction(F) of
                {atomic, ok} ->
                    io:format("✅✅✅ REJECTION SUCCESSFUL ✅✅✅~n"),
                    io:format("================================================================================~n~n"),
                    ok;
                {atomic, Error} -> Error;
                {aborted, Reason2} -> {error, Reason2}
            end
    end.

remove_pending_request(UserId) ->
    io:format("🔵 [instructordb] remove_pending_request called~n"),

    NormalizedId = normalize_user_id(UserId),

    F = fun() ->
        mnesia:delete({instructor_request, NormalizedId})
    end,

    case mnesia:transaction(F) of
        {atomic, ok} ->
            io:format("✅ Request removed~n"),
            ok;
        {aborted, _} -> ok
    end.

get_request(UserId) ->
    io:format("🔍 [instructordb] get_request called~n"),

    NormalizedId = normalize_user_id(UserId),

    F = fun() ->
        mnesia:read({instructor_request, NormalizedId})
    end,

    case mnesia:transaction(F) of
        {atomic, [Req]} ->
            Map = #{
                user_id => Req#instructor_request.user_id,
                name => Req#instructor_request.name,
                family => Req#instructor_request.family,
                reason => Req#instructor_request.reason,
                requested_at => Req#instructor_request.requested_at,
                status => Req#instructor_request.status,
                rejection_reason => Req#instructor_request.rejection_reason
            },
            io:format("   ✅ Found request~n"),
            {ok, Map};
        {atomic, []} ->
            io:format("   ❌ No request found~n"),
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_instructor_info(UserId) ->
    io:format("🔍 [instructordb] get_instructor_info called~n"),

    NormalizedId = normalize_user_id(UserId),
    IsVerified = is_verified_instructor(NormalizedId),

    PendingRequest = case get_request(NormalizedId) of
        {ok, Req} -> Req;
        _ -> none
    end,

    #{
        verified => IsVerified,
        pending_request => PendingRequest
    }.
