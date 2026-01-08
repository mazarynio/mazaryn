-module(businessdb).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../business.hrl").
-include("../job_records.hrl").

-export([
    create_business/2,
    create_business_concurrent/2,
    get_business_by_id/1,
    get_business_by_user_id/1,
    get_businesses_by_user_id/1,
    update_business/2,
    delete_business/1,
    verify_business/1,
    unverify_business/1,
    set_verification_level/2,

    add_team_member/3,
    remove_team_member/2,
    update_team_member_role/3,
    get_team_members/1,
    add_pending_invitation/3,
    remove_pending_invitation/2,
    accept_invitation/2,

    upload_logo/2,
    upload_banner/2,
    upload_gallery_image/2,
    remove_gallery_image/2,
    upload_video_intro/2,
    upload_document/3,
    remove_document/2,

    follow_business/2,
    unfollow_business/2,
    get_business_followers/1,
    get_business_following/1,
    is_following_business/2,

    add_post/2,
    remove_post/2,
    get_business_posts/1,
    increment_posts_count/1,

    update_subscription/2,
    get_subscription_info/1,
    check_feature_access/2,
    add_subscription_credits/3,
    deduct_subscription_credits/3,

    add_job_posting/2,
    remove_job_posting/2,
    get_business_job_postings/1,
    increment_job_count/1,
    decrement_job_count/1,

    add_ad_campaign/2,
    remove_ad_campaign/2,
    get_business_ads/1,
    update_ad_budget/2,
    track_ad_impression/1,
    track_ad_click/1,
    track_ad_conversion/1,

    update_analytics/2,
    get_analytics/1,
    record_page_view/2,
    record_profile_visitor/2,

    add_certification/2,
    remove_certification/2,
    add_award/2,
    remove_award/2,
    add_license/2,
    remove_license/2,

    add_partnership/2,
    remove_partnership/2,
    add_affiliate/2,
    remove_affiliate/2,

    add_review/2,
    get_business_reviews/1,
    calculate_average_rating/1,

    update_location/2,
    add_additional_location/2,
    remove_additional_location/2,

    search_businesses/1,
    search_businesses_by_industry/1,
    search_businesses_by_location/2,
    search_businesses_pattern/1,
    filter_businesses/1,

    get_all_businesses/0,
    get_verified_businesses/0,
    get_featured_businesses/0,
    get_businesses_by_tier/1
]).

-define(MAX_RETRIES, 10).
-define(BACKOFF_TIME, 20).
-define(LIMIT_SEARCH, 50).

create_business(UserID, BusinessData) ->
    Fun = fun() ->
        case check_user_exists(UserID) of
            false -> {error, user_not_found};
            true ->
                case validate_business_data(BusinessData) of
                    {error, Reason} -> {error, Reason};
                    ok ->
                        Now = calendar:universal_time(),
                        BusinessID = nanoid:gen(),

                        Business = #business{
                            id = BusinessID,
                            user_id = UserID,
                            company_name = maps:get(company_name, BusinessData),
                            legal_name = maps:get(legal_name, BusinessData, maps:get(company_name, BusinessData)),
                            industry = maps:get(industry, BusinessData, ""),
                            sub_industry = maps:get(sub_industry, BusinessData, ""),
                            company_size = maps:get(company_size, BusinessData, "1-10"),
                            website = maps:get(website, BusinessData, ""),
                            business_email = maps:get(business_email, BusinessData),
                            business_phone = maps:get(business_phone, BusinessData, ""),
                            business_type = maps:get(business_type, BusinessData, "B2C"),
                            founding_date = maps:get(founding_date, BusinessData, undefined),
                            registration_date = Now,
                            business_description = maps:get(business_description, BusinessData, ""),
                            location = maps:get(location, BusinessData, #{
                                address => "",
                                city => "",
                                state => "",
                                country => "",
                                postal_code => "",
                                geo_coordinates => {0.0, 0.0},
                                timezone => ""
                            }),
                            subscription_tier = "free",
                            subscription_start_date = Now,
                            account_status = "pending_verification",
                            date_created = Now,
                            date_updated = Now,
                            last_active = Now
                        },

                        mnesia:write(Business),

                        case mnesia:read(user, UserID) of
                            [User] ->
                                UpdatedBusinessIDs = [BusinessID | User#user.business_id],
                                mnesia:write(User#user{
                                    business_id = UpdatedBusinessIDs,
                                    date_updated = Now
                                });
                            [] -> ok
                        end,

                        {ok, BusinessID}
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, BusinessID}} -> {ok, BusinessID};
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

create_business_concurrent(UserID, BusinessData) ->
    case check_user_exists(UserID) of
        false -> {error, user_not_found};
        true ->
            case validate_business_data(BusinessData) of
                {error, Reason} -> {error, Reason};
                ok ->
                    BusinessID = nanoid:gen(),
                    Now = calendar:universal_time(),

                    Business = #business{
                        id = BusinessID,
                        user_id = UserID,
                        company_name = maps:get(company_name, BusinessData),
                        legal_name = maps:get(legal_name, BusinessData, maps:get(company_name, BusinessData)),
                        industry = maps:get(industry, BusinessData, ""),
                        sub_industry = maps:get(sub_industry, BusinessData, ""),
                        company_size = maps:get(company_size, BusinessData, "1-10"),
                        website = maps:get(website, BusinessData, ""),
                        business_email = maps:get(business_email, BusinessData),
                        business_phone = maps:get(business_phone, BusinessData, ""),
                        business_type = maps:get(business_type, BusinessData, "B2C"),
                        founding_date = maps:get(founding_date, BusinessData, undefined),
                        registration_date = Now,
                        business_description = maps:get(business_description, BusinessData, ""),
                        location = maps:get(location, BusinessData, #{
                            address => "",
                            city => "",
                            state => "",
                            country => "",
                            postal_code => "",
                            geo_coordinates => {0.0, 0.0},
                            timezone => ""
                        }),
                        subscription_tier = "free",
                        subscription_start_date = Now,
                        account_status = "pending_verification",
                        date_created = Now,
                        date_updated = Now,
                        last_active = Now
                    },

                    case write_business_with_retry(Business, UserID, ?MAX_RETRIES) of
                        ok -> {ok, BusinessID};
                        {error, Reason} -> {error, Reason}
                    end
            end
    end.

write_business_with_retry(Business, UserID, RetriesLeft) when RetriesLeft > 0 ->
    try
        case mnesia:transaction(fun() ->
            mnesia:write(Business),
            case mnesia:read(user, UserID) of
                [User] ->
                    UpdatedBusinessIDs = [Business#business.id | User#user.business_id],
                    mnesia:write(User#user{
                        business_id = UpdatedBusinessIDs,
                        date_updated = calendar:universal_time()
                    });
                [] -> ok
            end
        end) of
            {atomic, ok} -> ok;
            {aborted, Reason} ->
                error_logger:error_msg("Business write transaction ABORTED - retries left: ~p~nReason: ~p~n",
                                       [RetriesLeft, Reason]),
                timer:sleep(?BACKOFF_TIME * ((?MAX_RETRIES - RetriesLeft) + 1)),
                write_business_with_retry(Business, UserID, RetriesLeft - 1)
        end
    catch
        error:ErrReason:Stacktrace ->
            error_logger:error_msg("Business write FAILED (error) - retries left: ~p~nReason: ~p~nStacktrace: ~p~n",
                                   [RetriesLeft, ErrReason, Stacktrace]),
            timer:sleep(?BACKOFF_TIME * ((?MAX_RETRIES - RetriesLeft) + 1)),
            write_business_with_retry(Business, UserID, RetriesLeft - 1);
        throw:ThrowReason:Stacktrace ->
            error_logger:error_msg("Business write FAILED (throw) - retries left: ~p~nReason: ~p~nStacktrace: ~p~n",
                                   [RetriesLeft, ThrowReason, Stacktrace]),
            timer:sleep(?BACKOFF_TIME * ((?MAX_RETRIES - RetriesLeft) + 1)),
            write_business_with_retry(Business, UserID, RetriesLeft - 1);
        exit:ExitReason:Stacktrace ->
            error_logger:error_msg("Business write FAILED (exit) - retries left: ~p~nReason: ~p~nStacktrace: ~p~n",
                                   [RetriesLeft, ExitReason, Stacktrace]),
            {error, {process_died, ExitReason}}
    end;
write_business_with_retry(_Business, _UserID, 0) ->
    error_logger:error_msg("Business write FAILED - MAX RETRIES EXCEEDED~n", []),
    {error, max_retries_exceeded}.

get_business_by_id(BusinessID) ->
    Fun = fun() ->
        case mnesia:read(business, BusinessID) of
            [] -> {error, business_not_found};
            [Business] -> {ok, Business}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, Business}} -> Business;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_business_by_user_id(UserID) ->
    Fun = fun() ->
        mnesia:match_object(#business{user_id = UserID, _ = '_'})
    end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> {error, no_businesses_found};
        {atomic, [Business | _]} -> Business;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_businesses_by_user_id(UserID) ->
    Fun = fun() ->
        mnesia:match_object(#business{user_id = UserID, _ = '_'})
    end,
    case mnesia:transaction(Fun) of
        {atomic, Businesses} -> Businesses;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_business(BusinessID, UpdateData) ->
    Fun = fun() ->
        case mnesia:read(business, BusinessID) of
            [] -> {error, business_not_found};
            [Business] ->
                UpdatedBusiness = apply_business_updates(Business, UpdateData),
                mnesia:write(UpdatedBusiness#business{
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

apply_business_updates(Business, UpdateData) ->
    maps:fold(fun(Key, Value, Acc) ->
        case Key of
            company_name -> Acc#business{company_name = Value};
            legal_name -> Acc#business{legal_name = Value};
            industry -> Acc#business{industry = Value};
            sub_industry -> Acc#business{sub_industry = Value};
            company_size -> Acc#business{company_size = Value};
            website -> Acc#business{website = Value};
            business_email -> Acc#business{business_email = Value};
            business_phone -> Acc#business{business_phone = Value};
            business_type -> Acc#business{business_type = Value};
            business_description -> Acc#business{business_description = Value};
            long_description -> Acc#business{long_description = Value};
            mission_statement -> Acc#business{mission_statement = Value};
            vision_statement -> Acc#business{vision_statement = Value};
            employee_count -> Acc#business{employee_count = Value};
            annual_revenue -> Acc#business{annual_revenue = Value};
            year_established -> Acc#business{year_established = Value};
            growth_stage -> Acc#business{growth_stage = Value};
            funding_stage -> Acc#business{funding_stage = Value};
            value_proposition -> Acc#business{value_proposition = Value};
            work_environment_type -> Acc#business{work_environment_type = Value};
            company_culture_description -> Acc#business{company_culture_description = Value};
            _ -> Acc
        end
    end, Business, UpdateData).

delete_business(BusinessID) ->
    Fun = fun() ->
        case mnesia:read(business, BusinessID) of
            [] -> {error, business_not_found};
            [Business] ->
                UserID = Business#business.user_id,
                case mnesia:read(user, UserID) of
                    [User] ->
                        UpdatedBusinessIDs = lists:delete(BusinessID, User#user.business_id),
                        mnesia:write(User#user{
                            business_id = UpdatedBusinessIDs,
                            date_updated = calendar:universal_time()
                        });
                    [] -> ok
                end,
                mnesia:delete({business, BusinessID}),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

verify_business(BusinessID) ->
    Fun = fun() ->
        case mnesia:read(business, BusinessID) of
            [] -> {error, business_not_found};
            [Business] ->
                mnesia:write(Business#business{
                    verification_status = true,
                    verification_level = "basic",
                    account_status = "active",
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

unverify_business(BusinessID) ->
    Fun = fun() ->
        case mnesia:read(business, BusinessID) of
            [] -> {error, business_not_found};
            [Business] ->
                mnesia:write(Business#business{
                    verification_status = false,
                    verification_level = "none",
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

set_verification_level(BusinessID, Level) ->
    ValidLevels = ["none", "basic", "advanced", "premium"],
    case lists:member(Level, ValidLevels) of
        false -> {error, invalid_verification_level};
        true ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        mnesia:write(Business#business{
                            verification_level = Level,
                            verification_status = Level =/= "none",
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end
    end.

add_team_member(BusinessID, UserID, Role) ->
    Fun = fun() ->
        case mnesia:read(business, BusinessID) of
            [] -> {error, business_not_found};
            [Business] ->
                TeamMember = {UserID, Role, []},
                CurrentTeam = Business#business.team_members,
                case lists:keymember(UserID, 1, CurrentTeam) of
                    true -> {error, member_already_exists};
                    false ->
                        UpdatedTeam = [TeamMember | CurrentTeam],
                        mnesia:write(Business#business{
                            team_members = UpdatedTeam,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

remove_team_member(BusinessID, UserID) ->
    Fun = fun() ->
        case mnesia:read(business, BusinessID) of
            [] -> {error, business_not_found};
            [Business] ->
                CurrentTeam = Business#business.team_members,
                UpdatedTeam = lists:keydelete(UserID, 1, CurrentTeam),
                mnesia:write(Business#business{
                    team_members = UpdatedTeam,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_team_member_role(BusinessID, UserID, NewRole) ->
    Fun = fun() ->
        case mnesia:read(business, BusinessID) of
            [] -> {error, business_not_found};
            [Business] ->
                CurrentTeam = Business#business.team_members,
                case lists:keyfind(UserID, 1, CurrentTeam) of
                    false -> {error, member_not_found};
                    {UserID, _OldRole, Permissions} ->
                        UpdatedTeam = lists:keyreplace(UserID, 1, CurrentTeam, {UserID, NewRole, Permissions}),
                        mnesia:write(Business#business{
                            team_members = UpdatedTeam,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_team_members(BusinessID) ->
    Fun = fun() ->
        case mnesia:read(business, BusinessID) of
            [] -> {error, business_not_found};
            [Business] -> {ok, Business#business.team_members}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, Members}} -> Members;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_pending_invitation(BusinessID, UserID, Role) ->
    Fun = fun() ->
        case mnesia:read(business, BusinessID) of
            [] -> {error, business_not_found};
            [Business] ->
                Invitation = {UserID, Role, calendar:universal_time()},
                CurrentInvites = Business#business.pending_invitations,
                UpdatedInvites = [Invitation | CurrentInvites],
                mnesia:write(Business#business{
                    pending_invitations = UpdatedInvites,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

remove_pending_invitation(BusinessID, UserID) ->
    Fun = fun() ->
        case mnesia:read(business, BusinessID) of
            [] -> {error, business_not_found};
            [Business] ->
                CurrentInvites = Business#business.pending_invitations,
                UpdatedInvites = lists:filter(fun({UID, _, _}) -> UID =/= UserID end, CurrentInvites),
                mnesia:write(Business#business{
                    pending_invitations = UpdatedInvites,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

accept_invitation(BusinessID, UserID) ->
    Fun = fun() ->
        case mnesia:read(business, BusinessID) of
            [] -> {error, business_not_found};
            [Business] ->
                CurrentInvites = Business#business.pending_invitations,
                case lists:keyfind(UserID, 1, CurrentInvites) of
                    false -> {error, invitation_not_found};
                    {UserID, Role, _Timestamp} ->
                        UpdatedInvites = lists:keydelete(UserID, 1, CurrentInvites),
                        TeamMember = {UserID, Role, []},
                        CurrentTeam = Business#business.team_members,
                        UpdatedTeam = [TeamMember | CurrentTeam],
                        mnesia:write(Business#business{
                            team_members = UpdatedTeam,
                            pending_invitations = UpdatedInvites,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

check_user_exists(UserID) ->
    case mnesia:dirty_read(user, UserID) of
        [] -> false;
        [_User] -> true
    end.

validate_business_data(BusinessData) ->
    case maps:is_key(company_name, BusinessData) of
        false -> {error, missing_company_name};
        true ->
            case maps:is_key(business_email, BusinessData) of
                false -> {error, missing_business_email};
                true -> ok
            end
    end.

    upload_logo(BusinessID, IPFSHash) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    mnesia:write(Business#business{
                        logo_ipfs_hash = IPFSHash,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    upload_banner(BusinessID, IPFSHash) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    mnesia:write(Business#business{
                        banner_ipfs_hash = IPFSHash,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    upload_gallery_image(BusinessID, IPFSHash) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentGallery = Business#business.gallery_ipfs_hashes,
                    UpdatedGallery = [IPFSHash | CurrentGallery],
                    mnesia:write(Business#business{
                        gallery_ipfs_hashes = UpdatedGallery,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    remove_gallery_image(BusinessID, IPFSHash) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentGallery = Business#business.gallery_ipfs_hashes,
                    UpdatedGallery = lists:delete(IPFSHash, CurrentGallery),
                    mnesia:write(Business#business{
                        gallery_ipfs_hashes = UpdatedGallery,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    upload_video_intro(BusinessID, IPFSHash) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    mnesia:write(Business#business{
                        video_intro_ipfs_hash = IPFSHash,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    upload_document(BusinessID, DocumentType, IPFSHash) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentDocs = Business#business.documents_ipfs_hashes,
                    UpdatedDocs = maps:put(DocumentType, IPFSHash, CurrentDocs),
                    mnesia:write(Business#business{
                        documents_ipfs_hashes = UpdatedDocs,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    remove_document(BusinessID, DocumentType) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentDocs = Business#business.documents_ipfs_hashes,
                    UpdatedDocs = maps:remove(DocumentType, CurrentDocs),
                    mnesia:write(Business#business{
                        documents_ipfs_hashes = UpdatedDocs,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    follow_business(UserID, BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentFollowers = Business#business.followers,
                    case lists:member(UserID, CurrentFollowers) of
                        true -> {error, already_following};
                        false ->
                            UpdatedFollowers = [UserID | CurrentFollowers],
                            mnesia:write(Business#business{
                                followers = UpdatedFollowers,
                                followers_count = Business#business.followers_count + 1,
                                date_updated = calendar:universal_time()
                            }),
                            ok
                    end
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    unfollow_business(UserID, BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentFollowers = Business#business.followers,
                    UpdatedFollowers = lists:delete(UserID, CurrentFollowers),
                    mnesia:write(Business#business{
                        followers = UpdatedFollowers,
                        followers_count = max(0, Business#business.followers_count - 1),
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_business_followers(BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] -> {ok, Business#business.followers}
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, {ok, Followers}} -> Followers;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_business_following(BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] -> {ok, Business#business.following}
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, {ok, Following}} -> Following;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    is_following_business(UserID, BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> false;
                [Business] ->
                    lists:member(UserID, Business#business.followers)
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, _Reason} -> false
        end.

    add_post(BusinessID, PostID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentPosts = Business#business.posts,
                    UpdatedPosts = [PostID | CurrentPosts],
                    mnesia:write(Business#business{
                        posts = UpdatedPosts,
                        posts_count = Business#business.posts_count + 1,
                        date_updated = calendar:universal_time(),
                        last_active = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    remove_post(BusinessID, PostID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentPosts = Business#business.posts,
                    UpdatedPosts = lists:delete(PostID, CurrentPosts),
                    mnesia:write(Business#business{
                        posts = UpdatedPosts,
                        posts_count = max(0, Business#business.posts_count - 1),
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_business_posts(BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] -> {ok, Business#business.posts}
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, {ok, Posts}} -> Posts;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    increment_posts_count(BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    mnesia:write(Business#business{
                        posts_count = Business#business.posts_count + 1,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    update_subscription(BusinessID, SubscriptionData) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    Tier = maps:get(tier, SubscriptionData, Business#business.subscription_tier),
                    StartDate = maps:get(start_date, SubscriptionData, Business#business.subscription_start_date),
                    EndDate = maps:get(end_date, SubscriptionData, Business#business.subscription_end_date),
                    AutoRenew = maps:get(auto_renew, SubscriptionData, Business#business.subscription_auto_renew),
                    BillingCycle = maps:get(billing_cycle, SubscriptionData, Business#business.billing_cycle),

                    UpdatedBusiness = Business#business{
                        subscription_tier = Tier,
                        subscription_start_date = StartDate,
                        subscription_end_date = EndDate,
                        subscription_auto_renew = AutoRenew,
                        billing_cycle = BillingCycle,
                        date_updated = calendar:universal_time()
                    },

                    FinalBusiness = update_tier_limits(UpdatedBusiness, Tier),
                    mnesia:write(FinalBusiness),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    update_tier_limits(Business, "free") ->
        Business#business{
            max_active_job_postings = 2,
            posts_limit_monthly = 10,
            max_team_members = 1
        };
    update_tier_limits(Business, "basic") ->
        Business#business{
            max_active_job_postings = 5,
            posts_limit_monthly = 50,
            max_team_members = 5
        };
    update_tier_limits(Business, "professional") ->
        Business#business{
            max_active_job_postings = 999,
            posts_limit_monthly = 999,
            max_team_members = 20
        };
    update_tier_limits(Business, "enterprise") ->
        Business#business{
            max_active_job_postings = 9999,
            posts_limit_monthly = 9999,
            max_team_members = 100
        };
    update_tier_limits(Business, _) ->
        Business.

    get_subscription_info(BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    {ok, #{
                        tier => Business#business.subscription_tier,
                        start_date => Business#business.subscription_start_date,
                        end_date => Business#business.subscription_end_date,
                        auto_renew => Business#business.subscription_auto_renew,
                        billing_cycle => Business#business.billing_cycle,
                        payment_info => Business#business.payment_info
                    }}
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, {ok, Info}} -> Info;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    check_feature_access(BusinessID, FeatureName) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    PremiumFeatures = Business#business.premium_features,
                    case maps:get(FeatureName, PremiumFeatures, false) of
                        true -> {ok, granted};
                        false -> {ok, denied}
                    end
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, {ok, Status}} -> Status;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    add_subscription_credits(BusinessID, CreditType, Amount) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    UpdatedBusiness = case CreditType of
                        job_posting ->
                            Business#business{
                                job_posting_credits = Business#business.job_posting_credits + Amount
                            };
                        featured_job ->
                            Business#business{
                                featured_job_slots = Business#business.featured_job_slots + Amount
                            };
                        ads_budget ->
                            Business#business{
                                ads_budget_remaining = Business#business.ads_budget_remaining + Amount
                            };
                        referral ->
                            Business#business{
                                referral_credits = Business#business.referral_credits + Amount
                            };
                        loyalty ->
                            Business#business{
                                loyalty_points = Business#business.loyalty_points + Amount
                            };
                        _ ->
                            Business
                    end,
                    mnesia:write(UpdatedBusiness#business{
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    deduct_subscription_credits(BusinessID, CreditType, Amount) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    UpdatedBusiness = case CreditType of
                        job_posting ->
                            CurrentCredits = Business#business.job_posting_credits,
                            if
                                CurrentCredits >= Amount ->
                                    Business#business{
                                        job_posting_credits = CurrentCredits - Amount
                                    };
                                true ->
                                    Business
                            end;
                        featured_job ->
                            CurrentSlots = Business#business.featured_job_slots,
                            if
                                CurrentSlots >= Amount ->
                                    Business#business{
                                        featured_job_slots = CurrentSlots - Amount
                                    };
                                true ->
                                    Business
                            end;
                        ads_budget ->
                            CurrentBudget = Business#business.ads_budget_remaining,
                            if
                                CurrentBudget >= Amount ->
                                    Business#business{
                                        ads_budget_remaining = CurrentBudget - Amount
                                    };
                                true ->
                                    Business
                            end;
                        _ ->
                            Business
                    end,
                    case UpdatedBusiness =:= Business of
                        true -> {error, insufficient_credits};
                        false ->
                            mnesia:write(UpdatedBusiness#business{
                                date_updated = calendar:universal_time()
                            }),
                            ok
                    end
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    add_job_posting(BusinessID, JobPostingID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentJobs = Business#business.job_postings,
                    UpdatedJobs = [JobPostingID | CurrentJobs],
                    mnesia:write(Business#business{
                        job_postings = UpdatedJobs,
                        active_job_postings_count = Business#business.active_job_postings_count + 1,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    remove_job_posting(BusinessID, JobPostingID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentJobs = Business#business.job_postings,
                    UpdatedJobs = lists:delete(JobPostingID, CurrentJobs),
                    mnesia:write(Business#business{
                        job_postings = UpdatedJobs,
                        active_job_postings_count = max(0, Business#business.active_job_postings_count - 1),
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_business_job_postings(BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] -> {ok, Business#business.job_postings}
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, {ok, Jobs}} -> Jobs;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    increment_job_count(BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    mnesia:write(Business#business{
                        active_job_postings_count = Business#business.active_job_postings_count + 1,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    decrement_job_count(BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    mnesia:write(Business#business{
                        active_job_postings_count = max(0, Business#business.active_job_postings_count - 1),
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    add_ad_campaign(BusinessID, AdCampaignID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentAds = Business#business.ads_campaigns,
                    UpdatedAds = [AdCampaignID | CurrentAds],
                    mnesia:write(Business#business{
                        ads_campaigns = UpdatedAds,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    remove_ad_campaign(BusinessID, AdCampaignID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentAds = Business#business.ads_campaigns,
                    UpdatedAds = lists:delete(AdCampaignID, CurrentAds),
                    mnesia:write(Business#business{
                        ads_campaigns = UpdatedAds,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_business_ads(BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] -> {ok, Business#business.ads_campaigns}
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, {ok, Ads}} -> Ads;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    update_ad_budget(BusinessID, NewBudget) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    mnesia:write(Business#business{
                        ads_budget_remaining = NewBudget,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    track_ad_impression(BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    mnesia:write(Business#business{
                        ad_impressions = Business#business.ad_impressions + 1,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    track_ad_click(BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    mnesia:write(Business#business{
                        ad_clicks = Business#business.ad_clicks + 1,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    track_ad_conversion(BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    mnesia:write(Business#business{
                        ad_conversions = Business#business.ad_conversions + 1,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    update_analytics(BusinessID, AnalyticsData) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentAnalytics = Business#business.analytics_data,
                    UpdatedAnalytics = maps:merge(CurrentAnalytics, AnalyticsData),
                    mnesia:write(Business#business{
                        analytics_data = UpdatedAnalytics,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_analytics(BusinessID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] -> {ok, Business#business.analytics_data}
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, {ok, Analytics}} -> Analytics;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    record_page_view(BusinessID, VisitorData) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentAnalytics = Business#business.analytics_data,
                    PageViews = maps:get(page_views, CurrentAnalytics, []),
                    UpdatedPageViews = [VisitorData | PageViews],
                    UpdatedAnalytics = maps:put(page_views, UpdatedPageViews, CurrentAnalytics),
                    mnesia:write(Business#business{
                        analytics_data = UpdatedAnalytics,
                        total_impressions = Business#business.total_impressions + 1,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    record_profile_visitor(BusinessID, UserID) ->
        Fun = fun() ->
            case mnesia:read(business, BusinessID) of
                [] -> {error, business_not_found};
                [Business] ->
                    CurrentAnalytics = Business#business.analytics_data,
                    Visitors = maps:get(profile_visitors, CurrentAnalytics, []),
                    case lists:member(UserID, Visitors) of
                        true -> ok;
                        false ->
                            UpdatedVisitors = [UserID | Visitors],
                            UpdatedAnalytics = maps:put(profile_visitors, UpdatedVisitors, CurrentAnalytics),
                            mnesia:write(Business#business{
                                analytics_data = UpdatedAnalytics,
                                date_updated = calendar:universal_time()
                            }),
                            ok
                    end
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, ok} -> ok;
            {atomic, {error, Reason}} -> {error, Reason};
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

        add_certification(BusinessID, Certification) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        CurrentCerts = Business#business.certifications,
                        UpdatedCerts = [Certification | CurrentCerts],
                        mnesia:write(Business#business{
                            certifications = UpdatedCerts,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        remove_certification(BusinessID, CertificationName) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        CurrentCerts = Business#business.certifications,
                        UpdatedCerts = lists:filter(fun(Cert) ->
                            case Cert of
                                {Name, _, _, _} -> Name =/= CertificationName;
                                _ -> true
                            end
                        end, CurrentCerts),
                        mnesia:write(Business#business{
                            certifications = UpdatedCerts,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        add_award(BusinessID, Award) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        CurrentAwards = Business#business.awards,
                        UpdatedAwards = [Award | CurrentAwards],
                        mnesia:write(Business#business{
                            awards = UpdatedAwards,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        remove_award(BusinessID, AwardTitle) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        CurrentAwards = Business#business.awards,
                        UpdatedAwards = lists:filter(fun(Award) ->
                            case Award of
                                {Title, _, _, _} -> Title =/= AwardTitle;
                                _ -> true
                            end
                        end, CurrentAwards),
                        mnesia:write(Business#business{
                            awards = UpdatedAwards,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        add_license(BusinessID, License) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        CurrentLicenses = Business#business.licenses,
                        UpdatedLicenses = [License | CurrentLicenses],
                        mnesia:write(Business#business{
                            licenses = UpdatedLicenses,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        remove_license(BusinessID, LicenseID) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        CurrentLicenses = Business#business.licenses,
                        UpdatedLicenses = lists:filter(fun(License) ->
                            License =/= LicenseID
                        end, CurrentLicenses),
                        mnesia:write(Business#business{
                            licenses = UpdatedLicenses,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        add_partnership(BusinessID, PartnerBusinessID) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        CurrentPartnerships = Business#business.partnerships,
                        case lists:member(PartnerBusinessID, CurrentPartnerships) of
                            true -> {error, partnership_already_exists};
                            false ->
                                UpdatedPartnerships = [PartnerBusinessID | CurrentPartnerships],
                                mnesia:write(Business#business{
                                    partnerships = UpdatedPartnerships,
                                    date_updated = calendar:universal_time()
                                }),
                                ok
                        end
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        remove_partnership(BusinessID, PartnerBusinessID) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        CurrentPartnerships = Business#business.partnerships,
                        UpdatedPartnerships = lists:delete(PartnerBusinessID, CurrentPartnerships),
                        mnesia:write(Business#business{
                            partnerships = UpdatedPartnerships,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        add_affiliate(BusinessID, AffiliateBusinessID) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        CurrentAffiliates = Business#business.affiliates,
                        case lists:member(AffiliateBusinessID, CurrentAffiliates) of
                            true -> {error, affiliate_already_exists};
                            false ->
                                UpdatedAffiliates = [AffiliateBusinessID | CurrentAffiliates],
                                mnesia:write(Business#business{
                                    affiliates = UpdatedAffiliates,
                                    date_updated = calendar:universal_time()
                                }),
                                ok
                        end
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        remove_affiliate(BusinessID, AffiliateBusinessID) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        CurrentAffiliates = Business#business.affiliates,
                        UpdatedAffiliates = lists:delete(AffiliateBusinessID, CurrentAffiliates),
                        mnesia:write(Business#business{
                            affiliates = UpdatedAffiliates,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        add_review(BusinessID, ReviewID) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        CurrentReviews = Business#business.reviews,
                        UpdatedReviews = [ReviewID | CurrentReviews],
                        mnesia:write(Business#business{
                            reviews = UpdatedReviews,
                            total_reviews_count = Business#business.total_reviews_count + 1,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_business_reviews(BusinessID) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] -> {ok, Business#business.reviews}
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, {ok, Reviews}} -> Reviews;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        calculate_average_rating(BusinessID) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        ReviewIDs = Business#business.reviews,
                        case ReviewIDs of
                            [] -> {ok, 0.0};
                            _ ->
                                Ratings = lists:foldl(fun(ReviewID, Acc) ->
                                    case mnesia:read(company_review, ReviewID) of
                                        [Review] -> [Review#company_review.overall_rating | Acc];
                                        [] -> Acc
                                    end
                                end, [], ReviewIDs),
                                case Ratings of
                                    [] -> {ok, 0.0};
                                    _ ->
                                        Average = lists:sum(Ratings) / length(Ratings),
                                        mnesia:write(Business#business{
                                            average_rating = Average,
                                            date_updated = calendar:universal_time()
                                        }),
                                        {ok, Average}
                                end
                        end
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, {ok, Rating}} -> Rating;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        update_location(BusinessID, LocationData) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        mnesia:write(Business#business{
                            location = LocationData,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        add_additional_location(BusinessID, LocationData) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        CurrentLocations = Business#business.additional_locations,
                        UpdatedLocations = [LocationData | CurrentLocations],
                        mnesia:write(Business#business{
                            additional_locations = UpdatedLocations,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        remove_additional_location(BusinessID, LocationIndex) ->
            Fun = fun() ->
                case mnesia:read(business, BusinessID) of
                    [] -> {error, business_not_found};
                    [Business] ->
                        CurrentLocations = Business#business.additional_locations,
                        case LocationIndex > 0 andalso LocationIndex =< length(CurrentLocations) of
                            false -> {error, invalid_location_index};
                            true ->
                                UpdatedLocations = lists:sublist(CurrentLocations, LocationIndex - 1) ++
                                                  lists:nthtail(LocationIndex, CurrentLocations),
                                mnesia:write(Business#business{
                                    additional_locations = UpdatedLocations,
                                    date_updated = calendar:universal_time()
                                }),
                                ok
                        end
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} -> {error, Reason};
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        search_businesses(CompanyName) ->
            Fun = fun() ->
                mnesia:match_object(#business{company_name = CompanyName, _ = '_'})
            end,
            case mnesia:transaction(Fun) of
                {atomic, []} -> {error, business_not_found};
                {atomic, Businesses} -> Businesses;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        search_businesses_by_industry(Industry) ->
            Fun = fun() ->
                mnesia:match_object(#business{industry = Industry, _ = '_'})
            end,
            case mnesia:transaction(Fun) of
                {atomic, Businesses} -> Businesses;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        search_businesses_by_location(City, Country) ->
            Fun = fun() ->
                AllBusinesses = mnesia:match_object(#business{_ = '_'}),
                lists:filter(fun(Business) ->
                    Location = Business#business.location,
                    LocationCity = maps:get(city, Location, ""),
                    LocationCountry = maps:get(country, Location, ""),
                    (City =:= "" orelse string:lowercase(LocationCity) =:= string:lowercase(City)) andalso
                    (Country =:= "" orelse string:lowercase(LocationCountry) =:= string:lowercase(Country))
                end, AllBusinesses)
            end,
            case mnesia:transaction(Fun) of
                {atomic, Businesses} -> Businesses;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        search_businesses_pattern(Pattern) ->
            Fun = fun() ->
                mnesia:all_keys(business)
            end,
            {atomic, BusinessIDs} = mnesia:transaction(Fun),
            search_businesses_pattern(Pattern, BusinessIDs, ?LIMIT_SEARCH, []).

        search_businesses_pattern(_Pattern, [], _Limit, Acc) -> lists:reverse(Acc);
        search_businesses_pattern(_Pattern, _IDs, Limit, Acc) when Limit == length(Acc) -> lists:reverse(Acc);
        search_businesses_pattern(Pattern, [ID | Rest], Limit, Acc) ->
            case mnesia:dirty_read(business, ID) of
                [] ->
                    search_businesses_pattern(Pattern, Rest, Limit, Acc);
                [Business] ->
                    CompanyName = Business#business.company_name,
                    case re:run(CompanyName, Pattern, [caseless]) of
                        nomatch ->
                            search_businesses_pattern(Pattern, Rest, Limit, Acc);
                        {match, _} ->
                            search_businesses_pattern(Pattern, Rest, Limit, [Business | Acc])
                    end
            end.

        filter_businesses(FilterCriteria) ->
            Fun = fun() ->
                AllBusinesses = mnesia:match_object(#business{_ = '_'}),
                lists:filter(fun(Business) ->
                    apply_filters(Business, FilterCriteria)
                end, AllBusinesses)
            end,
            case mnesia:transaction(Fun) of
                {atomic, Businesses} -> Businesses;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        apply_filters(Business, FilterCriteria) ->
            lists:all(fun({Key, Value}) ->
                case Key of
                    industry ->
                        Business#business.industry =:= Value;
                    subscription_tier ->
                        Business#business.subscription_tier =:= Value;
                    verification_status ->
                        Business#business.verification_status =:= Value;
                    account_status ->
                        Business#business.account_status =:= Value;
                    business_type ->
                        Business#business.business_type =:= Value;
                    company_size ->
                        Business#business.company_size =:= Value;
                    min_followers ->
                        Business#business.followers_count >= Value;
                    verified_only ->
                        Value =:= true andalso Business#business.verification_status =:= true;
                    featured_only ->
                        Value =:= true andalso Business#business.featured_until =/= undefined andalso
                        calendar:universal_time() < Business#business.featured_until;
                    _ ->
                        true
                end
            end, FilterCriteria).

        get_all_businesses() ->
            Fun = fun() ->
                mnesia:match_object(#business{_ = '_'})
            end,
            case mnesia:transaction(Fun) of
                {atomic, Businesses} -> Businesses;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_verified_businesses() ->
            Fun = fun() ->
                mnesia:match_object(#business{verification_status = true, _ = '_'})
            end,
            case mnesia:transaction(Fun) of
                {atomic, Businesses} -> Businesses;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_featured_businesses() ->
            Fun = fun() ->
                Now = calendar:universal_time(),
                AllBusinesses = mnesia:match_object(#business{_ = '_'}),
                lists:filter(fun(Business) ->
                    case Business#business.featured_until of
                        undefined -> false;
                        FeaturedUntil -> FeaturedUntil > Now
                    end
                end, AllBusinesses)
            end,
            case mnesia:transaction(Fun) of
                {atomic, Businesses} -> Businesses;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_businesses_by_tier(Tier) ->
            Fun = fun() ->
                mnesia:match_object(#business{subscription_tier = Tier, _ = '_'})
            end,
            case mnesia:transaction(Fun) of
                {atomic, Businesses} -> Businesses;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.
