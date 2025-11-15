-module(notebookdb).
-author("Zaryn Technologies").

-export([
    create_notebook/10,
    update_notebook/11,
    delete_notebook/2,
    get_notebook_by_id/1,
    get_notebooks_by_creator/1,
    get_public_notebooks/0,
    get_notebooks_by_language/1,
    get_notebooks_by_kernel/1,
    get_notebooks_by_tag/1,

    get_notebook_content/1,
    update_notebook_content/3,
    get_notebook_outputs/1,
    save_notebook_outputs/2,

    add_dataset_to_notebook/2,
    remove_dataset_from_notebook/2,
    get_notebook_datasets/1,

    link_notebook_to_competition/2,
    unlink_notebook_from_competition/2,
    get_competition_notebooks/1,

    add_collaborator/3,
    remove_collaborator/3,
    get_notebook_collaborators/1,
    is_collaborator/2,

    create_version/3,
    get_notebook_versions/1,
    get_version_by_number/2,
    rollback_to_version/3,
    compare_versions/3,

    fork_notebook/2,
    get_notebook_forks/1,
    get_fork_parent/1,
    increment_fork_count/1,

    execute_notebook/2,
    get_execution_logs/1,
    get_execution_stats/1,
    schedule_notebook_run/3,
    cancel_scheduled_run/2,
    get_scheduled_runs/1,

    like_notebook/2,
    unlike_notebook/2,
    get_notebook_likes/1,
    has_user_liked/2,

    add_comment/3,
    get_notebook_comments/1,
    delete_comment/3,
    update_comment/3,

    set_notebook_environment/2,
    get_notebook_environment/1,
    set_dependencies/2,
    get_dependencies/1,

    add_citation/2,
    get_notebook_citations/1,
    remove_citation/2,

    add_interactive_widget/3,
    remove_interactive_widget/2,
    get_notebook_widgets/1,

    set_notebook_type/2,
    get_notebooks_by_type/1,

    search_notebooks/1,
    search_notebooks_advanced/1,
    get_trending_notebooks/1,
    get_featured_notebooks/0,
    get_most_forked_notebooks/1,
    get_most_liked_notebooks/1,

    report_notebook/4,

    get_notebook_stats/1,
    get_user_notebook_stats/1,

    pin_notebook/1,
    unpin_notebook/1,

    submit_notebook_to_competition/3,
    get_submission_from_notebook/1
]).

-include("../ml_records.hrl").
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(MAX_EXECUTION_TIME, 3600).
-define(DEFAULT_MEMORY_LIMIT, 4096).
-define(MAX_OUTPUT_SIZE, 10485760).

create_notebook(CreatorId, Title, Description, Language, KernelType, DatasetCids,
                CompetitionId, Environment, Tags, Visibility) ->
    Fun = fun() ->
        Id = nanoid:gen(),
        Now = calendar:universal_time(),

        Notebook = #notebook{
            id = Id,
            title = Title,
            creator_id = CreatorId,
            description = Description,
            content_cid = undefined,
            dataset_cids = DatasetCids,
            competition_id = CompetitionId,
            submission_id = undefined,
            language = Language,
            kernel_type = KernelType,
            environment = Environment,
            collaborators = [],
            version_cids = [],
            outputs = [],
            visibility = Visibility,
            execution_time_limit = ?MAX_EXECUTION_TIME,
            execution_count = 0,
            likes = [],
            comments = [],
            date_created = Now,
            date_updated = Now,
            report = [],
            data = #{},
            forked_from = undefined,
            fork_count = 0,
            execution_logs_cid = undefined,
            dependencies_cid = undefined,
            compute_time_used = 0,
            gpu_time_used = 0,
            scheduled_runs = [],
            notebook_type = analysis,
            citations = [],
            interactive_widgets = []
        },

        mnesia:write(Notebook),

        lists:foreach(fun(DatasetCid) ->
            case find_dataset_by_cid(DatasetCid) of
                {ok, DatasetId} ->
                    datasetdb:track_notebook_usage(DatasetId, Id);
                _ -> ok
            end
        end, DatasetCids),

        case CompetitionId of
            undefined -> ok;
            _ ->
                case mnesia:read({competition, CompetitionId}) of
                    [_Competition] -> ok;
                    [] -> ok
                end
        end,

        {ok, Id}
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, Id}} -> Id;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

find_dataset_by_cid(Cid) ->
    AllDatasets = mnesia:match_object(#dataset{_ = '_'}),
    case lists:filter(fun(D) -> D#dataset.content_cid =:= Cid end, AllDatasets) of
        [Dataset | _] -> {ok, Dataset#dataset.id};
        [] -> {error, not_found}
    end.

update_notebook(NotebookId, UserId, NewTitle, NewDescription, NewLanguage, NewKernelType,
                NewDatasetCids, NewCompetitionId, NewEnvironment, NewTags, NewVisibility) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] ->
                {error, notebook_not_found};
            [Notebook] ->
                IsOwner = Notebook#notebook.creator_id =:= UserId,
                IsCollaborator = lists:member(UserId, Notebook#notebook.collaborators),
                case IsOwner orelse IsCollaborator of
                    false ->
                        {error, unauthorized};
                    true ->
                        Now = calendar:universal_time(),

                        OldDatasets = Notebook#notebook.dataset_cids,
                        RemovedDatasets = OldDatasets -- NewDatasetCids,
                        AddedDatasets = NewDatasetCids -- OldDatasets,

                        lists:foreach(fun(Cid) ->
                            case find_dataset_by_cid(Cid) of
                                {ok, DatasetId} ->
                                    case mnesia:read({dataset, DatasetId}) of
                                        [Dataset] ->
                                            UpdatedNotebooks = lists:delete(NotebookId,
                                                Dataset#dataset.used_in_notebook_ids),
                                            mnesia:write(Dataset#dataset{
                                                used_in_notebook_ids = UpdatedNotebooks
                                            });
                                        [] -> ok
                                    end;
                                _ -> ok
                            end
                        end, RemovedDatasets),

                        lists:foreach(fun(Cid) ->
                            case find_dataset_by_cid(Cid) of
                                {ok, DatasetId} ->
                                    datasetdb:track_notebook_usage(DatasetId, NotebookId);
                                _ -> ok
                            end
                        end, AddedDatasets),

                        UpdatedData = maps:put(tags, NewTags, Notebook#notebook.data),

                        UpdatedNotebook = Notebook#notebook{
                            title = NewTitle,
                            description = NewDescription,
                            language = NewLanguage,
                            kernel_type = NewKernelType,
                            dataset_cids = NewDatasetCids,
                            competition_id = NewCompetitionId,
                            environment = NewEnvironment,
                            visibility = NewVisibility,
                            date_updated = Now,
                            data = UpdatedData
                        },

                        mnesia:write(UpdatedNotebook),
                        ok
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

delete_notebook(NotebookId, UserId) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] ->
                {error, notebook_not_found};
            [Notebook] ->
                case Notebook#notebook.creator_id of
                    UserId ->
                        lists:foreach(fun(Cid) ->
                            case find_dataset_by_cid(Cid) of
                                {ok, DatasetId} ->
                                    case mnesia:read({dataset, DatasetId}) of
                                        [Dataset] ->
                                            UpdatedNotebooks = lists:delete(NotebookId,
                                                Dataset#dataset.used_in_notebook_ids),
                                            mnesia:write(Dataset#dataset{
                                                used_in_notebook_ids = UpdatedNotebooks
                                            });
                                        [] -> ok
                                    end;
                                _ -> ok
                            end
                        end, Notebook#notebook.dataset_cids),

                        case Notebook#notebook.forked_from of
                            undefined -> ok;
                            ParentId ->
                                case mnesia:read({notebook, ParentId}) of
                                    [Parent] ->
                                        NewForkCount = max(0, Parent#notebook.fork_count - 1),
                                        mnesia:write(Parent#notebook{fork_count = NewForkCount});
                                    [] -> ok
                                end
                        end,

                        mnesia:delete({notebook, NotebookId}),
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

get_notebook_by_id(NotebookId) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] -> {error, notebook_not_found};
            [Notebook] -> Notebook
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_notebooks_by_creator(CreatorId) ->
    Fun = fun() ->
        mnesia:match_object(#notebook{creator_id = CreatorId, _ = '_'})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_public_notebooks() ->
    Fun = fun() ->
        mnesia:match_object(#notebook{visibility = public, _ = '_'})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_notebooks_by_language(Language) ->
    Fun = fun() ->
        mnesia:match_object(#notebook{language = Language, _ = '_'})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_notebooks_by_kernel(KernelType) ->
    Fun = fun() ->
        mnesia:match_object(#notebook{kernel_type = KernelType, _ = '_'})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_notebooks_by_tag(Tag) ->
    Fun = fun() ->
        AllNotebooks = mnesia:match_object(#notebook{_ = '_'}),
        lists:filter(fun(Notebook) ->
            Tags = maps:get(tags, Notebook#notebook.data, []),
            lists:member(Tag, Tags)
        end, AllNotebooks)
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_notebook_content(NotebookId) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] -> {error, notebook_not_found};
            [Notebook] ->
                case Notebook#notebook.content_cid of
                    undefined -> {error, no_content};
                    {pending, _} -> {error, content_not_ready};
                    CID ->
                        try
                            Content = ipfs_content:get_text_content(CID),
                            {ok, Content}
                        catch
                            _:Error -> {error, Error}
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_notebook_content(NotebookId, UserId, NewContent) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] ->
                {error, notebook_not_found};
            [Notebook] ->
                IsOwner = Notebook#notebook.creator_id =:= UserId,
                IsCollaborator = lists:member(UserId, Notebook#notebook.collaborators),
                case IsOwner orelse IsCollaborator of
                    false ->
                        {error, unauthorized};
                    true ->
                        Now = calendar:universal_time(),

                        ContentToCache = if
                            is_binary(NewContent) -> binary_to_list(NewContent);
                            true -> NewContent
                        end,

                        ok = content_cache:set({notebook_content, NotebookId}, ContentToCache),

                        mnesia:write(Notebook#notebook{
                            content_cid = {pending, NotebookId},
                            date_updated = Now
                        }),

                        {ok, NotebookId}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, NotebookId}} ->
            spawn(fun() ->
                upload_notebook_content(NotebookId)
            end),
            ok;
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, {transaction_failed, Reason}}
    end.

upload_notebook_content(NotebookId) ->
    try
        Content = content_cache:get({notebook_content, NotebookId}),
        CID = ipfs_content:upload_text(Content),

        UpdateF = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [Notebook] ->
                    VersionNum = length(Notebook#notebook.version_cids) + 1,
                    UpdatedVersions = [{VersionNum, CID, calendar:universal_time()} |
                                       Notebook#notebook.version_cids],
                    mnesia:write(Notebook#notebook{
                        content_cid = CID,
                        version_cids = UpdatedVersions
                    });
                [] -> ok
            end
        end,
        mnesia:transaction(UpdateF),

        content_cache:delete({notebook_content, NotebookId}),

        ok
    catch
        Exception:Error:Stacktrace ->
            error_logger:error_msg(
                "Exception while uploading notebook content ~p: ~p:~p~n~p",
                [NotebookId, Exception, Error, Stacktrace]
            )
    end.

get_notebook_outputs(NotebookId) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] -> {error, notebook_not_found};
            [Notebook] -> {ok, Notebook#notebook.outputs}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

save_notebook_outputs(NotebookId, Outputs) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] ->
                {error, notebook_not_found};
            [Notebook] ->
                mnesia:write(Notebook#notebook{
                    outputs = Outputs,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_dataset_to_notebook(NotebookId, DatasetCid) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] ->
                {error, notebook_not_found};
            [Notebook] ->
                UpdatedDatasets = case lists:member(DatasetCid, Notebook#notebook.dataset_cids) of
                    true -> Notebook#notebook.dataset_cids;
                    false -> [DatasetCid | Notebook#notebook.dataset_cids]
                end,
                mnesia:write(Notebook#notebook{
                    dataset_cids = UpdatedDatasets,
                    date_updated = calendar:universal_time()
                }),

                case find_dataset_by_cid(DatasetCid) of
                    {ok, DatasetId} ->
                        datasetdb:track_notebook_usage(DatasetId, NotebookId);
                    _ -> ok
                end,

                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

remove_dataset_from_notebook(NotebookId, DatasetCid) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] ->
                {error, notebook_not_found};
            [Notebook] ->
                UpdatedDatasets = lists:delete(DatasetCid, Notebook#notebook.dataset_cids),
                mnesia:write(Notebook#notebook{
                    dataset_cids = UpdatedDatasets,
                    date_updated = calendar:universal_time()
                }),

                case find_dataset_by_cid(DatasetCid) of
                    {ok, DatasetId} ->
                        case mnesia:read({dataset, DatasetId}) of
                            [Dataset] ->
                                UpdatedNotebooks = lists:delete(NotebookId,
                                    Dataset#dataset.used_in_notebook_ids),
                                mnesia:write(Dataset#dataset{
                                    used_in_notebook_ids = UpdatedNotebooks
                                });
                            [] -> ok
                        end;
                    _ -> ok
                end,

                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_notebook_datasets(NotebookId) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] -> {error, notebook_not_found};
            [Notebook] -> {ok, Notebook#notebook.dataset_cids}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

link_notebook_to_competition(NotebookId, CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] ->
                {error, notebook_not_found};
            [Notebook] ->
                case mnesia:read({competition, CompetitionId}) of
                    [] ->
                        {error, competition_not_found};
                    [_Competition] ->
                        mnesia:write(Notebook#notebook{
                            competition_id = CompetitionId,
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

unlink_notebook_from_competition(NotebookId, _CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] ->
                {error, notebook_not_found};
            [Notebook] ->
                mnesia:write(Notebook#notebook{
                    competition_id = undefined,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_competition_notebooks(CompetitionId) ->
    Fun = fun() ->
        mnesia:match_object(#notebook{competition_id = CompetitionId, _ = '_'})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

add_collaborator(NotebookId, OwnerId, CollaboratorId) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] ->
                {error, notebook_not_found};
            [Notebook] ->
                case Notebook#notebook.creator_id of
                    OwnerId ->
                        UpdatedCollaborators = case lists:member(CollaboratorId,
                                                                 Notebook#notebook.collaborators) of
                            true -> Notebook#notebook.collaborators;
                            false -> [CollaboratorId | Notebook#notebook.collaborators]
                        end,
                        mnesia:write(Notebook#notebook{
                            collaborators = UpdatedCollaborators,
                            date_updated = calendar:universal_time()
                        }),
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

remove_collaborator(NotebookId, OwnerId, CollaboratorId) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] ->
                {error, notebook_not_found};
            [Notebook] ->
                case Notebook#notebook.creator_id of
                    OwnerId ->
                        UpdatedCollaborators = lists:delete(CollaboratorId,
                                                           Notebook#notebook.collaborators),
                        mnesia:write(Notebook#notebook{
                            collaborators = UpdatedCollaborators,
                            date_updated = calendar:universal_time()
                        }),
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

get_notebook_collaborators(NotebookId) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] -> {error, notebook_not_found};
            [Notebook] -> {ok, Notebook#notebook.collaborators}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

is_collaborator(NotebookId, UserId) ->
    Fun = fun() ->
        case mnesia:read({notebook, NotebookId}) of
            [] -> false;
            [Notebook] -> lists:member(UserId, Notebook#notebook.collaborators)
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> false
    end.

    create_version(NotebookId, UserId, ChangeDescription) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    IsOwner = Notebook#notebook.creator_id =:= UserId,
                    IsCollaborator = lists:member(UserId, Notebook#notebook.collaborators),
                    case IsOwner orelse IsCollaborator of
                        false ->
                            {error, unauthorized};
                        true ->
                            case Notebook#notebook.content_cid of
                                undefined ->
                                    {error, no_content_to_version};
                                {pending, _} ->
                                    {error, content_not_ready};
                                CurrentCID ->
                                    Now = calendar:universal_time(),
                                    VersionNum = length(Notebook#notebook.version_cids) + 1,
                                    NewVersion = {VersionNum, CurrentCID, Now, ChangeDescription},
                                    UpdatedVersions = [NewVersion | Notebook#notebook.version_cids],
                                    mnesia:write(Notebook#notebook{
                                        version_cids = UpdatedVersions,
                                        date_updated = Now
                                    }),
                                    {ok, VersionNum}
                            end
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_notebook_versions(NotebookId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] -> {error, notebook_not_found};
                [Notebook] -> {ok, Notebook#notebook.version_cids}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_version_by_number(NotebookId, VersionNum) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    Versions = Notebook#notebook.version_cids,
                    case find_version(VersionNum, Versions) of
                        {ok, Version} -> {ok, Version};
                        {error, Reason} -> {error, Reason}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    find_version(_VersionNum, []) ->
        {error, version_not_found};
    find_version(VersionNum, [Version | Rest]) ->
        case Version of
            {VersionNum, _, _, _} -> {ok, Version};
            {VersionNum, _, _} -> {ok, Version};
            _ -> find_version(VersionNum, Rest)
        end.

    rollback_to_version(NotebookId, UserId, VersionNum) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    case Notebook#notebook.creator_id of
                        UserId ->
                            case find_version(VersionNum, Notebook#notebook.version_cids) of
                                {ok, {_, CID, _, _}} ->
                                    Now = calendar:universal_time(),
                                    mnesia:write(Notebook#notebook{
                                        content_cid = CID,
                                        date_updated = Now
                                    }),
                                    ok;
                                {ok, {_, CID, _}} ->
                                    Now = calendar:universal_time(),
                                    mnesia:write(Notebook#notebook{
                                        content_cid = CID,
                                        date_updated = Now
                                    }),
                                    ok;
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        _ ->
                            {error, unauthorized}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    compare_versions(NotebookId, VersionNum1, VersionNum2) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    Versions = Notebook#notebook.version_cids,
                    case {find_version(VersionNum1, Versions), find_version(VersionNum2, Versions)} of
                        {{ok, V1}, {ok, V2}} ->
                            {ok, #{version1 => V1, version2 => V2}};
                        {{error, _}, _} ->
                            {error, version1_not_found};
                        {_, {error, _}} ->
                            {error, version2_not_found}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    fork_notebook(NotebookId, UserId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [OriginalNotebook] ->
                    case OriginalNotebook#notebook.visibility of
                        private ->
                            IsCollaborator = lists:member(UserId, OriginalNotebook#notebook.collaborators),
                            IsOwner = OriginalNotebook#notebook.creator_id =:= UserId,
                            case IsOwner orelse IsCollaborator of
                                false -> {error, cannot_fork_private_notebook};
                                true -> do_fork(OriginalNotebook, UserId, NotebookId)
                            end;
                        public ->
                            do_fork(OriginalNotebook, UserId, NotebookId)
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    do_fork(OriginalNotebook, UserId, OriginalId) ->
        NewId = nanoid:gen(),
        Now = calendar:universal_time(),

        ForkedNotebook = OriginalNotebook#notebook{
            id = NewId,
            creator_id = UserId,
            title = OriginalNotebook#notebook.title ++ " (Fork)",
            collaborators = [],
            version_cids = [],
            likes = [],
            comments = [],
            date_created = Now,
            date_updated = Now,
            report = [],
            forked_from = OriginalId,
            fork_count = 0,
            execution_count = 0,
            compute_time_used = 0,
            gpu_time_used = 0,
            scheduled_runs = [],
            submission_id = undefined
        },

        mnesia:write(ForkedNotebook),

        UpdatedOriginal = OriginalNotebook#notebook{
            fork_count = OriginalNotebook#notebook.fork_count + 1
        },
        mnesia:write(UpdatedOriginal),

        {ok, NewId}.

    get_notebook_forks(NotebookId) ->
        Fun = fun() ->
            mnesia:match_object(#notebook{forked_from = NotebookId, _ = '_'})
        end,
        {atomic, Res} = mnesia:transaction(Fun),
        Res.

    get_fork_parent(NotebookId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] -> {error, notebook_not_found};
                [Notebook] ->
                    case Notebook#notebook.forked_from of
                        undefined -> {error, not_a_fork};
                        ParentId -> {ok, ParentId}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    increment_fork_count(NotebookId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    mnesia:write(Notebook#notebook{
                        fork_count = Notebook#notebook.fork_count + 1
                    }),
                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    execute_notebook(NotebookId, UserId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    IsOwner = Notebook#notebook.creator_id =:= UserId,
                    IsCollaborator = lists:member(UserId, Notebook#notebook.collaborators),
                    case IsOwner orelse IsCollaborator of
                        false ->
                            {error, unauthorized};
                        true ->
                            Now = calendar:universal_time(),
                            SessionId = nanoid:gen(),

                            mnesia:write(Notebook#notebook{
                                execution_count = Notebook#notebook.execution_count + 1,
                                date_updated = Now
                            }),

                            {ok, SessionId, Notebook#notebook.content_cid}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, {ok, SessionId, ContentCID}} ->
                spawn(fun() ->
                    run_notebook_execution(NotebookId, SessionId, ContentCID)
                end),
                {ok, SessionId};
            {atomic, {error, Reason}} ->
                {error, Reason};
            {aborted, Reason} ->
                {error, {transaction_failed, Reason}}
        end.

    run_notebook_execution(NotebookId, SessionId, _ContentCID) ->
        try
            StartTime = erlang:system_time(second),

            timer:sleep(1000),

            EndTime = erlang:system_time(second),
            Duration = EndTime - StartTime,

            ExecutionLog = #{
                session_id => SessionId,
                start_time => StartTime,
                end_time => EndTime,
                duration => Duration,
                status => completed,
                outputs => []
            },

            LogJSON = jsx:encode(ExecutionLog),
            LogCID = ipfs_content:upload_text(binary_to_list(LogJSON)),

            UpdateF = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [Notebook] ->
                        mnesia:write(Notebook#notebook{
                            execution_logs_cid = LogCID,
                            compute_time_used = Notebook#notebook.compute_time_used + Duration,
                            date_updated = calendar:universal_time()
                        });
                    [] -> ok
                end
            end,
            mnesia:transaction(UpdateF),

            ok
        catch
            Exception:Error:Stacktrace ->
                error_logger:error_msg(
                    "Exception during notebook execution ~p: ~p:~p~n~p",
                    [NotebookId, Exception, Error, Stacktrace]
                )
        end.

    get_execution_logs(NotebookId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] -> {error, notebook_not_found};
                [Notebook] ->
                    case Notebook#notebook.execution_logs_cid of
                        undefined -> {error, no_execution_logs};
                        CID ->
                            try
                                Logs = ipfs_content:get_text_content(CID),
                                {ok, Logs}
                            catch
                                _:Error -> {error, Error}
                            end
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_execution_stats(NotebookId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] -> {error, notebook_not_found};
                [Notebook] ->
                    {ok, #{
                        execution_count => Notebook#notebook.execution_count,
                        compute_time_used => Notebook#notebook.compute_time_used,
                        gpu_time_used => Notebook#notebook.gpu_time_used,
                        last_updated => Notebook#notebook.date_updated
                    }}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    schedule_notebook_run(NotebookId, UserId, CronExpression) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    case Notebook#notebook.creator_id of
                        UserId ->
                            Now = calendar:universal_time(),
                            ScheduleId = nanoid:gen(),
                            NewSchedule = {ScheduleId, CronExpression, true, Now, undefined},
                            UpdatedSchedules = [NewSchedule | Notebook#notebook.scheduled_runs],
                            mnesia:write(Notebook#notebook{
                                scheduled_runs = UpdatedSchedules,
                                date_updated = Now
                            }),
                            {ok, ScheduleId};
                        _ ->
                            {error, unauthorized}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    cancel_scheduled_run(NotebookId, ScheduleId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    UpdatedSchedules = lists:filter(fun(Schedule) ->
                        case Schedule of
                            {Id, _, _, _, _} -> Id =/= ScheduleId;
                            _ -> true
                        end
                    end, Notebook#notebook.scheduled_runs),
                    mnesia:write(Notebook#notebook{
                        scheduled_runs = UpdatedSchedules,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_scheduled_runs(NotebookId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] -> {error, notebook_not_found};
                [Notebook] -> {ok, Notebook#notebook.scheduled_runs}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    like_notebook(NotebookId, UserId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    case lists:member(UserId, Notebook#notebook.likes) of
                        true ->
                            {error, already_liked};
                        false ->
                            UpdatedLikes = [UserId | Notebook#notebook.likes],
                            mnesia:write(Notebook#notebook{
                                likes = UpdatedLikes,
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

    unlike_notebook(NotebookId, UserId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    UpdatedLikes = lists:delete(UserId, Notebook#notebook.likes),
                    mnesia:write(Notebook#notebook{
                        likes = UpdatedLikes,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_notebook_likes(NotebookId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] -> {error, notebook_not_found};
                [Notebook] -> {ok, Notebook#notebook.likes}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    has_user_liked(NotebookId, UserId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] -> false;
                [Notebook] -> lists:member(UserId, Notebook#notebook.likes)
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, _Reason} -> false
        end.

    add_comment(NotebookId, UserId, Content) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    CommentId = nanoid:gen(),
                    Now = calendar:universal_time(),

                    ok = content_cache:set({notebook_comment, CommentId}, Content),

                    NewComment = {CommentId, UserId, {pending, CommentId}, Now},
                    UpdatedComments = [NewComment | Notebook#notebook.comments],
                    mnesia:write(Notebook#notebook{
                        comments = UpdatedComments,
                        date_updated = Now
                    }),

                    {ok, CommentId}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, {ok, CommentId}} ->
                spawn(fun() ->
                    upload_notebook_comment(NotebookId, CommentId)
                end),
                {ok, CommentId};
            {atomic, {error, Reason}} ->
                {error, Reason};
            {aborted, Reason} ->
                {error, {transaction_failed, Reason}}
        end.

    upload_notebook_comment(NotebookId, CommentId) ->
        try
            Content = content_cache:get({notebook_comment, CommentId}),
            CID = ipfs_content:upload_text(Content),

            UpdateF = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [Notebook] ->
                        UpdatedComments = lists:map(fun(Comment) ->
                            case Comment of
                                {CommentId, UserId, {pending, CommentId}, Timestamp} ->
                                    {CommentId, UserId, CID, Timestamp};
                                Other -> Other
                            end
                        end, Notebook#notebook.comments),
                        mnesia:write(Notebook#notebook{comments = UpdatedComments});
                    [] -> ok
                end
            end,
            mnesia:transaction(UpdateF),

            content_cache:delete({notebook_comment, CommentId}),

            ok
        catch
            Exception:Error:Stacktrace ->
                error_logger:error_msg(
                    "Exception while uploading notebook comment ~p: ~p:~p~n~p",
                    [CommentId, Exception, Error, Stacktrace]
                )
        end.

    get_notebook_comments(NotebookId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] -> {error, notebook_not_found};
                [Notebook] -> {ok, Notebook#notebook.comments}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    delete_comment(NotebookId, CommentId, UserId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    case lists:keyfind(CommentId, 1, Notebook#notebook.comments) of
                        false ->
                            {error, comment_not_found};
                        {CommentId, CommentUserId, _, _} ->
                            IsOwner = Notebook#notebook.creator_id =:= UserId,
                            IsCommentAuthor = CommentUserId =:= UserId,
                            case IsOwner orelse IsCommentAuthor of
                                false ->
                                    {error, unauthorized};
                                true ->
                                    UpdatedComments = lists:keydelete(CommentId, 1,
                                                                      Notebook#notebook.comments),
                                    mnesia:write(Notebook#notebook{
                                        comments = UpdatedComments,
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

    update_comment(NotebookId, CommentId, NewContent) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    case lists:keyfind(CommentId, 1, Notebook#notebook.comments) of
                        false ->
                            {error, comment_not_found};
                        {CommentId, UserId, _, Timestamp} ->
                            ok = content_cache:set({notebook_comment_update, CommentId}, NewContent),

                            UpdatedComments = lists:keyreplace(CommentId, 1, Notebook#notebook.comments,
                                {CommentId, UserId, {pending_update, CommentId}, Timestamp}),
                            mnesia:write(Notebook#notebook{
                                comments = UpdatedComments,
                                date_updated = calendar:universal_time()
                            }),

                            {ok, CommentId}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, {ok, CommentId}} ->
                spawn(fun() ->
                    upload_comment_update(NotebookId, CommentId)
                end),
                ok;
            {atomic, {error, Reason}} ->
                {error, Reason};
            {aborted, Reason} ->
                {error, {transaction_failed, Reason}}
        end.

    upload_comment_update(NotebookId, CommentId) ->
        try
            Content = content_cache:get({notebook_comment_update, CommentId}),
            CID = ipfs_content:upload_text(Content),

            UpdateF = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [Notebook] ->
                        UpdatedComments = lists:map(fun(Comment) ->
                            case Comment of
                                {CommentId, UserId, {pending_update, CommentId}, Timestamp} ->
                                    {CommentId, UserId, CID, Timestamp};
                                Other -> Other
                            end
                        end, Notebook#notebook.comments),
                        mnesia:write(Notebook#notebook{comments = UpdatedComments});
                    [] -> ok
                end
            end,
            mnesia:transaction(UpdateF),

            content_cache:delete({notebook_comment_update, CommentId}),

            ok
        catch
            Exception:Error:Stacktrace ->
                error_logger:error_msg(
                    "Exception while updating notebook comment ~p: ~p:~p~n~p",
                    [CommentId, Exception, Error, Stacktrace]
                )
        end.

    set_notebook_environment(NotebookId, Environment) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    mnesia:write(Notebook#notebook{
                        environment = Environment,
                        date_updated = calendar:universal_time()
                    }),
                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_notebook_environment(NotebookId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] -> {error, notebook_not_found};
                [Notebook] -> {ok, Notebook#notebook.environment}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    set_dependencies(NotebookId, Dependencies) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] ->
                    {error, notebook_not_found};
                [Notebook] ->
                    ok = content_cache:set({notebook_deps, NotebookId}, Dependencies),

                    mnesia:write(Notebook#notebook{
                        dependencies_cid = {pending, NotebookId},
                        date_updated = calendar:universal_time()
                    }),

                    {ok, NotebookId}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, {ok, NotebookId}} ->
                spawn(fun() ->
                    upload_dependencies(NotebookId)
                end),
                ok;
            {atomic, {error, Reason}} ->
                {error, Reason};
            {aborted, Reason} ->
                {error, {transaction_failed, Reason}}
        end.

    upload_dependencies(NotebookId) ->
        try
            Deps = content_cache:get({notebook_deps, NotebookId}),
            CID = ipfs_content:upload_text(Deps),

            UpdateF = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [Notebook] ->
                        mnesia:write(Notebook#notebook{dependencies_cid = CID});
                    [] -> ok
                end
            end,
            mnesia:transaction(UpdateF),

            content_cache:delete({notebook_deps, NotebookId}),

            ok
        catch
            Exception:Error:Stacktrace ->
                error_logger:error_msg(
                    "Exception while uploading dependencies ~p: ~p:~p~n~p",
                    [NotebookId, Exception, Error, Stacktrace]
                )
        end.

    get_dependencies(NotebookId) ->
        Fun = fun() ->
            case mnesia:read({notebook, NotebookId}) of
                [] -> {error, notebook_not_found};
                [Notebook] ->
                    case Notebook#notebook.dependencies_cid of
                        undefined -> {error, no_dependencies};
                        {pending, _} -> {error, dependencies_not_ready};
                        CID ->
                            try
                                Deps = ipfs_content:get_text_content(CID),
                                {ok, Deps}
                            catch
                                _:Error -> {error, Error}
                            end
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

        add_citation(NotebookId, Citation) ->
            Fun = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [] ->
                        {error, notebook_not_found};
                    [Notebook] ->
                        UpdatedCitations = [Citation | Notebook#notebook.citations],
                        mnesia:write(Notebook#notebook{
                            citations = UpdatedCitations,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_notebook_citations(NotebookId) ->
            Fun = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [] -> {error, notebook_not_found};
                    [Notebook] -> {ok, Notebook#notebook.citations}
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        remove_citation(NotebookId, Citation) ->
            Fun = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [] ->
                        {error, notebook_not_found};
                    [Notebook] ->
                        UpdatedCitations = lists:delete(Citation, Notebook#notebook.citations),
                        mnesia:write(Notebook#notebook{
                            citations = UpdatedCitations,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        add_interactive_widget(NotebookId, WidgetType, WidgetCid) ->
            Fun = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [] ->
                        {error, notebook_not_found};
                    [Notebook] ->
                        WidgetId = nanoid:gen(),
                        Position = length(Notebook#notebook.interactive_widgets) + 1,
                        NewWidget = {WidgetId, WidgetType, WidgetCid, Position},
                        UpdatedWidgets = Notebook#notebook.interactive_widgets ++ [NewWidget],
                        mnesia:write(Notebook#notebook{
                            interactive_widgets = UpdatedWidgets,
                            date_updated = calendar:universal_time()
                        }),
                        {ok, WidgetId}
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        remove_interactive_widget(NotebookId, WidgetId) ->
            Fun = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [] ->
                        {error, notebook_not_found};
                    [Notebook] ->
                        UpdatedWidgets = lists:filter(fun(Widget) ->
                            case Widget of
                                {Id, _, _, _} -> Id =/= WidgetId;
                                _ -> true
                            end
                        end, Notebook#notebook.interactive_widgets),
                        mnesia:write(Notebook#notebook{
                            interactive_widgets = UpdatedWidgets,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_notebook_widgets(NotebookId) ->
            Fun = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [] -> {error, notebook_not_found};
                    [Notebook] -> {ok, Notebook#notebook.interactive_widgets}
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        set_notebook_type(NotebookId, NotebookType) ->
            Fun = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [] ->
                        {error, notebook_not_found};
                    [Notebook] ->
                        mnesia:write(Notebook#notebook{
                            notebook_type = NotebookType,
                            date_updated = calendar:universal_time()
                        }),
                        ok
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_notebooks_by_type(NotebookType) ->
            Fun = fun() ->
                mnesia:match_object(#notebook{notebook_type = NotebookType, _ = '_'})
            end,
            {atomic, Res} = mnesia:transaction(Fun),
            Res.

        search_notebooks(Query) ->
            Fun = fun() ->
                AllNotebooks = mnesia:match_object(#notebook{_ = '_'}),
                QueryLower = string:to_lower(Query),
                lists:filter(fun(Notebook) ->
                    TitleMatch = string:find(string:to_lower(Notebook#notebook.title), QueryLower) =/= nomatch,
                    DescMatch = string:find(string:to_lower(Notebook#notebook.description), QueryLower) =/= nomatch,
                    Tags = maps:get(tags, Notebook#notebook.data, []),
                    TagMatch = lists:any(fun(Tag) ->
                        string:find(string:to_lower(Tag), QueryLower) =/= nomatch
                    end, Tags),
                    TitleMatch orelse DescMatch orelse TagMatch
                end, AllNotebooks)
            end,

            {atomic, Res} = mnesia:transaction(Fun),
            Res.

        search_notebooks_advanced(SearchParams) ->
            #{
                query := Query,
                language := Language,
                kernel_type := KernelType,
                notebook_type := NotebookType,
                min_likes := MinLikes,
                min_forks := MinForks,
                tags := Tags
            } = SearchParams,

            Fun = fun() ->
                AllNotebooks = mnesia:match_object(#notebook{_ = '_'}),
                QueryLower = string:to_lower(Query),

                lists:filter(fun(Notebook) ->
                    TitleMatch = case Query of
                        "" -> true;
                        _ -> string:find(string:to_lower(Notebook#notebook.title), QueryLower) =/= nomatch
                    end,

                    LanguageMatch = case Language of
                        any -> true;
                        _ -> Notebook#notebook.language =:= Language
                    end,

                    KernelMatch = case KernelType of
                        any -> true;
                        _ -> Notebook#notebook.kernel_type =:= KernelType
                    end,

                    TypeMatch = case NotebookType of
                        any -> true;
                        _ -> Notebook#notebook.notebook_type =:= NotebookType
                    end,

                    LikesMatch = length(Notebook#notebook.likes) >= MinLikes,

                    ForksMatch = Notebook#notebook.fork_count >= MinForks,

                    NotebookTags = maps:get(tags, Notebook#notebook.data, []),
                    TagMatch = case Tags of
                        [] -> true;
                        _ -> lists:any(fun(Tag) -> lists:member(Tag, NotebookTags) end, Tags)
                    end,

                    TitleMatch andalso LanguageMatch andalso KernelMatch andalso
                    TypeMatch andalso LikesMatch andalso ForksMatch andalso TagMatch
                end, AllNotebooks)
            end,

            {atomic, Res} = mnesia:transaction(Fun),
            Res.

        get_trending_notebooks(Limit) ->
            Fun = fun() ->
                PublicNotebooks = mnesia:match_object(#notebook{visibility = public, _ = '_'}),
                Sorted = lists:sort(fun(A, B) ->
                    ScoreA = length(A#notebook.likes) + (A#notebook.fork_count * 2) + A#notebook.execution_count,
                    ScoreB = length(B#notebook.likes) + (B#notebook.fork_count * 2) + B#notebook.execution_count,
                    ScoreA > ScoreB
                end, PublicNotebooks),
                lists:sublist(Sorted, Limit)
            end,

            {atomic, Res} = mnesia:transaction(Fun),
            Res.

        get_featured_notebooks() ->
            Fun = fun() ->
                PublicNotebooks = mnesia:match_object(#notebook{visibility = public, _ = '_'}),
                Sorted = lists:sort(fun(A, B) ->
                    ScoreA = length(A#notebook.likes) + (A#notebook.fork_count * 3),
                    ScoreB = length(B#notebook.likes) + (B#notebook.fork_count * 3),
                    ScoreA > ScoreB
                end, PublicNotebooks),
                lists:sublist(Sorted, 10)
            end,

            {atomic, Res} = mnesia:transaction(Fun),
            Res.

        get_most_forked_notebooks(Limit) ->
            Fun = fun() ->
                PublicNotebooks = mnesia:match_object(#notebook{visibility = public, _ = '_'}),
                Sorted = lists:sort(fun(A, B) ->
                    A#notebook.fork_count > B#notebook.fork_count
                end, PublicNotebooks),
                lists:sublist(Sorted, Limit)
            end,

            {atomic, Res} = mnesia:transaction(Fun),
            Res.

        get_most_liked_notebooks(Limit) ->
            Fun = fun() ->
                PublicNotebooks = mnesia:match_object(#notebook{visibility = public, _ = '_'}),
                Sorted = lists:sort(fun(A, B) ->
                    length(A#notebook.likes) > length(B#notebook.likes)
                end, PublicNotebooks),
                lists:sublist(Sorted, Limit)
            end,

            {atomic, Res} = mnesia:transaction(Fun),
            Res.

        report_notebook(ReporterId, NotebookId, Type, Description) ->
            Fun = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [] -> {error, notebook_not_found};
                    [Notebook] ->
                        ReportId = nanoid:gen(),
                        Now = calendar:universal_time(),
                        Report = #report{
                            id = ReportId,
                            type = Type,
                            description = Description,
                            reporter = ReporterId,
                            date_created = Now,
                            data = #{notebook_id => NotebookId}
                        },
                        mnesia:write(Report),

                        UpdatedReports = [ReportId | Notebook#notebook.report],
                        mnesia:write(Notebook#notebook{report = UpdatedReports}),

                        {ok, ReportId}
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_notebook_stats(NotebookId) ->
            Fun = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [] -> {error, notebook_not_found};
                    [Notebook] ->
                        {ok, #{
                            likes_count => length(Notebook#notebook.likes),
                            comments_count => length(Notebook#notebook.comments),
                            fork_count => Notebook#notebook.fork_count,
                            execution_count => Notebook#notebook.execution_count,
                            compute_time_used => Notebook#notebook.compute_time_used,
                            gpu_time_used => Notebook#notebook.gpu_time_used,
                            versions_count => length(Notebook#notebook.version_cids),
                            collaborators_count => length(Notebook#notebook.collaborators),
                            datasets_count => length(Notebook#notebook.dataset_cids),
                            widgets_count => length(Notebook#notebook.interactive_widgets),
                            citations_count => length(Notebook#notebook.citations),
                            language => Notebook#notebook.language,
                            kernel_type => Notebook#notebook.kernel_type,
                            notebook_type => Notebook#notebook.notebook_type,
                            visibility => Notebook#notebook.visibility,
                            created => Notebook#notebook.date_created,
                            last_updated => Notebook#notebook.date_updated
                        }}
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_user_notebook_stats(UserId) ->
            Fun = fun() ->
                UserNotebooks = mnesia:match_object(#notebook{creator_id = UserId, _ = '_'}),

                TotalNotebooks = length(UserNotebooks),

                TotalLikes = lists:foldl(fun(N, Acc) ->
                    Acc + length(N#notebook.likes)
                end, 0, UserNotebooks),

                TotalForks = lists:foldl(fun(N, Acc) ->
                    Acc + N#notebook.fork_count
                end, 0, UserNotebooks),

                TotalExecutions = lists:foldl(fun(N, Acc) ->
                    Acc + N#notebook.execution_count
                end, 0, UserNotebooks),

                TotalComputeTime = lists:foldl(fun(N, Acc) ->
                    Acc + N#notebook.compute_time_used
                end, 0, UserNotebooks),

                TotalGPUTime = lists:foldl(fun(N, Acc) ->
                    Acc + N#notebook.gpu_time_used
                end, 0, UserNotebooks),

                LanguageDistribution = lists:foldl(fun(N, Acc) ->
                    Lang = N#notebook.language,
                    maps:update_with(Lang, fun(V) -> V + 1 end, 1, Acc)
                end, #{}, UserNotebooks),

                TypeDistribution = lists:foldl(fun(N, Acc) ->
                    Type = N#notebook.notebook_type,
                    maps:update_with(Type, fun(V) -> V + 1 end, 1, Acc)
                end, #{}, UserNotebooks),

                ForkedNotebooks = lists:filter(fun(N) ->
                    N#notebook.forked_from =/= undefined
                end, UserNotebooks),

                {ok, #{
                    total_notebooks => TotalNotebooks,
                    total_likes => TotalLikes,
                    total_forks => TotalForks,
                    total_executions => TotalExecutions,
                    total_compute_time => TotalComputeTime,
                    total_gpu_time => TotalGPUTime,
                    language_distribution => LanguageDistribution,
                    type_distribution => TypeDistribution,
                    forked_notebooks_count => length(ForkedNotebooks),
                    average_likes => case TotalNotebooks of
                        0 -> 0;
                        _ -> TotalLikes / TotalNotebooks
                    end,
                    average_forks => case TotalNotebooks of
                        0 -> 0;
                        _ -> TotalForks / TotalNotebooks
                    end
                }}
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        pin_notebook(NotebookId) ->
            Fun = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [] -> {error, notebook_not_found};
                    [Notebook] ->
                        ContentCID = Notebook#notebook.content_cid,
                        case ContentCID of
                            undefined -> {error, no_content};
                            {pending, _} -> {error, content_not_ready};
                            _ ->
                                spawn(fun() ->
                                    try
                                        ipfs_client_5:pin_add([{arg, ContentCID}])
                                    catch
                                        _:Error ->
                                            error_logger:error_msg("Failed to pin notebook ~p: ~p",
                                                                  [NotebookId, Error])
                                    end
                                end),
                                ok
                        end
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        unpin_notebook(NotebookId) ->
            Fun = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [] -> {error, notebook_not_found};
                    [Notebook] ->
                        ContentCID = Notebook#notebook.content_cid,
                        case ContentCID of
                            undefined -> {error, no_content};
                            {pending, _} -> {error, content_not_ready};
                            _ ->
                                spawn(fun() ->
                                    try
                                        ipfs_client_5:pin_rm([{arg, ContentCID}])
                                    catch
                                        _:Error ->
                                            error_logger:error_msg("Failed to unpin notebook ~p: ~p",
                                                                  [NotebookId, Error])
                                    end
                                end),
                                ok
                        end
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        submit_notebook_to_competition(NotebookId, UserId, CompetitionId) ->
            Fun = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [] ->
                        {error, notebook_not_found};
                    [Notebook] ->
                        IsOwner = Notebook#notebook.creator_id =:= UserId,
                        IsCollaborator = lists:member(UserId, Notebook#notebook.collaborators),
                        case IsOwner orelse IsCollaborator of
                            false ->
                                {error, unauthorized};
                            true ->
                                case mnesia:read({competition, CompetitionId}) of
                                    [] ->
                                        {error, competition_not_found};
                                    [Competition] ->
                                        case Competition#competition.status of
                                            active ->
                                                case lists:member(UserId, Competition#competition.participants) of
                                                    false ->
                                                        {error, not_a_participant};
                                                    true ->
                                                        SubmissionId = nanoid:gen(),
                                                        Now = calendar:universal_time(),

                                                        Submission = #submission{
                                                            id = SubmissionId,
                                                            competition_id = CompetitionId,
                                                            team_id = undefined,
                                                            user_id = UserId,
                                                            submission_cid = Notebook#notebook.content_cid,
                                                            notebook_id = NotebookId,
                                                            submission_number = 1,
                                                            score_public = undefined,
                                                            score_private = undefined,
                                                            evaluation_status = pending,
                                                            evaluation_cid = undefined,
                                                            error_message = undefined,
                                                            submission_time = Now,
                                                            compute_session_id = undefined,
                                                            late_submission = false,
                                                            disqualified = false,
                                                            disqualification_reason = undefined,
                                                            file_size_bytes = 0,
                                                            evaluation_time_seconds = undefined,
                                                            metadata = #{notebook_id => NotebookId}
                                                        },

                                                        mnesia:write(Submission),

                                                        UpdatedSubmissions = [SubmissionId |
                                                                             Competition#competition.submission_ids],
                                                        mnesia:write(Competition#competition{
                                                            submission_ids = UpdatedSubmissions,
                                                            date_updated = Now
                                                        }),

                                                        mnesia:write(Notebook#notebook{
                                                            submission_id = SubmissionId,
                                                            competition_id = CompetitionId,
                                                            notebook_type = competition_submission,
                                                            date_updated = Now
                                                        }),

                                                        {ok, SubmissionId}
                                                end;
                                            _ ->
                                                {error, competition_not_active}
                                        end
                                end
                        end
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.

        get_submission_from_notebook(NotebookId) ->
            Fun = fun() ->
                case mnesia:read({notebook, NotebookId}) of
                    [] -> {error, notebook_not_found};
                    [Notebook] ->
                        case Notebook#notebook.submission_id of
                            undefined -> {error, no_submission};
                            SubmissionId -> {ok, SubmissionId}
                        end
                end
            end,

            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, {transaction_failed, Reason}}
            end.
