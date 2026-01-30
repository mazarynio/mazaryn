-module(modeldb).
-author("Zaryn Technologies").

-export([
    create_model/10,
    create_model_concurrent/10,
    create_model_from_file/9,
    update_model/11,
    delete_model/2,
    upload_model_file/2,

    get_model_by_id/1,
    get_models_by_creator/1,
    get_public_models/0,
    get_models_by_framework/1,
    get_models_by_task/1,
    get_models_by_tag/1,
    download_model/2,

    create_model_version/4,
    get_model_versions/1,
    get_version_by_number/2,
    rollback_to_version/3,

    add_training_dataset/2,
    remove_training_dataset/2,
    get_training_datasets/1,

    set_performance_metrics/2,
    get_performance_metrics/1,
    update_metric/3,

    deploy_model/2,
    undeploy_model/2,
    get_deployment_info/1,
    update_deployment_endpoint/2,

    add_benchmark_result/4,
    get_benchmark_results/1,
    get_best_benchmark/2,

    rate_model/3,
    get_model_rating/1,

    increment_download_count/1,
    track_inference_time/2,
    calculate_carbon_footprint/2,

    search_models/1,
    search_models_advanced/1,
    get_trending_models/1,
    get_featured_models/0,
    get_models_by_performance/2,

    pin_model/1,
    unpin_model/1,
    update_pin_status/2,

    report_model/4,

    link_to_competition/2,
    unlink_from_competition/2,
    get_competition_models/1,

    export_model_card/1,
    import_model_card/2,

    validate_model_file/1,
    get_supported_frameworks/0,
    decode_model_id/1
]).

-include("../ml_records.hrl").
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("kernel/include/file.hrl").

-define(MAX_FILE_SIZE, 21474836480).
-define(MAX_RETRIES, 10).
-define(BACKOFF_TIME, 20).
-define(SUPPORTED_FRAMEWORKS, [
    "tensorflow", "pytorch", "keras", "scikit-learn",
    "xgboost", "lightgbm", "catboost", "onnx",
    "mxnet", "paddle", "jax", "huggingface"
]).
-define(CHUNK_SIZE, 10485760).

create_model(CreatorId, Title, Description, Framework, TaskType, ModelFile, License, Tags, Visibility, PerformanceMetrics) ->
    Fun = fun() ->
        PlainId = nanoid:gen(),
        Id = encode_id(PlainId),
        Now = calendar:universal_time(),

        FileContent = if
            is_binary(ModelFile) -> ModelFile;
            is_list(ModelFile) -> list_to_binary(ModelFile);
            true -> <<>>
        end,

        ok = content_cache:set({model_file, Id}, FileContent),
        SizeBytes = byte_size(FileContent),

        TitleList = ensure_list(Title),
        DescriptionList = ensure_list(Description),
        FrameworkList = ensure_list(Framework),
        TaskTypeAtom = ensure_atom(TaskType),
        LicenseList = ensure_list(License),
        TagsList = [ensure_list(Tag) || Tag <- Tags],
        VisibilityAtom = ensure_atom(Visibility),
        CreatorIdList = ensure_list(CreatorId),

        Model = #model{
            id = Id,
            creator_id = CreatorIdList,
            title = TitleList,
            description = DescriptionList,
            framework = FrameworkList,
            task_type = TaskTypeAtom,
            file_cid = {pending, Id},
            size_bytes = SizeBytes,
            license = LicenseList,
            tags = TagsList,
            visibility = VisibilityAtom,
            downloads = 0,
            pin_info = [],
            deployment_info = #{},
            date_created = Now,
            date_updated = Now,
            report = [],
            data = #{},
            training_dataset_cids = [],
            performance_metrics = PerformanceMetrics,
            inference_api_endpoint = undefined,
            docker_image_cid = undefined,
            model_card_cid = undefined,
            version_history = [{1, "1.0.0", {pending, Id}, Now, "Initial version"}],
            benchmark_results = [],
            dependencies_cid = undefined,
            inference_time_ms = 0,
            carbon_footprint = 0.0
        },

        mnesia:write(Model),

        case mnesia:read({user, CreatorIdList}) of
            [User] ->
                UpdatedModels = [Id | User#user.datasets],
                mnesia:write(User#user{datasets = UpdatedModels});
            [] ->
                error_logger:warning_msg("User ~p not found when creating model", [CreatorIdList])
        end,

        {ok, Id}
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, Id}} ->
            spawn(fun() ->
                upload_model_to_ipfs(Id)
            end),
            Id;
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, {transaction_failed, Reason}}
    end.

encode_id(PlainId) when is_binary(PlainId) ->
    encode_id(binary_to_list(PlainId));
encode_id(PlainId) when is_list(PlainId) ->
    "id:" ++ PlainId;
encode_id(PlainId) ->
    encode_id(to_string(PlainId)).

create_model_concurrent(CreatorId, Title, Description, Framework, TaskType, ModelFile, License, Tags, Visibility, PerformanceMetrics) ->
    IdFuture = spawn_monitor(fun() -> exit({result, nanoid:gen()}) end),
    Id = receive_result(IdFuture),

    Now = calendar:universal_time(),

    FileContent = if
        is_binary(ModelFile) -> ModelFile;
        is_list(ModelFile) -> list_to_binary(ModelFile);
        true -> <<>>
    end,

    CacheFuture = spawn_monitor(fun() ->
        ok = content_cache:set({model_file, Id}, FileContent),
        exit({result, ok})
    end),

    SizeFuture = spawn_monitor(fun() ->
        exit({result, byte_size(FileContent)})
    end),

    receive_result(CacheFuture),
    SizeBytes = receive_result(SizeFuture),

    Model = #model{
        id = Id,
        creator_id = CreatorId,
        title = Title,
        description = Description,
        framework = Framework,
        task_type = TaskType,
        file_cid = {pending, Id},
        size_bytes = SizeBytes,
        license = License,
        tags = Tags,
        visibility = Visibility,
        downloads = 0,
        pin_info = [],
        deployment_info = #{},
        date_created = Now,
        date_updated = Now,
        report = [],
        data = #{},
        training_dataset_cids = [],
        performance_metrics = PerformanceMetrics,
        inference_api_endpoint = undefined,
        docker_image_cid = undefined,
        model_card_cid = undefined,
        version_history = [{1, "1.0.0", {pending, Id}, Now, "Initial version"}],
        benchmark_results = [],
        dependencies_cid = undefined,
        inference_time_ms = 0,
        carbon_footprint = 0.0
    },

    case write_model_with_retry(Model, CreatorId, ?MAX_RETRIES) of
        ok ->
            spawn(fun() ->
                upload_model_to_ipfs(Id)
            end),
            Id;
        {error, Reason} ->
            {error, Reason}
    end.

create_model_from_file(CreatorId, Title, Description, Framework, TaskType, FilePath, License, Tags, Visibility) ->
    Path = case is_binary(FilePath) of
        true -> binary_to_list(FilePath);
        false -> FilePath
    end,

    case validate_model_file(Path) of
        {error, Reason} ->
            {error, Reason};
        {ok, FileInfo} ->
            case file:read_file(Path) of
                {ok, FileContent} ->
                    PerformanceMetrics = #{},
                    create_model(CreatorId, Title, Description, Framework, TaskType, FileContent, License, Tags, Visibility, PerformanceMetrics);
                {error, Reason} ->
                    {error, {file_read_error, Reason}}
            end
    end.

update_model(ModelId, CreatorId, NewTitle, NewDescription, NewFramework, NewTaskType, NewModelFile, NewLicense, NewTags, NewVisibility, NewPerformanceMetrics) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] ->
                {error, model_not_found};
            [Model] ->
                case Model#model.creator_id of
                    CreatorId ->
                        Now = calendar:universal_time(),

                        {NewFileCID, NewSizeBytes} = case NewModelFile of
                            undefined ->
                                {Model#model.file_cid, Model#model.size_bytes};
                            _ ->
                                FileContent = if
                                    is_binary(NewModelFile) -> NewModelFile;
                                    is_list(NewModelFile) -> list_to_binary(NewModelFile);
                                    true -> <<>>
                                end,
                                content_cache:set({model_update, ModelId}, FileContent),
                                Size = byte_size(FileContent),
                                {{pending_update, ModelId}, Size}
                        end,

                        {CurrentMajor, CurrentMinor, CurrentPatch} = parse_version(lists:nth(1, Model#model.version_history)),
                        NewVersion = format_version({CurrentMajor, CurrentMinor, CurrentPatch + 1}),

                        VersionNum = length(Model#model.version_history) + 1,
                        NewVersionEntry = {VersionNum, NewVersion, NewFileCID, Now, "Updated model"},
                        UpdatedVersionHistory = [NewVersionEntry | Model#model.version_history],

                        UpdatedModel = Model#model{
                            title = NewTitle,
                            description = NewDescription,
                            framework = NewFramework,
                            task_type = NewTaskType,
                            file_cid = NewFileCID,
                            license = NewLicense,
                            tags = NewTags,
                            visibility = NewVisibility,
                            performance_metrics = NewPerformanceMetrics,
                            version_history = UpdatedVersionHistory,
                            size_bytes = NewSizeBytes,
                            date_updated = Now
                        },

                        mnesia:write(UpdatedModel),

                        case NewModelFile of
                            undefined -> ok;
                            _ ->
                                spawn(fun() ->
                                    upload_model_update(ModelId)
                                end)
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

    delete_model(ModelId, UserId) ->
        Fun = fun() ->
            DecodedModelId = decode_model_id(ModelId),
            DecodedUserId = decode_model_id(UserId),

            case mnesia:read({model, DecodedModelId}) of
                [] ->
                    {error, model_not_found};
                [Model] ->
                    ModelCreatorId = Model#model.creator_id,

                    NormalizedCreatorId = normalize_for_comparison(ModelCreatorId),
                    NormalizedUserId = normalize_for_comparison(DecodedUserId),

                    case NormalizedCreatorId =:= NormalizedUserId of
                        true ->
                            mnesia:delete({model, Model#model.id}),

                            case mnesia:read({user, DecodedUserId}) of
                                [User] ->
                                    UpdatedModels = lists:delete(Model#model.id, User#user.datasets),
                                    mnesia:write(User#user{datasets = UpdatedModels});
                                [] ->
                                    ok
                            end,

                            ok;
                        false ->
                            {error, unauthorized}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} ->
                Result;
            {aborted, Reason} ->
                {error, {transaction_failed, Reason}}
        end.

    normalize_for_comparison(Id) when is_list(Id) ->
        case string:prefix(Id, "id:") of
            nomatch -> Id;
            Rest -> Rest
        end;
    normalize_for_comparison(Id) ->
        normalize_for_comparison(to_string(Id)).

upload_model_file(ModelId, FilePath) ->
    case validate_model_file(FilePath) of
        {error, Reason} ->
            {error, Reason};
        {ok, _FileInfo} ->
            case file:read_file(FilePath) of
                {ok, FileContent} ->
                    Fun = fun() ->
                        case mnesia:read({model, ModelId}) of
                            [] ->
                                {error, model_not_found};
                            [Model] ->
                                Now = calendar:universal_time(),
                                SizeBytes = byte_size(FileContent),

                                ok = content_cache:set({model_file_update, ModelId}, FileContent),

                                {Major, Minor, _Patch} = parse_version(lists:nth(1, Model#model.version_history)),
                                NewVersion = format_version({Major, Minor + 1, 0}),

                                VersionNum = length(Model#model.version_history) + 1,
                                NewVersionEntry = {VersionNum, NewVersion, {pending_update, ModelId}, Now, "File updated"},
                                UpdatedVersionHistory = [NewVersionEntry | Model#model.version_history],

                                UpdatedModel = Model#model{
                                    file_cid = {pending_update, ModelId},
                                    size_bytes = SizeBytes,
                                    version_history = UpdatedVersionHistory,
                                    date_updated = Now
                                },

                                mnesia:write(UpdatedModel),

                                spawn(fun() ->
                                    upload_model_file_to_ipfs(ModelId)
                                end),

                                ok
                        end
                    end,

                    case mnesia:transaction(Fun) of
                        {atomic, Result} -> Result;
                        {aborted, Reason} -> {error, {transaction_failed, Reason}}
                    end;
                {error, Reason} ->
                    {error, {file_read_error, Reason}}
            end
    end.

    get_model_by_id(ModelId) ->
        DecodedId = ensure_list(ModelId),
        Fun = fun() ->
            case mnesia:read({model, DecodedId}) of
                [] -> {error, model_not_found};
                [Model] -> Model
            end
        end,
        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.


get_models_by_creator(CreatorId) ->
    Fun = fun() ->
        mnesia:match_object(#model{creator_id = CreatorId, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_public_models() ->
    Fun = fun() ->
        mnesia:match_object(#model{visibility = public, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_models_by_framework(Framework) ->
    Fun = fun() ->
        mnesia:match_object(#model{framework = Framework, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_models_by_task(TaskType) ->
    Fun = fun() ->
        mnesia:match_object(#model{task_type = TaskType, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_models_by_tag(Tag) ->
    Fun = fun() ->
        AllModels = mnesia:match_object(#model{_ = '_'}),
        lists:filter(fun(Model) ->
            lists:member(Tag, Model#model.tags)
        end, AllModels)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

download_model(ModelId, _UserId) ->
    increment_download_count(ModelId),
    get_model_file(ModelId).

get_model_file(ModelId) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                FileCID = Model#model.file_cid,
                case FileCID of
                    {pending, Id} when Id =:= ModelId ->
                        case content_cache:get({model_file, Id}) of
                            undefined -> {error, file_not_ready};
                            CachedFile -> {ok, CachedFile}
                        end;
                    {pending_update, Id} when Id =:= ModelId ->
                        case content_cache:get({model_update, Id}) of
                            undefined -> {error, file_not_ready};
                            CachedFile -> {ok, CachedFile}
                        end;
                    _ ->
                        try
                            ActualFile = ipfs_media:get_media_binary(FileCID),
                            {ok, ActualFile}
                        catch
                            _:Error -> {error, Error}
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, File}} -> File;
        {atomic, {error, Reason}} -> {error, Reason};
        Error -> Error
    end.

create_model_version(ModelId, UserId, NewModelFile, ChangeDescription) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] ->
                {error, model_not_found};
            [Model] ->
                case Model#model.creator_id of
                    UserId ->
                        Now = calendar:universal_time(),

                        FileContent = if
                            is_binary(NewModelFile) -> NewModelFile;
                            is_list(NewModelFile) -> list_to_binary(NewModelFile);
                            true -> <<>>
                        end,
                        content_cache:set({model_version, ModelId}, FileContent),

                        {Major, Minor, _Patch} = parse_version(lists:nth(1, Model#model.version_history)),
                        NewVersion = format_version({Major, Minor + 1, 0}),

                        VersionNum = length(Model#model.version_history) + 1,
                        NewVersionEntry = {VersionNum, NewVersion, {pending_version, ModelId}, Now, ChangeDescription},
                        UpdatedVersionHistory = [NewVersionEntry | Model#model.version_history],

                        UpdatedModel = Model#model{
                            file_cid = {pending_version, ModelId},
                            version_history = UpdatedVersionHistory,
                            date_updated = Now
                        },

                        mnesia:write(UpdatedModel),

                        spawn(fun() ->
                            upload_model_version(ModelId)
                        end),

                        {ok, NewVersion};
                    _ ->
                        {error, unauthorized}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_model_versions(ModelId) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] -> {ok, Model#model.version_history}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_version_by_number(ModelId, VersionNum) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                case lists:keyfind(VersionNum, 1, Model#model.version_history) of
                    false -> {error, version_not_found};
                    VersionEntry -> {ok, VersionEntry}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

rollback_to_version(ModelId, UserId, VersionNum) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] ->
                {error, model_not_found};
            [Model] ->
                case Model#model.creator_id of
                    UserId ->
                        case lists:keyfind(VersionNum, 1, Model#model.version_history) of
                            false ->
                                {error, version_not_found};
                            {_, _Version, CID, _, _} ->
                                Now = calendar:universal_time(),
                                UpdatedModel = Model#model{
                                    file_cid = CID,
                                    date_updated = Now
                                },
                                mnesia:write(UpdatedModel),
                                ok
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

add_training_dataset(ModelId, DatasetCID) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                UpdatedDatasets = case lists:member(DatasetCID, Model#model.training_dataset_cids) of
                    true -> Model#model.training_dataset_cids;
                    false -> [DatasetCID | Model#model.training_dataset_cids]
                end,
                mnesia:write(Model#model{
                    training_dataset_cids = UpdatedDatasets,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

remove_training_dataset(ModelId, DatasetCID) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                UpdatedDatasets = lists:delete(DatasetCID, Model#model.training_dataset_cids),
                mnesia:write(Model#model{
                    training_dataset_cids = UpdatedDatasets,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_training_datasets(ModelId) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] -> {ok, Model#model.training_dataset_cids}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

set_performance_metrics(ModelId, Metrics) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                mnesia:write(Model#model{
                    performance_metrics = Metrics,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_performance_metrics(ModelId) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] -> {ok, Model#model.performance_metrics}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_metric(ModelId, MetricName, MetricValue) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                UpdatedMetrics = maps:put(MetricName, MetricValue, Model#model.performance_metrics),
                mnesia:write(Model#model{
                    performance_metrics = UpdatedMetrics,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

deploy_model(ModelId, DeploymentConfig) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                EndpointUrl = generate_endpoint_url(ModelId),
                DeploymentInfo = maps:merge(DeploymentConfig, #{
                    endpoint_url => EndpointUrl,
                    deployed_at => calendar:universal_time(),
                    status => active
                }),
                mnesia:write(Model#model{
                    deployment_info = DeploymentInfo,
                    inference_api_endpoint = EndpointUrl,
                    date_updated = calendar:universal_time()
                }),
                {ok, EndpointUrl}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

undeploy_model(ModelId, UserId) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                case Model#model.creator_id of
                    UserId ->
                        mnesia:write(Model#model{
                            deployment_info = #{},
                            inference_api_endpoint = undefined,
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

get_deployment_info(ModelId) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] -> {ok, Model#model.deployment_info}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

update_deployment_endpoint(ModelId, NewEndpoint) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                UpdatedDeployment = maps:put(endpoint_url, NewEndpoint, Model#model.deployment_info),
                mnesia:write(Model#model{
                    deployment_info = UpdatedDeployment,
                    inference_api_endpoint = NewEndpoint,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

add_benchmark_result(ModelId, DatasetId, Metric, Score) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                BenchmarkEntry = {DatasetId, Metric, Score},
                UpdatedBenchmarks = [BenchmarkEntry | Model#model.benchmark_results],
                mnesia:write(Model#model{
                    benchmark_results = UpdatedBenchmarks,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_benchmark_results(ModelId) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] -> {ok, Model#model.benchmark_results}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_best_benchmark(ModelId, Metric) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                FilteredBenchmarks = lists:filter(fun({_, M, _}) -> M =:= Metric end, Model#model.benchmark_results),
                case FilteredBenchmarks of
                    [] -> {error, no_benchmarks_for_metric};
                    _ ->
                        Best = lists:foldl(fun({D, M, S}, {_, _, BestS} = Acc) ->
                            if S > BestS -> {D, M, S};
                               true -> Acc
                            end
                        end, hd(FilteredBenchmarks), tl(FilteredBenchmarks)),
                        {ok, Best}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

rate_model(ModelId, UserId, Rating) when Rating >= 1, Rating =< 5 ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                Now = calendar:universal_time(),
                RatingEntry = {UserId, Rating, Now},

                DataMap = Model#model.data,
                CurrentRatings = maps:get(ratings, DataMap, []),
                UpdatedRatings = lists:keystore(UserId, 1, CurrentRatings, RatingEntry),
                UpdatedDataMap = maps:put(ratings, UpdatedRatings, DataMap),

                mnesia:write(Model#model{data = UpdatedDataMap}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end;
rate_model(_, _, _) ->
    {error, invalid_rating}.

get_model_rating(ModelId) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                DataMap = Model#model.data,
                Ratings = maps:get(ratings, DataMap, []),
                case Ratings of
                    [] -> {ok, 0, 0.0};
                    _ ->
                        RatingValues = [R || {_, R, _} <- Ratings],
                        AvgRating = lists:sum(RatingValues) / length(RatingValues),
                        {ok, length(Ratings), AvgRating}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

increment_download_count(ModelId) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                mnesia:write(Model#model{downloads = Model#model.downloads + 1}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

track_inference_time(ModelId, TimeMs) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                CurrentTime = Model#model.inference_time_ms,
                NewAvgTime = case CurrentTime of
                    0 -> TimeMs;
                    _ -> (CurrentTime + TimeMs) / 2
                end,
                mnesia:write(Model#model{inference_time_ms = NewAvgTime}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

calculate_carbon_footprint(ModelId, TrainingHours) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                CarbonPerHour = 0.5,
                Footprint = TrainingHours * CarbonPerHour,
                mnesia:write(Model#model{carbon_footprint = Footprint}),
                {ok, Footprint}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

search_models(Query) ->
    Fun = fun() ->
        AllModels = mnesia:match_object(#model{_ = '_'}),
        QueryLower = string:to_lower(Query),
        lists:filter(fun(Model) ->
            TitleMatch = string:find(string:to_lower(Model#model.title), QueryLower) =/= nomatch,
            DescMatch = string:find(string:to_lower(Model#model.description), QueryLower) =/= nomatch,
            TagMatch = lists:any(fun(Tag) ->
                string:find(string:to_lower(Tag), QueryLower) =/= nomatch
            end, Model#model.tags),
            TitleMatch orelse DescMatch orelse TagMatch
        end, AllModels)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

search_models_advanced(SearchParams) ->
    #{
        query := Query,
        framework := Framework,
        task_type := TaskType,
        min_downloads := MinDownloads,
        tags := Tags
    } = SearchParams,

    Fun = fun() ->
        AllModels = mnesia:match_object(#model{_ = '_'}),
        QueryLower = string:to_lower(Query),

        lists:filter(fun(Model) ->
            TitleMatch = case Query of
                "" -> true;
                _ -> string:find(string:to_lower(Model#model.title), QueryLower) =/= nomatch
            end,

            FrameworkMatch = case Framework of
                any -> true;
                _ -> Model#model.framework =:= Framework
            end,

            TaskMatch = case TaskType of
                any -> true;
                _ -> Model#model.task_type =:= TaskType
            end,

            DownloadMatch = Model#model.downloads >= MinDownloads,

            TagMatch = case Tags of
                [] -> true;
                _ -> lists:any(fun(Tag) -> lists:member(Tag, Model#model.tags) end, Tags)
            end,

            TitleMatch andalso FrameworkMatch andalso TaskMatch andalso DownloadMatch andalso TagMatch
        end, AllModels)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_trending_models(Limit) ->
    Fun = fun() ->
        AllModels = mnesia:match_object(#model{visibility = public, _ = '_'}),
        Sorted = lists:sort(fun(A, B) ->
            A#model.downloads > B#model.downloads
        end, AllModels),
        lists:sublist(Sorted, Limit)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_featured_models() ->
    Fun = fun() ->
        AllModels = mnesia:match_object(#model{visibility = public, _ = '_'}),
        Sorted = lists:sort(fun(A, B) ->
            ScoreA = A#model.downloads * 2 + length(A#model.benchmark_results),
            ScoreB = B#model.downloads * 2 + length(B#model.benchmark_results),
            ScoreA > ScoreB
        end, AllModels),
        lists:sublist(Sorted, 10)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_models_by_performance(Metric, MinScore) ->
    Fun = fun() ->
        AllModels = mnesia:match_object(#model{_ = '_'}),
        lists:filter(fun(Model) ->
            Metrics = Model#model.performance_metrics,
            case maps:get(Metric, Metrics, undefined) of
                undefined -> false;
                Score -> Score >= MinScore
            end
        end, AllModels)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

pin_model(ModelId) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                FileCID = Model#model.file_cid,
                case is_cid_ready(FileCID) of
                    false -> {error, file_not_ready};
                    true ->
                        spawn(fun() ->
                            try
                                ipfs_client_5:pin_add([{arg, FileCID}])
                            catch
                                _:Error ->
                                    error_logger:error_msg("Failed to pin model ~p: ~p", [ModelId, Error])
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

unpin_model(ModelId) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                FileCID = Model#model.file_cid,
                case is_cid_ready(FileCID) of
                    false -> {error, file_not_ready};
                    true ->
                        spawn(fun() ->
                            try
                                ipfs_client_5:pin_rm([{arg, FileCID}])
                            catch
                                _:Error ->
                                    error_logger:error_msg("Failed to unpin model ~p: ~p", [ModelId, Error])
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

update_pin_status(ModelId, PinInfo) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                mnesia:write(Model#model{pin_info = PinInfo}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

report_model(ReporterId, ModelId, Type, Description) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                ReportId = nanoid:gen(),
                Now = calendar:universal_time(),
                Report = #report{
                    id = ReportId,
                    type = Type,
                    description = Description,
                    reporter = ReporterId,
                    date_created = Now,
                    data = #{model_id => ModelId}
                },
                mnesia:write(Report),

                UpdatedReports = [ReportId | Model#model.report],
                mnesia:write(Model#model{report = UpdatedReports}),

                {ok, ReportId}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

link_to_competition(ModelId, CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                DataMap = Model#model.data,
                Competitions = maps:get(competitions, DataMap, []),
                UpdatedCompetitions = case lists:member(CompetitionId, Competitions) of
                    true -> Competitions;
                    false -> [CompetitionId | Competitions]
                end,
                UpdatedDataMap = maps:put(competitions, UpdatedCompetitions, DataMap),
                mnesia:write(Model#model{data = UpdatedDataMap}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

unlink_from_competition(ModelId, CompetitionId) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                DataMap = Model#model.data,
                Competitions = maps:get(competitions, DataMap, []),
                UpdatedCompetitions = lists:delete(CompetitionId, Competitions),
                UpdatedDataMap = maps:put(competitions, UpdatedCompetitions, DataMap),
                mnesia:write(Model#model{data = UpdatedDataMap}),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_competition_models(CompetitionId) ->
    Fun = fun() ->
        AllModels = mnesia:match_object(#model{_ = '_'}),
        lists:filter(fun(Model) ->
            DataMap = Model#model.data,
            Competitions = maps:get(competitions, DataMap, []),
            lists:member(CompetitionId, Competitions)
        end, AllModels)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

export_model_card(ModelId) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                Card = #{
                    id => Model#model.id,
                    title => Model#model.title,
                    description => Model#model.description,
                    framework => Model#model.framework,
                    task_type => Model#model.task_type,
                    license => Model#model.license,
                    tags => Model#model.tags,
                    performance_metrics => Model#model.performance_metrics,
                    training_datasets => Model#model.training_dataset_cids,
                    benchmark_results => Model#model.benchmark_results,
                    inference_time_ms => Model#model.inference_time_ms,
                    carbon_footprint => Model#model.carbon_footprint,
                    date_created => Model#model.date_created
                },
                {ok, Card}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

import_model_card(ModelId, CardData) ->
    Fun = fun() ->
        case mnesia:read({model, ModelId}) of
            [] -> {error, model_not_found};
            [Model] ->
                CardJSON = jsx:encode(CardData),
                CardCID = ipfs_content:upload_text(binary_to_list(CardJSON)),
                mnesia:write(Model#model{
                    model_card_cid = CardCID,
                    date_updated = calendar:universal_time()
                }),
                ok
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

validate_model_file(FilePath) ->
    case filelib:is_file(FilePath) of
        false ->
            {error, file_not_found};
        true ->
            case file:read_file_info(FilePath) of
                {ok, FileInfo} ->
                    Size = FileInfo#file_info.size,
                    if
                        Size > ?MAX_FILE_SIZE ->
                            {error, {file_too_large, Size, ?MAX_FILE_SIZE}};
                        Size == 0 ->
                            {error, empty_file};
                        true ->
                            {ok, FileInfo}
                    end;
                {error, Reason} ->
                    {error, {file_info_error, Reason}}
            end
    end.

get_supported_frameworks() ->
    ?SUPPORTED_FRAMEWORKS.

upload_model_to_ipfs(ModelId) ->
    try
        FileContent = content_cache:get({model_file, ModelId}),

        CID = case byte_size(FileContent) of
            Size when Size > ?CHUNK_SIZE ->
                upload_large_file_chunked(FileContent);
            _ ->
                ipfs_media:upload_media(FileContent)
        end,

        UpdateF = fun() ->
            case mnesia:read({model, ModelId}) of
                [Model] ->
                    mnesia:write(Model#model{file_cid = CID});
                [] -> ok
            end
        end,
        mnesia:transaction(UpdateF),

        content_cache:delete({model_file, ModelId}),

        ok
    catch
        Exception:Error:Stacktrace ->
            error_logger:error_msg(
                "Exception while uploading model ~p to IPFS: ~p:~p~n~p",
                [ModelId, Exception, Error, Stacktrace]
            )
    end.

upload_model_update(ModelId) ->
    try
        FileContent = content_cache:get({model_update, ModelId}),

        CID = case byte_size(FileContent) of
            Size when Size > ?CHUNK_SIZE ->
                upload_large_file_chunked(FileContent);
            _ ->
                ipfs_media:upload_media(FileContent)
        end,

        UpdateF = fun() ->
            case mnesia:read({model, ModelId}) of
                [Model] ->
                    [{VersionNum, Version, _, Timestamp, Description} | RestVersions] = Model#model.version_history,
                    UpdatedVersionEntry = {VersionNum, Version, CID, Timestamp, Description},
                    UpdatedVersionHistory = [UpdatedVersionEntry | RestVersions],
                    mnesia:write(Model#model{
                        file_cid = CID,
                        version_history = UpdatedVersionHistory
                    });
                [] -> ok
            end
        end,
        mnesia:transaction(UpdateF),

        content_cache:delete({model_update, ModelId}),

        ok
    catch
        Exception:Error:Stacktrace ->
            error_logger:error_msg(
                "Exception while uploading model update ~p: ~p:~p~n~p",
                [ModelId, Exception, Error, Stacktrace]
            )
    end.

upload_model_file_to_ipfs(ModelId) ->
    try
        FileContent = content_cache:get({model_file_update, ModelId}),

        CID = case byte_size(FileContent) of
            Size when Size > ?CHUNK_SIZE ->
                upload_large_file_chunked(FileContent);
            _ ->
                ipfs_media:upload_media(FileContent)
        end,

        UpdateF = fun() ->
            case mnesia:read({model, ModelId}) of
                [Model] ->
                    [{VersionNum, Version, _, Timestamp, Description} | RestVersions] = Model#model.version_history,
                    UpdatedVersionEntry = {VersionNum, Version, CID, Timestamp, Description},
                    UpdatedVersionHistory = [UpdatedVersionEntry | RestVersions],
                    mnesia:write(Model#model{
                        file_cid = CID,
                        version_history = UpdatedVersionHistory
                    });
                [] -> ok
            end
        end,
        mnesia:transaction(UpdateF),

        content_cache:delete({model_file_update, ModelId}),

        ok
    catch
        Exception:Error:Stacktrace ->
            error_logger:error_msg(
                "Exception while uploading model file ~p: ~p:~p~n~p",
                [ModelId, Exception, Error, Stacktrace]
            )
    end.

upload_model_version(ModelId) ->
    try
        FileContent = content_cache:get({model_version, ModelId}),

        CID = case byte_size(FileContent) of
            Size when Size > ?CHUNK_SIZE ->
                upload_large_file_chunked(FileContent);
            _ ->
                ipfs_media:upload_media(FileContent)
        end,

        UpdateF = fun() ->
            case mnesia:read({model, ModelId}) of
                [Model] ->
                    [{VersionNum, Version, _, Timestamp, Description} | RestVersions] = Model#model.version_history,
                    UpdatedVersionEntry = {VersionNum, Version, CID, Timestamp, Description},
                    UpdatedVersionHistory = [UpdatedVersionEntry | RestVersions],
                    mnesia:write(Model#model{
                        file_cid = CID,
                        version_history = UpdatedVersionHistory
                    });
                [] -> ok
            end
        end,
        mnesia:transaction(UpdateF),

        content_cache:delete({model_version, ModelId}),

        ok
    catch
        Exception:Error:Stacktrace ->
            error_logger:error_msg(
                "Exception while uploading model version ~p: ~p:~p~n~p",
                [ModelId, Exception, Error, Stacktrace]
            )
    end.

upload_large_file_chunked(BinaryContent) ->
    Chunks = split_into_chunks(BinaryContent, ?CHUNK_SIZE),
    ChunkCIDs = [ipfs_media:upload_media(Chunk) || Chunk <- Chunks],

    Manifest = #{
        <<"type">> => <<"chunked">>,
        <<"chunks">> => ChunkCIDs,
        <<"total_size">> => byte_size(BinaryContent)
    },

    ManifestJSON = jsx:encode(Manifest),
    ipfs_content:upload_text(binary_to_list(ManifestJSON)).

split_into_chunks(Binary, ChunkSize) ->
    split_into_chunks(Binary, ChunkSize, []).

split_into_chunks(<<>>, _ChunkSize, Acc) ->
    lists:reverse(Acc);
split_into_chunks(Binary, ChunkSize, Acc) when byte_size(Binary) =< ChunkSize ->
    lists:reverse([Binary | Acc]);
split_into_chunks(Binary, ChunkSize, Acc) ->
    <<Chunk:ChunkSize/binary, Rest/binary>> = Binary,
    split_into_chunks(Rest, ChunkSize, [Chunk | Acc]).

generate_endpoint_url(ModelId) ->
    "https://api.zaryn.ai/inference/" ++ ModelId.

ensure_list(Value) when is_binary(Value) -> binary_to_list(Value);
ensure_list(Value) when is_list(Value) -> Value;
ensure_list(Value) when is_atom(Value) -> atom_to_list(Value);
ensure_list(Value) -> Value.

ensure_atom(Value) when is_binary(Value) -> binary_to_atom(Value, utf8);
ensure_atom(Value) when is_list(Value) -> list_to_atom(Value);
ensure_atom(Value) when is_atom(Value) -> Value.

parse_version({_, Version, _, _, _}) ->
    Parts = string:split(Version, ".", all),
    [Major, Minor, Patch] = lists:map(fun list_to_integer/1, Parts),
    {Major, Minor, Patch}.

format_version({Major, Minor, Patch}) ->
    lists:flatten(io_lib:format("~p.~p.~p", [Major, Minor, Patch])).

is_cid_ready({pending, _}) -> false;
is_cid_ready({pending_update, _}) -> false;
is_cid_ready({pending_version, _}) -> false;
is_cid_ready(undefined) -> false;
is_cid_ready(_) -> true.

receive_result({Pid, Ref}) ->
    receive
        {'DOWN', Ref, process, Pid, {result, Result}} ->
            Result;
        {'DOWN', Ref, process, Pid, Reason} ->
            error_logger:error_msg("Process ~p failed: ~p", [Pid, Reason]),
            exit({concurrent_operation_failed, Reason})
    after 30000 ->
        exit(Pid, kill),
        exit(timeout)
    end.

write_model_with_retry(Model, UserId, RetriesLeft) when RetriesLeft > 0 ->
    Fun = fun() ->
        mnesia:write(Model),

        case mnesia:read({user, UserId}) of
            [User] ->
                UpdatedModels = [Model#model.id | User#user.datasets],
                mnesia:write(User#user{datasets = UpdatedModels});
            [] ->
                error_logger:warning_msg("User ~p not found", [UserId])
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, _} ->
            ok;
        {aborted, Reason} ->
            error_logger:warning_msg("Model write failed (retries left: ~p): ~p",
                                   [RetriesLeft, Reason]),
            timer:sleep(?BACKOFF_TIME * ((?MAX_RETRIES - RetriesLeft) + 1)),
            write_model_with_retry(Model, UserId, RetriesLeft - 1)
    end;
write_model_with_retry(_Model, _UserId, 0) ->
    {error, max_retries_exceeded}.

    decode_model_id(EncodedId) when is_binary(EncodedId) ->
        decode_model_id(binary_to_list(EncodedId));

    decode_model_id(EncodedId) when is_list(EncodedId) ->

        case string:prefix(EncodedId, "id:") of
            nomatch ->
                Result = "id:" ++ EncodedId,
                Result;
            _ ->
                EncodedId
        end;

    decode_model_id(EncodedId) ->
        decode_model_id(to_string(EncodedId)).

    remove_all_prefixes(String, Prefix) ->
        case string:prefix(String, Prefix) of
            nomatch -> String;
            Rest -> remove_all_prefixes(Rest, Prefix)
        end.

        is_base64(String) ->
            Re = "^[A-Za-z0-9+/]+={0,2}$",
            case re:run(String, Re) of
                {match, _} -> true;
                _ -> false
            end.

        add_base64_padding(String) ->
            Len = length(String),
            case Len rem 4 of
                0 -> String;
                2 -> String ++ "==";
                3 -> String ++ "=";
                _ -> String
            end.

        to_string(Term) when is_binary(Term) -> binary_to_list(Term);
        to_string(Term) when is_list(Term) -> Term;
        to_string(Term) when is_atom(Term) -> atom_to_list(Term);
        to_string(Term) -> lists:flatten(io_lib:format("~p", [Term])).
