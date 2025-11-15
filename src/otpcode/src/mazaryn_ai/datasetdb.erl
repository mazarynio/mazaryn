-module(datasetdb).
-author("Zaryn Technologies").

-export([
    create_dataset/8,
    create_dataset_concurrent/8,
    create_dataset_from_file/7,
    create_dataset_from_zip/7,
    create_dataset_from_url/7,

    update_dataset/9,
    delete_dataset/2,
    upload_dataset_file/2,

    get_dataset_by_id/1,
    get_datasets_by_creator/1,
    get_public_datasets/0,
    get_datasets_by_tag/1,
    get_dataset_content/1,
    get_dataset_sample/1,
    get_dataset_schema/1,
    get_dataset_metadata/1,
    download_dataset/2,

    create_dataset_version/4,
    get_dataset_versions/1,
    get_version_by_number/2,
    rollback_to_version/3,

    request_dataset_access/3,
    approve_access_request/3,
    reject_access_request/3,
    add_collaborator/3,
    remove_collaborator/3,
    get_dataset_collaborators/1,

    rate_dataset/3,
    get_dataset_rating/1,
    calculate_quality_score/1,
    validate_dataset_schema/2,

    link_dataset_to_competition/2,
    unlink_dataset_from_competition/2,
    get_competitions_using_dataset/1,
    link_related_datasets/2,
    get_related_datasets/1,

    track_notebook_usage/2,
    track_model_usage/2,
    get_dataset_usage_stats/1,
    increment_download_count/1,

    search_datasets/1,
    search_datasets_advanced/1,
    get_trending_datasets/1,
    get_featured_datasets/0,

    generate_doi/1,
    increment_citation_count/1,
    get_citation_info/1,

    pin_dataset/1,
    unpin_dataset/1,
    update_pin_status/2,

    report_dataset/4,

    schedule_dataset_update/3,
    get_dataset_update_schedule/1,

    validate_dataset_file/1,
    get_supported_formats/0,
    extract_zip_metadata/1
]).

-include("../ml_records.hrl").
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("kernel/include/file.hrl").

-define(DEFAULT_QUALITY_SCORE, 50.0).
-define(SAMPLE_SIZE_LIMIT, 1000).
-define(MAX_RETRIES, 10).
-define(BACKOFF_TIME, 20).
-define(MAX_FILE_SIZE, 10737418240).
-define(SUPPORTED_FORMATS, [
    ".zip", ".csv", ".json", ".parquet", ".xlsx",
    ".txt", ".tsv", ".h5", ".hdf5", ".feather",
    ".pkl", ".pickle", ".npy", ".npz", ".tar.gz",
    ".sql", ".db", ".sqlite", ".arff"
]).
-define(CHUNK_SIZE, 10485760).

create_dataset(CreatorId, Title, Description, Content, MetadataMap, License, Tags, Visibility) ->
    Fun = fun() ->
        Id = nanoid:gen(),
        Now = calendar:universal_time(),

        ContentToCache = if
            is_binary(Content) -> binary_to_list(Content);
            true -> Content
        end,
        ok = content_cache:set({dataset, Id}, ContentToCache),

        SizeBytes = calculate_content_size(ContentToCache),

        Dataset = #dataset{
            id = Id,
            title = Title,
            description = Description,
            creator_id = CreatorId,
            content_cid = {pending, Id},
            metadata_cid = undefined,
            size_bytes = SizeBytes,
            license = License,
            version = "1.0.0",
            tags = Tags,
            visibility = Visibility,
            downloads = 0,
            ratings = [],
            pin_info = [],
            competition_ids = [],
            date_created = Now,
            date_updated = Now,
            report = [],
            metadata = MetadataMap,
            version_history = [{1, "1.0.0", {pending, Id}, Now, "Initial version"}],
            schema_cid = undefined,
            sample_cid = undefined,
            citation_count = 0,
            doi = undefined,
            related_dataset_ids = [],
            data_quality_score = ?DEFAULT_QUALITY_SCORE,
            update_frequency = static,
            access_requests = [],
            collaborators = [],
            used_in_notebook_ids = [],
            used_in_model_ids = []
        },

        mnesia:write(Dataset),

        case mnesia:read({user, CreatorId}) of
            [User] ->
                UserDatasets = User#user.datasets,
                UpdatedDatasets = case UserDatasets of
                    undefined -> [Id];
                    List when is_list(List) -> [Id | List];
                    _ -> [Id]
                end,
                mnesia:write(User#user{datasets = UpdatedDatasets});
            [] ->
                error_logger:warning_msg("User ~p not found when creating dataset", [CreatorId])
        end,

        {ok, Id, ContentToCache}
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, Id, CachedContent}} ->
            spawn(fun() ->
                upload_dataset_to_ipfs(Id, CachedContent, MetadataMap)
            end),
            Id;
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, {transaction_failed, Reason}}
    end.

create_dataset_concurrent(CreatorId, Title, Description, Content, MetadataMap, License, Tags, Visibility) ->
    IdFuture = spawn_monitor(fun() -> exit({result, nanoid:gen()}) end),
    Id = receive_result(IdFuture),

    Now = calendar:universal_time(),

    ContentToCache = if
        is_binary(Content) -> binary_to_list(Content);
        true -> Content
    end,

    CacheFuture = spawn_monitor(fun() ->
        ok = content_cache:set({dataset, Id}, ContentToCache),
        exit({result, ok})
    end),

    SizeFuture = spawn_monitor(fun() ->
        exit({result, calculate_content_size(ContentToCache)})
    end),

    receive_result(CacheFuture),
    SizeBytes = receive_result(SizeFuture),

    Dataset = #dataset{
        id = Id,
        title = Title,
        description = Description,
        creator_id = CreatorId,
        content_cid = {pending, Id},
        metadata_cid = undefined,
        size_bytes = SizeBytes,
        license = License,
        version = "1.0.0",
        tags = Tags,
        visibility = Visibility,
        downloads = 0,
        ratings = [],
        pin_info = [],
        competition_ids = [],
        date_created = Now,
        date_updated = Now,
        report = [],
        metadata = MetadataMap,
        version_history = [{1, "1.0.0", {pending, Id}, Now, "Initial version"}],
        schema_cid = undefined,
        sample_cid = undefined,
        citation_count = 0,
        doi = undefined,
        related_dataset_ids = [],
        data_quality_score = ?DEFAULT_QUALITY_SCORE,
        update_frequency = static,
        access_requests = [],
        collaborators = [],
        used_in_notebook_ids = [],
        used_in_model_ids = []
    },

    case write_dataset_with_retry(Dataset, CreatorId, ?MAX_RETRIES) of
        ok ->
            spawn(fun() ->
                upload_dataset_to_ipfs(Id, ContentToCache, MetadataMap)
            end),
            Id;
        {error, Reason} ->
            {error, Reason}
    end.

create_dataset_from_file(CreatorId, Title, Description, FilePath, License, Tags, Visibility) ->
    case validate_dataset_file(FilePath) of
        {error, Reason} ->
            {error, Reason};
        {ok, FileInfo} ->
            case file:read_file(FilePath) of
                {ok, FileContent} ->
                    Metadata = extract_file_metadata(FilePath, FileInfo, FileContent),

                    case filename:extension(FilePath) of
                        ".zip" ->
                            create_dataset_from_zip_content(
                                CreatorId, Title, Description,
                                FileContent, Metadata, License, Tags, Visibility
                            );
                        _ ->
                            create_dataset_from_content(
                                CreatorId, Title, Description,
                                FileContent, Metadata, License, Tags, Visibility
                            )
                    end;
                {error, Reason} ->
                    {error, {file_read_error, Reason}}
            end
    end.

create_dataset_from_zip(CreatorId, Title, Description, ZipFilePath, License, Tags, Visibility) ->
    case filename:extension(ZipFilePath) of
        ".zip" ->
            create_dataset_from_file(CreatorId, Title, Description, ZipFilePath, License, Tags, Visibility);
        _ ->
            {error, not_a_zip_file}
    end.

create_dataset_from_content(CreatorId, Title, Description, FileContent, Metadata, License, Tags, Visibility) ->
    Fun = fun() ->
        Id = nanoid:gen(),
        Now = calendar:universal_time(),

        ok = content_cache:set({dataset_file, Id}, FileContent),

        SizeBytes = byte_size(FileContent),

        Dataset = #dataset{
            id = Id,
            title = Title,
            description = Description,
            creator_id = CreatorId,
            content_cid = {pending, Id},
            metadata_cid = undefined,
            size_bytes = SizeBytes,
            license = License,
            version = "1.0.0",
            tags = Tags,
            visibility = Visibility,
            downloads = 0,
            ratings = [],
            pin_info = [],
            competition_ids = [],
            date_created = Now,
            date_updated = Now,
            report = [],
            metadata = Metadata,
            version_history = [{1, "1.0.0", {pending, Id}, Now, "Initial version"}],
            schema_cid = undefined,
            sample_cid = undefined,
            citation_count = 0,
            doi = undefined,
            related_dataset_ids = [],
            data_quality_score = ?DEFAULT_QUALITY_SCORE,
            update_frequency = static,
            access_requests = [],
            collaborators = [],
            used_in_notebook_ids = [],
            used_in_model_ids = []
        },

        mnesia:write(Dataset),

        case mnesia:read({user, CreatorId}) of
            [User] ->
                UserDatasets = case User#user.datasets of
                    undefined -> [Id];
                    List when is_list(List) -> [Id | List];
                    _ -> [Id]
                end,
                mnesia:write(User#user{datasets = UserDatasets});
            [] ->
                error_logger:warning_msg("User ~p not found when creating dataset", [CreatorId])
        end,

        {ok, Id}
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, Id}} ->
            spawn(fun() ->
                upload_binary_dataset_to_ipfs(Id, FileContent, Metadata)
            end),
            {ok, Id};
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, {transaction_failed, Reason}}
    end.

create_dataset_from_zip_content(CreatorId, Title, Description, ZipContent, Metadata, License, Tags, Visibility) ->
    case extract_zip_metadata(ZipContent) of
        {ok, ZipInfo} ->
            EnhancedMetadata = maps:merge(Metadata, #{
                zip_info => ZipInfo,
                is_archive => true,
                archive_type => zip
            }),

            create_dataset_from_content(
                CreatorId, Title, Description,
                ZipContent, EnhancedMetadata, License, Tags, Visibility
            );
        {error, Reason} ->
            error_logger:warning_msg("Failed to extract zip metadata: ~p", [Reason]),
            create_dataset_from_content(
                CreatorId, Title, Description,
                ZipContent, Metadata, License, Tags, Visibility
            )
    end.

create_dataset_from_url(CreatorId, Title, Description, Url, License, Tags, Visibility) ->
    case download_file_from_url(Url) of
        {ok, FileContent, ContentType} ->
            Extension = determine_extension(Url, ContentType),

            Metadata = #{
                source_url => Url,
                content_type => ContentType,
                format => Extension,
                downloaded_at => calendar:universal_time()
            },

            create_dataset_from_content(
                CreatorId, Title, Description,
                FileContent, Metadata, License, Tags, Visibility
            );
        {error, Reason} ->
            {error, {download_failed, Reason}}
    end.

update_dataset(DatasetId, CreatorId, NewTitle, NewDescription, NewContent, NewMetadata, NewLicense, NewTags, NewVisibility) ->
    Fun = fun() ->
        case mnesia:read({dataset, DatasetId}) of
            [] ->
                {error, dataset_not_found};
            [Dataset] ->
                case Dataset#dataset.creator_id of
                    CreatorId ->
                        Now = calendar:universal_time(),

                        {NewContentCID, NewSizeBytes} = case NewContent of
                            undefined ->
                                {Dataset#dataset.content_cid, Dataset#dataset.size_bytes};
                            _ ->
                                ContentToCache = if
                                    is_binary(NewContent) -> binary_to_list(NewContent);
                                    true -> NewContent
                                end,
                                content_cache:set({dataset_update, DatasetId}, ContentToCache),
                                Size = calculate_content_size(ContentToCache),
                                {{pending_update, DatasetId}, Size}
                        end,

                        {CurrentMajor, CurrentMinor, CurrentPatch} = parse_version(Dataset#dataset.version),
                        NewVersion = format_version({CurrentMajor, CurrentMinor, CurrentPatch + 1}),

                        VersionNum = length(Dataset#dataset.version_history) + 1,
                        NewVersionEntry = {VersionNum, NewVersion, NewContentCID, Now, "Updated dataset"},
                        UpdatedVersionHistory = [NewVersionEntry | Dataset#dataset.version_history],

                        UpdatedDataset = Dataset#dataset{
                            title = NewTitle,
                            description = NewDescription,
                            content_cid = NewContentCID,
                            metadata = NewMetadata,
                            license = NewLicense,
                            tags = NewTags,
                            visibility = NewVisibility,
                            version = NewVersion,
                            version_history = UpdatedVersionHistory,
                            size_bytes = NewSizeBytes,
                            date_updated = Now
                        },

                        mnesia:write(UpdatedDataset),

                        case NewContent of
                            undefined -> ok;
                            _ ->
                                spawn(fun() ->
                                    ContentToUpload = content_cache:get({dataset_update, DatasetId}),
                                    upload_dataset_update(DatasetId, ContentToUpload, NewMetadata)
                                end)
                        end,

                        ok;
                    _ ->
                        {error, unauthorized}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

delete_dataset(DatasetId, UserId) ->
    Fun = fun() ->
        case mnesia:read({dataset, DatasetId}) of
            [] ->
                {error, dataset_not_found};
            [Dataset] ->
                case Dataset#dataset.creator_id of
                    UserId ->
                        mnesia:delete({dataset, DatasetId}),

                        case mnesia:read({user, UserId}) of
                            [User] ->
                                UpdatedDatasets = lists:delete(DatasetId, User#user.datasets),
                                mnesia:write(User#user{datasets = UpdatedDatasets});
                            [] -> ok
                        end,

                        ok;
                    _ ->
                        {error, unauthorized}
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

upload_dataset_file(DatasetId, FilePath) ->
    case validate_dataset_file(FilePath) of
        {error, Reason} ->
            {error, Reason};
        {ok, FileInfo} ->
            case file:read_file(FilePath) of
                {ok, FileContent} ->
                    Fun = fun() ->
                        case mnesia:read({dataset, DatasetId}) of
                            [] ->
                                {error, dataset_not_found};
                            [Dataset] ->
                                Now = calendar:universal_time(),
                                SizeBytes = byte_size(FileContent),

                                ok = content_cache:set({dataset_file_update, DatasetId}, FileContent),

                                Metadata = extract_file_metadata(FilePath, FileInfo, FileContent),
                                UpdatedMetadata = maps:merge(Dataset#dataset.metadata, Metadata),

                                {Major, Minor, _Patch} = parse_version(Dataset#dataset.version),
                                NewVersion = format_version({Major, Minor + 1, 0}),

                                VersionNum = length(Dataset#dataset.version_history) + 1,
                                NewVersionEntry = {VersionNum, NewVersion, {pending_update, DatasetId}, Now, "File updated"},
                                UpdatedVersionHistory = [NewVersionEntry | Dataset#dataset.version_history],

                                UpdatedDataset = Dataset#dataset{
                                    content_cid = {pending_update, DatasetId},
                                    size_bytes = SizeBytes,
                                    metadata = UpdatedMetadata,
                                    version = NewVersion,
                                    version_history = UpdatedVersionHistory,
                                    date_updated = Now
                                },

                                mnesia:write(UpdatedDataset),

                                spawn(fun() ->
                                    upload_binary_dataset_to_ipfs(DatasetId, FileContent, UpdatedMetadata)
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

get_dataset_by_id(DatasetId) ->
    Fun = fun() ->
        case mnesia:read({dataset, DatasetId}) of
            [] -> {error, dataset_not_found};
            [Dataset] -> Dataset
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

get_datasets_by_creator(CreatorId) ->
    Fun = fun() ->
        mnesia:match_object(#dataset{creator_id = CreatorId, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_public_datasets() ->
    Fun = fun() ->
        mnesia:match_object(#dataset{visibility = public, _ = '_'})
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_datasets_by_tag(Tag) ->
    Fun = fun() ->
        AllDatasets = mnesia:match_object(#dataset{_ = '_'}),
        lists:filter(fun(Dataset) ->
            lists:member(Tag, Dataset#dataset.tags)
        end, AllDatasets)
    end,

    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_dataset_content(DatasetId) ->
    Fun = fun() ->
        case mnesia:read({dataset, DatasetId}) of
            [] -> {error, dataset_not_found};
            [Dataset] ->
                ContentCID = Dataset#dataset.content_cid,
                case ContentCID of
                    {pending, Id} when Id =:= DatasetId ->
                        case content_cache:get({dataset, Id}) of
                            undefined -> {error, content_not_ready};
                            CachedContent -> {ok, CachedContent}
                        end;
                    {pending_update, Id} when Id =:= DatasetId ->
                        case content_cache:get({dataset_update, Id}) of
                            undefined -> {error, content_not_ready};
                            CachedContent -> {ok, CachedContent}
                        end;
                    _ ->
                        try
                            ActualContent = ipfs_content:get_text_content(ContentCID),
                            {ok, ActualContent}
                        catch
                            _:Error -> {error, Error}
                        end
                end
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, {ok, Content}} -> Content;
        {atomic, {error, Reason}} -> {error, Reason};
        Error -> Error
    end.

get_dataset_sample(DatasetId) ->
    Fun = fun() ->
        case mnesia:read({dataset, DatasetId}) of
            [] -> {error, dataset_not_found};
            [Dataset] ->
                case Dataset#dataset.sample_cid of
                    undefined -> {error, sample_not_available};
                    SampleCID ->
                        try
                            SampleContent = ipfs_content:get_text_content(SampleCID),
                            {ok, SampleContent}
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

get_dataset_schema(DatasetId) ->
    Fun = fun() ->
        case mnesia:read({dataset, DatasetId}) of
            [] -> {error, dataset_not_found};
            [Dataset] ->
                case Dataset#dataset.schema_cid of
                    undefined -> {error, schema_not_available};
                    SchemaCID ->
                        try
                            SchemaContent = ipfs_content:get_text_content(SchemaCID),
                            {ok, SchemaContent}
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

get_dataset_metadata(DatasetId) ->
    Fun = fun() ->
        case mnesia:read({dataset, DatasetId}) of
            [] -> {error, dataset_not_found};
            [Dataset] -> {ok, Dataset#dataset.metadata}
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, {transaction_failed, Reason}}
    end.

download_dataset(DatasetId, _UserId) ->
    increment_download_count(DatasetId),
    get_dataset_content(DatasetId).

    create_dataset_version(DatasetId, UserId, NewContent, ChangeDescription) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] ->
                    {error, dataset_not_found};
                [Dataset] ->
                    IsCollaborator = lists:member(UserId, Dataset#dataset.collaborators),
                    IsCreator = Dataset#dataset.creator_id =:= UserId,

                    case IsCreator orelse IsCollaborator of
                        false ->
                            {error, unauthorized};
                        true ->
                            Now = calendar:universal_time(),

                            ContentToCache = if
                                is_binary(NewContent) -> binary_to_list(NewContent);
                                true -> NewContent
                            end,
                            content_cache:set({dataset_version, DatasetId}, ContentToCache),

                            {Major, Minor, _Patch} = parse_version(Dataset#dataset.version),
                            NewVersion = format_version({Major, Minor + 1, 0}),

                            VersionNum = length(Dataset#dataset.version_history) + 1,
                            NewVersionEntry = {VersionNum, NewVersion, {pending_version, DatasetId}, Now, ChangeDescription},
                            UpdatedVersionHistory = [NewVersionEntry | Dataset#dataset.version_history],

                            UpdatedDataset = Dataset#dataset{
                                content_cid = {pending_version, DatasetId},
                                version = NewVersion,
                                version_history = UpdatedVersionHistory,
                                date_updated = Now
                            },

                            mnesia:write(UpdatedDataset),

                            spawn(fun() ->
                                upload_dataset_version(DatasetId, ContentToCache)
                            end),

                            {ok, NewVersion}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_dataset_versions(DatasetId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] -> {ok, Dataset#dataset.version_history}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_version_by_number(DatasetId, VersionNum) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    case lists:keyfind(VersionNum, 1, Dataset#dataset.version_history) of
                        false -> {error, version_not_found};
                        VersionEntry -> {ok, VersionEntry}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    rollback_to_version(DatasetId, UserId, VersionNum) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] ->
                    {error, dataset_not_found};
                [Dataset] ->
                    case Dataset#dataset.creator_id of
                        UserId ->
                            case lists:keyfind(VersionNum, 1, Dataset#dataset.version_history) of
                                false ->
                                    {error, version_not_found};
                                {_, Version, CID, _, _} ->
                                    Now = calendar:universal_time(),
                                    UpdatedDataset = Dataset#dataset{
                                        content_cid = CID,
                                        version = Version,
                                        date_updated = Now
                                    },
                                    mnesia:write(UpdatedDataset),
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

    request_dataset_access(DatasetId, UserId, Reason) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    case Dataset#dataset.visibility of
                        public ->
                            {error, dataset_already_public};
                        private ->
                            RequestId = nanoid:gen(),
                            Now = calendar:universal_time(),
                            Request = {RequestId, UserId, Reason, pending, Now},
                            UpdatedRequests = [Request | Dataset#dataset.access_requests],
                            mnesia:write(Dataset#dataset{access_requests = UpdatedRequests}),
                            {ok, RequestId}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    approve_access_request(DatasetId, CreatorId, RequestId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    case Dataset#dataset.creator_id of
                        CreatorId ->
                            case lists:keyfind(RequestId, 1, Dataset#dataset.access_requests) of
                                false ->
                                    {error, request_not_found};
                                {RequestId, UserId, Reason, pending, Timestamp} ->
                                    UpdatedRequest = {RequestId, UserId, Reason, approved, Timestamp},
                                    UpdatedRequests = lists:keyreplace(RequestId, 1,
                                        Dataset#dataset.access_requests, UpdatedRequest),

                                    UpdatedCollaborators = [UserId | Dataset#dataset.collaborators],

                                    mnesia:write(Dataset#dataset{
                                        access_requests = UpdatedRequests,
                                        collaborators = UpdatedCollaborators
                                    }),
                                    ok;
                                _ ->
                                    {error, request_already_processed}
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

    reject_access_request(DatasetId, CreatorId, RequestId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    case Dataset#dataset.creator_id of
                        CreatorId ->
                            case lists:keyfind(RequestId, 1, Dataset#dataset.access_requests) of
                                false ->
                                    {error, request_not_found};
                                {RequestId, UserId, Reason, pending, Timestamp} ->
                                    UpdatedRequest = {RequestId, UserId, Reason, rejected, Timestamp},
                                    UpdatedRequests = lists:keyreplace(RequestId, 1,
                                        Dataset#dataset.access_requests, UpdatedRequest),
                                    mnesia:write(Dataset#dataset{access_requests = UpdatedRequests}),
                                    ok;
                                _ ->
                                    {error, request_already_processed}
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

    add_collaborator(DatasetId, CreatorId, CollaboratorId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    case Dataset#dataset.creator_id of
                        CreatorId ->
                            UpdatedCollaborators = case lists:member(CollaboratorId, Dataset#dataset.collaborators) of
                                true -> Dataset#dataset.collaborators;
                                false -> [CollaboratorId | Dataset#dataset.collaborators]
                            end,
                            mnesia:write(Dataset#dataset{collaborators = UpdatedCollaborators}),
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

    remove_collaborator(DatasetId, CreatorId, CollaboratorId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    case Dataset#dataset.creator_id of
                        CreatorId ->
                            UpdatedCollaborators = lists:delete(CollaboratorId, Dataset#dataset.collaborators),
                            mnesia:write(Dataset#dataset{collaborators = UpdatedCollaborators}),
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

    get_dataset_collaborators(DatasetId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] -> {ok, Dataset#dataset.collaborators}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    rate_dataset(DatasetId, UserId, Rating) when Rating >= 1, Rating =< 5 ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    Now = calendar:universal_time(),
                    UpdatedRatings = lists:keydelete(UserId, 1, Dataset#dataset.ratings),
                    NewRatings = [{UserId, Rating, Now} | UpdatedRatings],

                    mnesia:write(Dataset#dataset{ratings = NewRatings}),

                    spawn(fun() -> calculate_quality_score(DatasetId) end),

                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end;
    rate_dataset(_, _, _) ->
        {error, invalid_rating}.

    get_dataset_rating(DatasetId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    Ratings = Dataset#dataset.ratings,
                    case Ratings of
                        [] -> {ok, 0, 0.0};
                        _ ->
                            RatingValues = [Rating || {_, Rating, _} <- Ratings],
                            AvgRating = lists:sum(RatingValues) / length(RatingValues),
                            {ok, length(Ratings), AvgRating}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    calculate_quality_score(DatasetId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    Ratings = Dataset#dataset.ratings,
                    RatingScore = case Ratings of
                        [] -> 25.0;
                        _ ->
                            RatingValues = [Rating || {_, Rating, _} <- Ratings],
                            AvgRating = lists:sum(RatingValues) / length(RatingValues),
                            (AvgRating / 5.0) * 40.0
                    end,

                    Downloads = Dataset#dataset.downloads,
                    DownloadScore = min(20.0, math:log(Downloads + 1) * 4.0),

                    Citations = Dataset#dataset.citation_count,
                    CitationScore = min(20.0, math:log(Citations + 1) * 5.0),

                    HasSchema = case Dataset#dataset.schema_cid of
                        undefined -> 0;
                        _ -> 7
                    end,
                    HasSample = case Dataset#dataset.sample_cid of
                        undefined -> 0;
                        _ -> 7
                    end,
                    HasMetadata = case maps:size(Dataset#dataset.metadata) of
                        0 -> 0;
                        _ -> 6
                    end,
                    CompletenessScore = HasSchema + HasSample + HasMetadata,

                    TotalScore = RatingScore + DownloadScore + CitationScore + CompletenessScore,

                    mnesia:write(Dataset#dataset{data_quality_score = TotalScore}),
                    {ok, TotalScore}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    validate_dataset_schema(_DatasetId, _ExpectedSchema) ->
        {ok, valid}.

    link_dataset_to_competition(DatasetId, CompetitionId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    UpdatedCompetitions = case lists:member(CompetitionId, Dataset#dataset.competition_ids) of
                        true -> Dataset#dataset.competition_ids;
                        false -> [CompetitionId | Dataset#dataset.competition_ids]
                    end,
                    mnesia:write(Dataset#dataset{competition_ids = UpdatedCompetitions}),
                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    unlink_dataset_from_competition(DatasetId, CompetitionId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    UpdatedCompetitions = lists:delete(CompetitionId, Dataset#dataset.competition_ids),
                    mnesia:write(Dataset#dataset{competition_ids = UpdatedCompetitions}),
                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_competitions_using_dataset(DatasetId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] -> {ok, Dataset#dataset.competition_ids}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    link_related_datasets(DatasetId, RelatedDatasetId) ->
        Fun = fun() ->
            case {mnesia:read({dataset, DatasetId}), mnesia:read({dataset, RelatedDatasetId})} of
                {[], _} -> {error, dataset_not_found};
                {_, []} -> {error, related_dataset_not_found};
                {[Dataset], [RelatedDataset]} ->
                    UpdatedRelated1 = case lists:member(RelatedDatasetId, Dataset#dataset.related_dataset_ids) of
                        true -> Dataset#dataset.related_dataset_ids;
                        false -> [RelatedDatasetId | Dataset#dataset.related_dataset_ids]
                    end,

                    UpdatedRelated2 = case lists:member(DatasetId, RelatedDataset#dataset.related_dataset_ids) of
                        true -> RelatedDataset#dataset.related_dataset_ids;
                        false -> [DatasetId | RelatedDataset#dataset.related_dataset_ids]
                    end,

                    mnesia:write(Dataset#dataset{related_dataset_ids = UpdatedRelated1}),
                    mnesia:write(RelatedDataset#dataset{related_dataset_ids = UpdatedRelated2}),
                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_related_datasets(DatasetId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] -> {ok, Dataset#dataset.related_dataset_ids}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    track_notebook_usage(DatasetId, NotebookId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    UpdatedNotebooks = case lists:member(NotebookId, Dataset#dataset.used_in_notebook_ids) of
                        true -> Dataset#dataset.used_in_notebook_ids;
                        false -> [NotebookId | Dataset#dataset.used_in_notebook_ids]
                    end,
                    mnesia:write(Dataset#dataset{used_in_notebook_ids = UpdatedNotebooks}),
                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    track_model_usage(DatasetId, ModelId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    UpdatedModels = case lists:member(ModelId, Dataset#dataset.used_in_model_ids) of
                        true -> Dataset#dataset.used_in_model_ids;
                        false -> [ModelId | Dataset#dataset.used_in_model_ids]
                    end,
                    mnesia:write(Dataset#dataset{used_in_model_ids = UpdatedModels}),
                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_dataset_usage_stats(DatasetId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    {ok, #{
                        notebooks => length(Dataset#dataset.used_in_notebook_ids),
                        models => length(Dataset#dataset.used_in_model_ids),
                        competitions => length(Dataset#dataset.competition_ids),
                        downloads => Dataset#dataset.downloads,
                        citations => Dataset#dataset.citation_count
                    }}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    increment_download_count(DatasetId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    mnesia:write(Dataset#dataset{downloads = Dataset#dataset.downloads + 1}),
                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    search_datasets(Query) ->
        Fun = fun() ->
            AllDatasets = mnesia:match_object(#dataset{_ = '_'}),
            QueryLower = string:to_lower(Query),
            lists:filter(fun(Dataset) ->
                TitleMatch = string:find(string:to_lower(Dataset#dataset.title), QueryLower) =/= nomatch,
                DescMatch = string:find(string:to_lower(Dataset#dataset.description), QueryLower) =/= nomatch,
                TagMatch = lists:any(fun(Tag) ->
                    string:find(string:to_lower(Tag), QueryLower) =/= nomatch
                end, Dataset#dataset.tags),
                TitleMatch orelse DescMatch orelse TagMatch
            end, AllDatasets)
        end,

        {atomic, Res} = mnesia:transaction(Fun),
        Res.

    search_datasets_advanced(SearchParams) ->
        #{
            query := Query,
            tags := Tags,
            min_quality := MinQuality,
            license := License,
            visibility := Visibility
        } = SearchParams,

        Fun = fun() ->
            AllDatasets = mnesia:match_object(#dataset{_ = '_'}),
            QueryLower = string:to_lower(Query),

            lists:filter(fun(Dataset) ->
                TitleMatch = case Query of
                    "" -> true;
                    _ -> string:find(string:to_lower(Dataset#dataset.title), QueryLower) =/= nomatch
                end,

                TagMatch = case Tags of
                    [] -> true;
                    _ -> lists:any(fun(Tag) -> lists:member(Tag, Dataset#dataset.tags) end, Tags)
                end,

                QualityMatch = Dataset#dataset.data_quality_score >= MinQuality,

                LicenseMatch = case License of
                    any -> true;
                    _ -> Dataset#dataset.license =:= License
                end,

                VisibilityMatch = case Visibility of
                    any -> true;
                    _ -> Dataset#dataset.visibility =:= Visibility
                end,

                TitleMatch andalso TagMatch andalso QualityMatch andalso LicenseMatch andalso VisibilityMatch
            end, AllDatasets)
        end,

        {atomic, Res} = mnesia:transaction(Fun),
        Res.

    get_trending_datasets(Limit) ->
        Fun = fun() ->
            AllDatasets = mnesia:match_object(#dataset{visibility = public, _ = '_'}),
            Sorted = lists:sort(fun(A, B) ->
                ScoreA = A#dataset.downloads + (A#dataset.citation_count * 10),
                ScoreB = B#dataset.downloads + (B#dataset.citation_count * 10),
                ScoreA > ScoreB
            end, AllDatasets),
            lists:sublist(Sorted, Limit)
        end,

        {atomic, Res} = mnesia:transaction(Fun),
        Res.

    get_featured_datasets() ->
        Fun = fun() ->
            AllDatasets = mnesia:match_object(#dataset{visibility = public, _ = '_'}),
            Sorted = lists:sort(fun(A, B) ->
                A#dataset.data_quality_score > B#dataset.data_quality_score
            end, AllDatasets),
            lists:sublist(Sorted, 10)
        end,

        {atomic, Res} = mnesia:transaction(Fun),
        Res.

    generate_doi(DatasetId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    case Dataset#dataset.doi of
                        undefined ->
                            DOI = "10.5281/dataset." ++ DatasetId,
                            mnesia:write(Dataset#dataset{doi = DOI}),
                            {ok, DOI};
                        ExistingDOI ->
                            {ok, ExistingDOI}
                    end
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    increment_citation_count(DatasetId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    mnesia:write(Dataset#dataset{citation_count = Dataset#dataset.citation_count + 1}),
                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_citation_info(DatasetId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    {ok, #{
                        doi => Dataset#dataset.doi,
                        citation_count => Dataset#dataset.citation_count,
                        title => Dataset#dataset.title,
                        creator_id => Dataset#dataset.creator_id,
                        date_created => Dataset#dataset.date_created
                    }}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    pin_dataset(DatasetId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    ContentCID = Dataset#dataset.content_cid,
                    case is_cid_ready(ContentCID) of
                        false -> {error, content_not_ready};
                        true ->
                            spawn(fun() ->
                                try
                                    ipfs_client_5:pin_add([{arg, ContentCID}])
                                catch
                                    _:Error ->
                                        error_logger:error_msg("Failed to pin dataset ~p: ~p", [DatasetId, Error])
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

    unpin_dataset(DatasetId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    ContentCID = Dataset#dataset.content_cid,
                    case is_cid_ready(ContentCID) of
                        false -> {error, content_not_ready};
                        true ->
                            spawn(fun() ->
                                try
                                    ipfs_client_5:pin_rm([{arg, ContentCID}])
                                catch
                                    _:Error ->
                                        error_logger:error_msg("Failed to unpin dataset ~p: ~p", [DatasetId, Error])
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

    update_pin_status(DatasetId, PinInfo) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    mnesia:write(Dataset#dataset{pin_info = PinInfo}),
                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    report_dataset(ReporterId, DatasetId, Type, Description) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    ReportId = nanoid:gen(),
                    Now = calendar:universal_time(),
                    Report = #report{
                        id = ReportId,
                        type = Type,
                        description = Description,
                        reporter = ReporterId,
                        date_created = Now,
                        data = #{dataset_id => DatasetId}
                    },
                    mnesia:write(Report),

                    UpdatedReports = [ReportId | Dataset#dataset.report],
                    mnesia:write(Dataset#dataset{report = UpdatedReports}),

                    {ok, ReportId}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    schedule_dataset_update(DatasetId, Frequency, CronExpression) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    UpdatedMetadata = maps:put(schedule, #{
                        frequency => Frequency,
                        cron => CronExpression,
                        last_run => undefined
                    }, Dataset#dataset.metadata),

                    mnesia:write(Dataset#dataset{
                        update_frequency = Frequency,
                        metadata = UpdatedMetadata
                    }),
                    ok
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    get_dataset_update_schedule(DatasetId) ->
        Fun = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [] -> {error, dataset_not_found};
                [Dataset] ->
                    Schedule = maps:get(schedule, Dataset#dataset.metadata, undefined),
                    {ok, Schedule}
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, Result} -> Result;
            {aborted, Reason} -> {error, {transaction_failed, Reason}}
        end.

    validate_dataset_file(FilePath) ->
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
                                Extension = string:to_lower(filename:extension(FilePath)),
                                case lists:member(Extension, ?SUPPORTED_FORMATS) of
                                    true ->
                                        {ok, FileInfo};
                                    false ->
                                        {error, {unsupported_format, Extension}}
                                end
                        end;
                    {error, Reason} ->
                        {error, {file_info_error, Reason}}
                end
        end.

    get_supported_formats() ->
        ?SUPPORTED_FORMATS.

        extract_file_metadata(FilePath, FileInfo, FileContent) ->
            FileName = filename:basename(FilePath),
            Extension = string:to_lower(filename:extension(FilePath)),
            Size = FileInfo#file_info.size,
            ModTime = FileInfo#file_info.mtime,

            FormatStr = case Extension of
                [] -> "";
                [$.|Rest] -> Rest;
                Other -> Other
            end,

            BaseMetadata = #{
                filename => FileName,
                format => FormatStr,
                size_bytes => Size,
                modified_time => ModTime,
                file_type => determine_file_type(Extension)
            },

            FormatMetadata = case Extension of
                ".zip" ->
                    case extract_zip_metadata(FileContent) of
                        {ok, ZipInfo} -> ZipInfo;
                        _ -> #{}
                    end;
                ".csv" ->
                    extract_csv_metadata(FileContent);
                ".json" ->
                    extract_json_metadata(FileContent);
                ".xlsx" ->
                    #{spreadsheet => true};
                _ ->
                    #{}
            end,

            maps:merge(BaseMetadata, FormatMetadata).

            extract_zip_metadata(ZipContent) when is_binary(ZipContent) ->
                TempFile = "/tmp/temp_zip_" ++ integer_to_list(erlang:system_time()) ++ ".zip",
                try
                    ok = file:write_file(TempFile, ZipContent),
                    case zip:list_dir(TempFile) of
                        {ok, [_ZipComment | FileEntries]} ->
                            Files = lists:map(fun(Entry) ->
                                case Entry of
                                    {zip_file, Name, _Info, _Comment, _Offset, _CompSize} -> Name;
                                    _ -> "unknown"
                                end
                            end, FileEntries),

                            Sizes = lists:map(fun(Entry) ->
                                case Entry of
                                    {zip_file, _Name, Info, _Comment, _Offset, _CompSize}
                                      when is_record(Info, file_info) ->
                                        Info#file_info.size;
                                    _ -> 0
                                end
                            end, FileEntries),

                            TotalSize = lists:sum(Sizes),
                            file:delete(TempFile),

                            {ok, #{
                                file_count => length(Files),
                                files => Files,
                                total_uncompressed_size => TotalSize,
                                compression_ratio => calculate_compression_ratio(byte_size(ZipContent), TotalSize)
                            }};
                        {ok, []} ->
                            file:delete(TempFile),
                            {ok, #{file_count => 0, files => [], total_uncompressed_size => 0, compression_ratio => 0}};
                        {error, Reason} ->
                            file:delete(TempFile),
                            {error, Reason}
                    end
                catch
                    _:Error ->
                        catch file:delete(TempFile),
                        {error, {zip_parse_error, Error}}
                end;
            extract_zip_metadata(_) ->
                {error, invalid_zip_content}.

    extract_csv_metadata(CsvContent) when is_binary(CsvContent) ->
        Lines = binary:split(CsvContent, <<"\n">>, [global]),
        NonEmptyLines = [L || L <- Lines, byte_size(L) > 0],

        case NonEmptyLines of
            [] ->
                #{rows => 0};
            [Header | DataRows] ->
                Columns = binary:split(Header, <<",">>, [global]),

                #{
                    rows => length(DataRows),
                    columns => length(Columns),
                    column_names => [binary_to_list(C) || C <- Columns],
                    has_header => true
                }
        end;
    extract_csv_metadata(_) ->
        #{}.

    extract_json_metadata(JsonContent) when is_binary(JsonContent) ->
        try
            Parsed = jsx:decode(JsonContent, [return_maps]),

            case Parsed of
                List when is_list(List) ->
                    #{
                        type => array,
                        records => length(List),
                        structure => infer_json_structure(List)
                    };
                Map when is_map(Map) ->
                    #{
                        type => object,
                        keys => maps:keys(Map),
                        structure => infer_json_structure(Map)
                    };
                _ ->
                    #{type => primitive}
            end
        catch
            _:_ ->
                #{parse_error => true}
        end;
    extract_json_metadata(_) ->
        #{}.

    infer_json_structure([First | _Rest]) when is_map(First) ->
        maps:keys(First);
    infer_json_structure(Map) when is_map(Map) ->
        maps:keys(Map);
    infer_json_structure(_) ->
        unknown.

    determine_file_type(".csv") -> tabular;
    determine_file_type(".tsv") -> tabular;
    determine_file_type(".xlsx") -> tabular;
    determine_file_type(".json") -> structured;
    determine_file_type(".xml") -> structured;
    determine_file_type(".parquet") -> columnar;
    determine_file_type(".feather") -> columnar;
    determine_file_type(".h5") -> hierarchical;
    determine_file_type(".hdf5") -> hierarchical;
    determine_file_type(".zip") -> archive;
    determine_file_type(".tar.gz") -> archive;
    determine_file_type(".pkl") -> serialized;
    determine_file_type(".pickle") -> serialized;
    determine_file_type(".npy") -> array;
    determine_file_type(".npz") -> archive;
    determine_file_type(".sql") -> database;
    determine_file_type(".db") -> database;
    determine_file_type(".sqlite") -> database;
    determine_file_type(_) -> unknown.

    calculate_compression_ratio(CompressedSize, UncompressedSize) when UncompressedSize > 0 ->
        (CompressedSize / UncompressedSize) * 100;
    calculate_compression_ratio(_, _) ->
        0.

        determine_extension(Url, _ContentType) ->
            Extension = filename:extension(Url),
            case Extension of
                "" -> "bin";
                [$.|Rest] -> Rest;
                Ext -> Ext
            end.

    download_file_from_url(Url) ->
        try
            case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
                {ok, {{_Version, 200, _ReasonPhrase}, Headers, Body}} ->
                    ContentType = proplists:get_value("content-type", Headers, "application/octet-stream"),
                    {ok, Body, ContentType};
                {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, _Body}} ->
                    {error, {http_error, StatusCode}};
                {error, Reason} ->
                    {error, Reason}
            end
        catch
            _:Error ->
                {error, {download_exception, Error}}
        end.

    calculate_content_size(Content) when is_list(Content) ->
        length(Content);
    calculate_content_size(Content) when is_binary(Content) ->
        byte_size(Content);
    calculate_content_size(_) ->
        0.

    parse_version(VersionString) ->
        Parts = string:split(VersionString, ".", all),
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

    write_dataset_with_retry(Dataset, UserId, RetriesLeft) when RetriesLeft > 0 ->
        Fun = fun() ->
            mnesia:write(Dataset),

            case mnesia:read({user, UserId}) of
                [User] ->
                    UserDatasets = case User#user.datasets of
                        undefined -> [Dataset#dataset.id];
                        List when is_list(List) -> [Dataset#dataset.id | List];
                        _ -> [Dataset#dataset.id]
                    end,
                    mnesia:write(User#user{datasets = UserDatasets});
                [] ->
                    error_logger:warning_msg("User ~p not found", [UserId])
            end
        end,

        case mnesia:transaction(Fun) of
            {atomic, _} ->
                ok;
            {aborted, Reason} ->
                error_logger:warning_msg("Dataset write failed (retries left: ~p): ~p",
                                       [RetriesLeft, Reason]),
                timer:sleep(?BACKOFF_TIME * ((?MAX_RETRIES - RetriesLeft) + 1)),
                write_dataset_with_retry(Dataset, UserId, RetriesLeft - 1)
        end;
write_dataset_with_retry(_Dataset, _UserId, 0) ->
    {error, max_retries_exceeded}.

upload_dataset_to_ipfs(DatasetId, Content, Metadata) ->
    try
        ContentCID = ipfs_content:upload_text(Content),

        Sample = generate_sample(Content, ?SAMPLE_SIZE_LIMIT),
        SampleCID = case Sample of
            "" -> undefined;
            _ -> ipfs_content:upload_text(Sample)
        end,

        Schema = generate_schema(Content),
        SchemaCID = case Schema of
            "" -> undefined;
            _ -> ipfs_content:upload_text(Schema)
        end,

        MetadataJSON = jsx:encode(Metadata),
        MetadataCID = ipfs_content:upload_text(binary_to_list(MetadataJSON)),

        {ok, #{id := _KeyID, name := _BinID}} = ipfs_client_4:key_gen("dataset_" ++ DatasetId),

        PublishOptions = [
            {key, "dataset_" ++ DatasetId},
            {resolve, false},
            {lifetime, "8760h0m0s"},
            {ttl, "24h0m0s"},
            {v1compat, true},
            {ipns_base, "base36"},
            {quieter, true},
            {'allow-offline', true}
        ],

        {ok, #{name := IPNSKey}} = ipfs_client_5:name_publish(
            "/ipfs/" ++ ContentCID,
            PublishOptions
        ),

        UpdateF = fun() ->
            case mnesia:read({dataset, DatasetId}) of
                [Dataset] ->
                    UpdatedDataset = Dataset#dataset{
                        content_cid = ContentCID,
                        ipns = IPNSKey,
                        sample_cid = SampleCID,
                        schema_cid = SchemaCID,
                        metadata_cid = MetadataCID
                    },
                    mnesia:write(UpdatedDataset);
                [] -> ok
            end
        end,
        mnesia:transaction(UpdateF),

        content_cache:delete({dataset, DatasetId}),

        ok
    catch
        Exception:Error:Stacktrace ->
            error_logger:error_msg(
                "Exception while uploading dataset ~p to IPFS: ~p:~p~n~p",
                [DatasetId, Exception, Error, Stacktrace]
            )
    end.

upload_dataset_update(DatasetId, Content, Metadata) ->
    upload_dataset_to_ipfs(DatasetId, Content, Metadata).

    upload_dataset_version(DatasetId, Content) ->
        try
            ContentCID = ipfs_content:upload_text(Content),

            UpdateF = fun() ->
                case mnesia:read({dataset, DatasetId}) of
                    [Dataset] ->
                        [{VersionNum, Version, _, Timestamp, Description} | RestVersions] = Dataset#dataset.version_history,
                        UpdatedVersionEntry = {VersionNum, Version, ContentCID, Timestamp, Description},
                        UpdatedVersionHistory = [UpdatedVersionEntry | RestVersions],

                        UpdatedDataset = Dataset#dataset{
                            content_cid = ContentCID,
                            version_history = UpdatedVersionHistory
                        },
                        mnesia:write(UpdatedDataset);
                    [] -> ok
                end
            end,
            mnesia:transaction(UpdateF),

            content_cache:delete({dataset_version, DatasetId}),

            ok
        catch
            Exception:Error:Stacktrace ->
                error_logger:error_msg(
                    "Exception while uploading dataset version ~p: ~p:~p~n~p",
                    [DatasetId, Exception, Error, Stacktrace]
                )
        end.

        upload_binary_dataset_to_ipfs(DatasetId, BinaryContent, Metadata) ->
            try
                Size = byte_size(BinaryContent),

                ContentCID = if
                    Size > ?CHUNK_SIZE ->
                        upload_large_file_chunked(BinaryContent);
                    true ->
                        ipfs_media:upload_media(BinaryContent)
                end,

                SampleCID = generate_sample_from_binary(BinaryContent, Metadata),

                MetadataJSON = jsx:encode(Metadata),
                MetadataCID = ipfs_content:upload_text(binary_to_list(MetadataJSON)),

                UpdateF = fun() ->
                    case mnesia:read({dataset, DatasetId}) of
                        [Dataset] ->
                            UpdatedDataset = Dataset#dataset{
                                content_cid = ContentCID,
                                sample_cid = SampleCID,
                                metadata_cid = MetadataCID
                            },
                            mnesia:write(UpdatedDataset);
                        [] -> ok
                    end
                end,
                mnesia:transaction(UpdateF),

                content_cache:delete({dataset_file, DatasetId}),
                content_cache:delete({dataset_file_update, DatasetId}),

                spawn(fun() ->
                    timer:sleep(15000),

                    case ContentCID of
                        undefined ->
                            error_logger:info_msg("No content to publish to IPNS for dataset ~p", [DatasetId]);
                        "" ->
                            error_logger:info_msg("Empty content, skipping IPNS publish for dataset ~p", [DatasetId]);
                        _ ->
                            try
                                KeyName = "dataset_" ++ DatasetId,
                                {ok, #{id := _KeyID, name := _BinID}} = ipfs_client_4:key_gen(KeyName),

                                PublishOptions = [
                                    {key, KeyName},
                                    {resolve, false},
                                    {lifetime, "8760h0m0s"},
                                    {ttl, "24h0m0s"},
                                    {v1compat, true},
                                    {ipns_base, "base36"},
                                    {quieter, true},
                                    {'allow-offline', true}
                                ],

                                case ipfs_client_5:name_publish(
                                    "/ipfs/" ++ ContentCID,
                                    PublishOptions
                                ) of
                                    {ok, #{name := IPNSKey}} ->
                                        update_dataset_ipns(DatasetId, IPNSKey);
                                    {error, _Reason} ->
                                        error_logger:error_msg("IPNS publish failed for dataset ~p: ~p", [DatasetId, _Reason]),
                                        err
                                end
                            catch
                                Exception:Error:Stacktrace ->
                                    error_logger:error_msg(
                                        "Exception while publishing to IPNS for dataset ~p: ~p:~p~n~p",
                                        [DatasetId, Exception, Error, Stacktrace]
                                    )
                            end
                    end
                end),

                ok
            catch
                Exception:Error:Stacktrace ->
                    error_logger:error_msg(
                        "Exception while uploading binary dataset ~p to IPFS: ~p:~p~n~p",
                        [DatasetId, Exception, Error, Stacktrace]
                    )
            end.

        update_dataset_ipns(DatasetId, IPNSKey) ->
            UpdateF = fun() ->
                case mnesia:read({dataset, DatasetId}) of
                    [Dataset] ->
                        UpdatedDataset = Dataset#dataset{ipns = IPNSKey},
                        mnesia:write(UpdatedDataset),
                        ok;
                    [] ->
                        {error, not_found}
                end
            end,

            case mnesia:transaction(UpdateF) of
                {atomic, ok} -> ok;
                {atomic, {error, Reason}} ->
                    error_logger:error_msg("Failed to update dataset ~p with IPNS: ~p", [DatasetId, Reason]),
                    {error, Reason};
                {aborted, Reason} ->
                    error_logger:error_msg("Transaction aborted while updating dataset ~p with IPNS: ~p", [DatasetId, Reason]),
                    {error, {transaction_failed, Reason}}
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

    generate_sample_from_binary(BinaryContent, Metadata) ->
        Format = maps:get(format, Metadata, unknown),

        case Format of
            "csv" ->
                Sample = generate_sample(binary_to_list(BinaryContent), 1000),
                ipfs_content:upload_text(Sample);
            "json" ->
                try
                    Parsed = jsx:decode(BinaryContent, [return_maps]),
                    case Parsed of
                        List when is_list(List) ->
                            SampleList = lists:sublist(List, 100),
                            SampleJSON = jsx:encode(SampleList),
                            ipfs_content:upload_text(binary_to_list(SampleJSON));
                        _ ->
                            undefined
                    end
                catch
                    _:_ -> undefined
                end;
            _ ->
                undefined
        end.

    generate_sample(Content, Limit) ->
        Lines = string:split(Content, "\n", all),
        SampleLines = lists:sublist(Lines, Limit),
        string:join(SampleLines, "\n").

    generate_schema(_Content) ->
        "".
