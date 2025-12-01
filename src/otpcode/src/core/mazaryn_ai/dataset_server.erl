-module(dataset_server).
-author("Zaryn Technologies").
-include_lib("kernel/include/logger.hrl").
-export([
    start_link/0,
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
    extract_zip_metadata/1,
    download_dataset/2,
    download_dataset/3,
    download_dataset_with_wait/2,
    download_dataset_with_wait/3,
    schedule_dataset_download/3,
    get_dataset_download_status/1,
    cancel_dataset_download/1,
    download_dataset_to_browser/2,
    download_dataset_to_browser/3,
    get_dataset_download_url/1,
    debug_dataset_info/1,
    list_all_datasets_debug/0,
    get_dataset_file_cid/1,
    upload_dataset_file_to_ipfs/3,
    get_dataset_cid/1,
    download_dataset_async/2,
    download_dataset_async/3,
    fetch_from_ipfs/1,
    get_dataset_zip_by_id/2,
    get_dataset_zip_by_cid/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {worker_pool_size = 100, worker_pool = []}).
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).
create_dataset(CreatorId, Title, Description, Content, MetadataMap, License, Tags, Visibility) ->
    gen_server:call({global, ?MODULE}, {create_dataset, CreatorId, Title, Description, Content, MetadataMap, License, Tags, Visibility}).
create_dataset_concurrent(CreatorId, Title, Description, Content, MetadataMap, License, Tags, Visibility) ->
    RequestId = make_ref(),
    gen_server:cast({global, ?MODULE}, {create_dataset_concurrent, CreatorId, Title, Description, Content, MetadataMap, License, Tags, Visibility, {self(), RequestId}}),
    receive
        {dataset_creation_result, RequestId, Result} -> Result
    after 45000 ->
        {error, timeout}
    end.
create_dataset_from_file(CreatorId, Title, Description, FilePath, License, Tags, Visibility) ->
    gen_server:call({global, ?MODULE}, {create_dataset_from_file, CreatorId, Title, Description, FilePath, License, Tags, Visibility}, 60000).
create_dataset_from_zip(CreatorId, Title, Description, ZipFilePath, License, Tags, Visibility) ->
    gen_server:call({global, ?MODULE}, {create_dataset_from_zip, CreatorId, Title, Description, ZipFilePath, License, Tags, Visibility}, 60000).
create_dataset_from_url(CreatorId, Title, Description, Url, License, Tags, Visibility) ->
    gen_server:call({global, ?MODULE}, {create_dataset_from_url, CreatorId, Title, Description, Url, License, Tags, Visibility}, 60000).
update_dataset(DatasetId, CreatorId, NewTitle, NewDescription, NewContent, NewMetadata, NewLicense, NewTags, NewVisibility) ->
    gen_server:call({global, ?MODULE}, {update_dataset, DatasetId, CreatorId, NewTitle, NewDescription, NewContent, NewMetadata, NewLicense, NewTags, NewVisibility}).
delete_dataset(DatasetId, UserId) ->
    gen_server:call({global, ?MODULE}, {delete_dataset, DatasetId, UserId}).
upload_dataset_file(DatasetId, FilePath) ->
    gen_server:call({global, ?MODULE}, {upload_dataset_file, DatasetId, FilePath}, 60000).
get_dataset_by_id(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_by_id, DatasetId}).
get_datasets_by_creator(CreatorId) ->
    gen_server:call({global, ?MODULE}, {get_datasets_by_creator, CreatorId}).
get_public_datasets() ->
    gen_server:call({global, ?MODULE}, {get_public_datasets}).
get_datasets_by_tag(Tag) ->
    gen_server:call({global, ?MODULE}, {get_datasets_by_tag, Tag}).
get_dataset_content(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_content, DatasetId}).
get_dataset_sample(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_sample, DatasetId}).
get_dataset_schema(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_schema, DatasetId}).
get_dataset_metadata(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_metadata, DatasetId}).
create_dataset_version(DatasetId, UserId, NewContent, ChangeDescription) ->
    gen_server:call({global, ?MODULE}, {create_dataset_version, DatasetId, UserId, NewContent, ChangeDescription}, 60000).
get_dataset_versions(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_versions, DatasetId}).
get_version_by_number(DatasetId, VersionNum) ->
    gen_server:call({global, ?MODULE}, {get_version_by_number, DatasetId, VersionNum}).
rollback_to_version(DatasetId, UserId, VersionNum) ->
    gen_server:call({global, ?MODULE}, {rollback_to_version, DatasetId, UserId, VersionNum}).
request_dataset_access(DatasetId, UserId, Reason) ->
    gen_server:call({global, ?MODULE}, {request_dataset_access, DatasetId, UserId, Reason}).
approve_access_request(DatasetId, CreatorId, RequestId) ->
    gen_server:call({global, ?MODULE}, {approve_access_request, DatasetId, CreatorId, RequestId}).
reject_access_request(DatasetId, CreatorId, RequestId) ->
    gen_server:call({global, ?MODULE}, {reject_access_request, DatasetId, CreatorId, RequestId}).
add_collaborator(DatasetId, CreatorId, CollaboratorId) ->
    gen_server:call({global, ?MODULE}, {add_collaborator, DatasetId, CreatorId, CollaboratorId}).
remove_collaborator(DatasetId, CreatorId, CollaboratorId) ->
    gen_server:call({global, ?MODULE}, {remove_collaborator, DatasetId, CreatorId, CollaboratorId}).
get_dataset_collaborators(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_collaborators, DatasetId}).
rate_dataset(DatasetId, UserId, Rating) ->
    gen_server:call({global, ?MODULE}, {rate_dataset, DatasetId, UserId, Rating}).
get_dataset_rating(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_rating, DatasetId}).
calculate_quality_score(DatasetId) ->
    gen_server:call({global, ?MODULE}, {calculate_quality_score, DatasetId}).
validate_dataset_schema(DatasetId, ExpectedSchema) ->
    gen_server:call({global, ?MODULE}, {validate_dataset_schema, DatasetId, ExpectedSchema}).
link_dataset_to_competition(DatasetId, CompetitionId) ->
    gen_server:call({global, ?MODULE}, {link_dataset_to_competition, DatasetId, CompetitionId}).
unlink_dataset_from_competition(DatasetId, CompetitionId) ->
    gen_server:call({global, ?MODULE}, {unlink_dataset_from_competition, DatasetId, CompetitionId}).
get_competitions_using_dataset(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_competitions_using_dataset, DatasetId}).
link_related_datasets(DatasetId, RelatedDatasetId) ->
    gen_server:call({global, ?MODULE}, {link_related_datasets, DatasetId, RelatedDatasetId}).
get_related_datasets(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_related_datasets, DatasetId}).
track_notebook_usage(DatasetId, NotebookId) ->
    gen_server:call({global, ?MODULE}, {track_notebook_usage, DatasetId, NotebookId}).
track_model_usage(DatasetId, ModelId) ->
    gen_server:call({global, ?MODULE}, {track_model_usage, DatasetId, ModelId}).
get_dataset_usage_stats(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_usage_stats, DatasetId}).
increment_download_count(DatasetId) ->
    gen_server:call({global, ?MODULE}, {increment_download_count, DatasetId}).
search_datasets(Query) ->
    gen_server:call({global, ?MODULE}, {search_datasets, Query}).
search_datasets_advanced(SearchParams) ->
    gen_server:call({global, ?MODULE}, {search_datasets_advanced, SearchParams}).
get_trending_datasets(Limit) ->
    gen_server:call({global, ?MODULE}, {get_trending_datasets, Limit}).
get_featured_datasets() ->
    gen_server:call({global, ?MODULE}, {get_featured_datasets}).
generate_doi(DatasetId) ->
    gen_server:call({global, ?MODULE}, {generate_doi, DatasetId}).
increment_citation_count(DatasetId) ->
    gen_server:call({global, ?MODULE}, {increment_citation_count, DatasetId}).
get_citation_info(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_citation_info, DatasetId}).
pin_dataset(DatasetId) ->
    gen_server:call({global, ?MODULE}, {pin_dataset, DatasetId}, 30000).
unpin_dataset(DatasetId) ->
    gen_server:call({global, ?MODULE}, {unpin_dataset, DatasetId}, 30000).
update_pin_status(DatasetId, PinInfo) ->
    gen_server:call({global, ?MODULE}, {update_pin_status, DatasetId, PinInfo}).
report_dataset(ReporterId, DatasetId, Type, Description) ->
    gen_server:call({global, ?MODULE}, {report_dataset, ReporterId, DatasetId, Type, Description}).
schedule_dataset_update(DatasetId, Frequency, CronExpression) ->
    gen_server:call({global, ?MODULE}, {schedule_dataset_update, DatasetId, Frequency, CronExpression}).
get_dataset_update_schedule(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_update_schedule, DatasetId}).
validate_dataset_file(FilePath) ->
    gen_server:call({global, ?MODULE}, {validate_dataset_file, FilePath}).
get_supported_formats() ->
    gen_server:call({global, ?MODULE}, {get_supported_formats}).
extract_zip_metadata(ZipContent) ->
    gen_server:call({global, ?MODULE}, {extract_zip_metadata, ZipContent}).
download_dataset(DatasetId, UserId) ->
    gen_server:call({global, ?MODULE}, {download_dataset, DatasetId, UserId}, 60000).
download_dataset(DatasetId, UserId, Options) ->
    gen_server:call({global, ?MODULE}, {download_dataset, DatasetId, UserId, Options}, 60000).
download_dataset_with_wait(DatasetId, UserId) ->
    gen_server:call({global, ?MODULE}, {download_dataset_with_wait, DatasetId, UserId}, 60000).
download_dataset_with_wait(DatasetId, UserId, Options) ->
    gen_server:call({global, ?MODULE}, {download_dataset_with_wait, DatasetId, UserId, Options}, 60000).
schedule_dataset_download(DatasetId, UserId, ScheduleTime) ->
    gen_server:call({global, ?MODULE}, {schedule_dataset_download, DatasetId, UserId, ScheduleTime}).
get_dataset_download_status(DownloadId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_download_status, DownloadId}).
cancel_dataset_download(DownloadId) ->
    gen_server:call({global, ?MODULE}, {cancel_dataset_download, DownloadId}).
download_dataset_to_browser(DatasetId, UserId) ->
    gen_server:call({global, ?MODULE}, {download_dataset_to_browser, DatasetId, UserId}, 60000).
download_dataset_to_browser(DatasetId, UserId, Options) ->
    gen_server:call({global, ?MODULE}, {download_dataset_to_browser, DatasetId, UserId, Options}, 60000).
get_dataset_download_url(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_download_url, DatasetId}).
debug_dataset_info(DatasetId) ->
    gen_server:call({global, ?MODULE}, {debug_dataset_info, DatasetId}).
list_all_datasets_debug() ->
    gen_server:call({global, ?MODULE}, {list_all_datasets_debug}).
get_dataset_file_cid(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_file_cid, DatasetId}).
upload_dataset_file_to_ipfs(DatasetId, FileContent, Metadata) ->
    gen_server:call({global, ?MODULE}, {upload_dataset_file_to_ipfs, DatasetId, FileContent, Metadata}, 60000).
get_dataset_cid(DatasetId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_cid, DatasetId}).
download_dataset_async(DatasetId, UserId) ->
    gen_server:call({global, ?MODULE}, {download_dataset_async, DatasetId, UserId}).
download_dataset_async(DatasetId, UserId, Options) ->
    gen_server:call({global, ?MODULE}, {download_dataset_async, DatasetId, UserId, Options}).
fetch_from_ipfs(CID) ->
    gen_server:call({global, ?MODULE}, {fetch_from_ipfs, CID}, 60000).
get_dataset_zip_by_id(DatasetId, UserId) ->
    gen_server:call({global, ?MODULE}, {get_dataset_zip_by_id, DatasetId, UserId}).
get_dataset_zip_by_cid(CID) ->
    gen_server:call({global, ?MODULE}, {get_dataset_zip_by_cid, CID}).
init([]) ->
    ?LOG_NOTICE("Dataset server has been started - ~p", [self()]),
    PoolSize = application:get_env(social_network, dataset_worker_pool_size, 100),
    WorkerPool = initialize_worker_pool(PoolSize),
    {ok, #state{worker_pool_size = PoolSize, worker_pool = WorkerPool}}.
initialize_worker_pool(Size) ->
    [spawn_link(fun() -> worker_loop() end) || _ <- lists:seq(1, Size)].
worker_loop() ->
    receive
        {create_dataset, CreatorId, Title, Description, Content, MetadataMap, License, Tags, Visibility, From} ->
            Result = datasetdb:create_dataset(CreatorId, Title, Description, Content, MetadataMap, License, Tags, Visibility),
            gen_server:reply(From, Result),
            worker_loop();
        {create_dataset_concurrent, CreatorId, Title, Description, Content, MetadataMap, License, Tags, Visibility, {Pid, RequestId}} ->
            Result = datasetdb:create_dataset_concurrent(CreatorId, Title, Description, Content, MetadataMap, License, Tags, Visibility),
            Pid ! {dataset_creation_result, RequestId, Result},
            worker_loop();
        {create_dataset_from_file, CreatorId, Title, Description, FilePath, License, Tags, Visibility, From} ->
            Result = datasetdb:create_dataset_from_file(CreatorId, Title, Description, FilePath, License, Tags, Visibility),
            gen_server:reply(From, Result),
            worker_loop();
        {create_dataset_from_zip, CreatorId, Title, Description, ZipFilePath, License, Tags, Visibility, From} ->
            Result = datasetdb:create_dataset_from_zip(CreatorId, Title, Description, ZipFilePath, License, Tags, Visibility),
            gen_server:reply(From, Result),
            worker_loop();
        {create_dataset_from_url, CreatorId, Title, Description, Url, License, Tags, Visibility, From} ->
            Result = datasetdb:create_dataset_from_url(CreatorId, Title, Description, Url, License, Tags, Visibility),
            gen_server:reply(From, Result),
            worker_loop();
        {upload_dataset_file, DatasetId, FilePath, From} ->
            Result = datasetdb:upload_dataset_file(DatasetId, FilePath),
            gen_server:reply(From, Result),
            worker_loop();
        {create_dataset_version, DatasetId, UserId, NewContent, ChangeDescription, From} ->
            Result = datasetdb:create_dataset_version(DatasetId, UserId, NewContent, ChangeDescription),
            gen_server:reply(From, Result),
            worker_loop();
        {pin_dataset, DatasetId, From} ->
            Result = datasetdb:pin_dataset(DatasetId),
            gen_server:reply(From, Result),
            worker_loop();
        {unpin_dataset, DatasetId, From} ->
            Result = datasetdb:unpin_dataset(DatasetId),
            gen_server:reply(From, Result),
            worker_loop();
        {upload_dataset_file_to_ipfs, DatasetId, FileContent, Metadata, From} ->
            Result = datasetdb:upload_dataset_file_to_ipfs(DatasetId, FileContent, Metadata),
            gen_server:reply(From, Result),
            worker_loop();
        {fetch_from_ipfs, CID, From} ->
            Result = datasetdb:fetch_from_ipfs(CID),
            gen_server:reply(From, Result),
            worker_loop();
        {download_dataset, DatasetId, UserId, Options, From} ->
            Result = datasetdb:download_dataset(DatasetId, UserId, Options),
            gen_server:reply(From, Result),
            worker_loop();
        {download_dataset_with_wait, DatasetId, UserId, Options, From} ->
            Result = datasetdb:download_dataset_with_wait(DatasetId, UserId, Options),
            gen_server:reply(From, Result),
            worker_loop();
        {download_dataset_to_browser, DatasetId, UserId, Options, From} ->
            Result = datasetdb:download_dataset_to_browser(DatasetId, UserId, Options),
            gen_server:reply(From, Result),
            worker_loop();
        {download_dataset_async, DatasetId, UserId, Options, From} ->
            Result = datasetdb:download_dataset_async(DatasetId, UserId, Options),
            gen_server:reply(From, Result),
            worker_loop();
        {get_dataset_zip_by_id, DatasetId, UserId, From} ->
            Result = datasetdb:get_dataset_zip_by_id(DatasetId, UserId),
            gen_server:reply(From, Result),
            worker_loop();
        {get_dataset_zip_by_cid, CID, From} ->
            Result = datasetdb:get_dataset_zip_by_cid(CID),
            gen_server:reply(From, Result),
            worker_loop();
        {stop, From} ->
            From ! {stopped, self()};
        Other ->
            ?LOG_WARNING("Worker received unknown message: ~p", [Other]),
            worker_loop()
    end.
handle_call({create_dataset, CreatorId, Title, Description, Content, MetadataMap, License, Tags, Visibility}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_dataset, CreatorId, Title, Description, Content, MetadataMap, License, Tags, Visibility, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({create_dataset_from_file, CreatorId, Title, Description, FilePath, License, Tags, Visibility}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_dataset_from_file, CreatorId, Title, Description, FilePath, License, Tags, Visibility, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({create_dataset_from_zip, CreatorId, Title, Description, ZipFilePath, License, Tags, Visibility}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_dataset_from_zip, CreatorId, Title, Description, ZipFilePath, License, Tags, Visibility, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({create_dataset_from_url, CreatorId, Title, Description, Url, License, Tags, Visibility}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_dataset_from_url, CreatorId, Title, Description, Url, License, Tags, Visibility, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({upload_dataset_file, DatasetId, FilePath}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {upload_dataset_file, DatasetId, FilePath, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({create_dataset_version, DatasetId, UserId, NewContent, ChangeDescription}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_dataset_version, DatasetId, UserId, NewContent, ChangeDescription, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({pin_dataset, DatasetId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {pin_dataset, DatasetId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({unpin_dataset, DatasetId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {unpin_dataset, DatasetId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({upload_dataset_file_to_ipfs, DatasetId, FileContent, Metadata}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {upload_dataset_file_to_ipfs, DatasetId, FileContent, Metadata, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({fetch_from_ipfs, CID}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {fetch_from_ipfs, CID, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({download_dataset, DatasetId, UserId, Options}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {download_dataset, DatasetId, UserId, Options, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({download_dataset_with_wait, DatasetId, UserId, Options}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {download_dataset_with_wait, DatasetId, UserId, Options, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({download_dataset_to_browser, DatasetId, UserId, Options}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {download_dataset_to_browser, DatasetId, UserId, Options, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({download_dataset_async, DatasetId, UserId, Options}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {download_dataset_async, DatasetId, UserId, Options, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_dataset_zip_by_id, DatasetId, UserId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {get_dataset_zip_by_id, DatasetId, UserId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_dataset_zip_by_cid, CID}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {get_dataset_zip_by_cid, CID, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({update_dataset, DatasetId, CreatorId, NewTitle, NewDescription, NewContent, NewMetadata, NewLicense, NewTags, NewVisibility}, _From, State) ->
    Res = datasetdb:update_dataset(DatasetId, CreatorId, NewTitle, NewDescription, NewContent, NewMetadata, NewLicense, NewTags, NewVisibility),
    {reply, Res, State};
handle_call({delete_dataset, DatasetId, UserId}, _From, State) ->
    Res = datasetdb:delete_dataset(DatasetId, UserId),
    {reply, Res, State};
handle_call({get_dataset_by_id, DatasetId}, _From, State) ->
    Res = datasetdb:get_dataset_by_id(DatasetId),
    {reply, Res, State};
handle_call({get_datasets_by_creator, CreatorId}, _From, State) ->
    Res = datasetdb:get_datasets_by_creator(CreatorId),
    {reply, Res, State};
handle_call({get_public_datasets}, _From, State) ->
    Res = datasetdb:get_public_datasets(),
    {reply, Res, State};
handle_call({get_datasets_by_tag, Tag}, _From, State) ->
    Res = datasetdb:get_datasets_by_tag(Tag),
    {reply, Res, State};
handle_call({get_dataset_content, DatasetId}, _From, State) ->
    Res = datasetdb:get_dataset_content(DatasetId),
    {reply, Res, State};
handle_call({get_dataset_sample, DatasetId}, _From, State) ->
    Res = datasetdb:get_dataset_sample(DatasetId),
    {reply, Res, State};
handle_call({get_dataset_schema, DatasetId}, _From, State) ->
    Res = datasetdb:get_dataset_schema(DatasetId),
    {reply, Res, State};
handle_call({get_dataset_metadata, DatasetId}, _From, State) ->
    Res = datasetdb:get_dataset_metadata(DatasetId),
    {reply, Res, State};
handle_call({get_dataset_versions, DatasetId}, _From, State) ->
    Res = datasetdb:get_dataset_versions(DatasetId),
    {reply, Res, State};
handle_call({get_version_by_number, DatasetId, VersionNum}, _From, State) ->
    Res = datasetdb:get_version_by_number(DatasetId, VersionNum),
    {reply, Res, State};
handle_call({rollback_to_version, DatasetId, UserId, VersionNum}, _From, State) ->
    Res = datasetdb:rollback_to_version(DatasetId, UserId, VersionNum),
    {reply, Res, State};
handle_call({request_dataset_access, DatasetId, UserId, Reason}, _From, State) ->
    Res = datasetdb:request_dataset_access(DatasetId, UserId, Reason),
    {reply, Res, State};
handle_call({approve_access_request, DatasetId, CreatorId, RequestId}, _From, State) ->
    Res = datasetdb:approve_access_request(DatasetId, CreatorId, RequestId),
    {reply, Res, State};
handle_call({reject_access_request, DatasetId, CreatorId, RequestId}, _From, State) ->
    Res = datasetdb:reject_access_request(DatasetId, CreatorId, RequestId),
    {reply, Res, State};
handle_call({add_collaborator, DatasetId, CreatorId, CollaboratorId}, _From, State) ->
    Res = datasetdb:add_collaborator(DatasetId, CreatorId, CollaboratorId),
    {reply, Res, State};
handle_call({remove_collaborator, DatasetId, CreatorId, CollaboratorId}, _From, State) ->
    Res = datasetdb:remove_collaborator(DatasetId, CreatorId, CollaboratorId),
    {reply, Res, State};
handle_call({get_dataset_collaborators, DatasetId}, _From, State) ->
    Res = datasetdb:get_dataset_collaborators(DatasetId),
    {reply, Res, State};
handle_call({rate_dataset, DatasetId, UserId, Rating}, _From, State) ->
    Res = datasetdb:rate_dataset(DatasetId, UserId, Rating),
    {reply, Res, State};
handle_call({get_dataset_rating, DatasetId}, _From, State) ->
    Res = datasetdb:get_dataset_rating(DatasetId),
    {reply, Res, State};
handle_call({calculate_quality_score, DatasetId}, _From, State) ->
    Res = datasetdb:calculate_quality_score(DatasetId),
    {reply, Res, State};
handle_call({validate_dataset_schema, DatasetId, ExpectedSchema}, _From, State) ->
    Res = datasetdb:validate_dataset_schema(DatasetId, ExpectedSchema),
    {reply, Res, State};
handle_call({link_dataset_to_competition, DatasetId, CompetitionId}, _From, State) ->
    Res = datasetdb:link_dataset_to_competition(DatasetId, CompetitionId),
    {reply, Res, State};
handle_call({unlink_dataset_from_competition, DatasetId, CompetitionId}, _From, State) ->
    Res = datasetdb:unlink_dataset_from_competition(DatasetId, CompetitionId),
    {reply, Res, State};
handle_call({get_competitions_using_dataset, DatasetId}, _From, State) ->
    Res = datasetdb:get_competitions_using_dataset(DatasetId),
    {reply, Res, State};
handle_call({link_related_datasets, DatasetId, RelatedDatasetId}, _From, State) ->
    Res = datasetdb:link_related_datasets(DatasetId, RelatedDatasetId),
    {reply, Res, State};
handle_call({get_related_datasets, DatasetId}, _From, State) ->
    Res = datasetdb:get_related_datasets(DatasetId),
    {reply, Res, State};
handle_call({track_notebook_usage, DatasetId, NotebookId}, _From, State) ->
    Res = datasetdb:track_notebook_usage(DatasetId, NotebookId),
    {reply, Res, State};
handle_call({track_model_usage, DatasetId, ModelId}, _From, State) ->
    Res = datasetdb:track_model_usage(DatasetId, ModelId),
    {reply, Res, State};
handle_call({get_dataset_usage_stats, DatasetId}, _From, State) ->
    Res = datasetdb:get_dataset_usage_stats(DatasetId),
    {reply, Res, State};
handle_call({increment_download_count, DatasetId}, _From, State) ->
    Res = datasetdb:increment_download_count(DatasetId),
    {reply, Res, State};
handle_call({search_datasets, Query}, _From, State) ->
    Res = datasetdb:search_datasets(Query),
    {reply, Res, State};
handle_call({search_datasets_advanced, SearchParams}, _From, State) ->
    Res = datasetdb:search_datasets_advanced(SearchParams),
    {reply, Res, State};
handle_call({get_trending_datasets, Limit}, _From, State) ->
    Res = datasetdb:get_trending_datasets(Limit),
    {reply, Res, State};
handle_call({get_featured_datasets}, _From, State) ->
    Res = datasetdb:get_featured_datasets(),
    {reply, Res, State};
handle_call({generate_doi, DatasetId}, _From, State) ->
    Res = datasetdb:generate_doi(DatasetId),
    {reply, Res, State};
handle_call({increment_citation_count, DatasetId}, _From, State) ->
    Res = datasetdb:increment_citation_count(DatasetId),
    {reply, Res, State};
handle_call({get_citation_info, DatasetId}, _From, State) ->
    Res = datasetdb:get_citation_info(DatasetId),
    {reply, Res, State};
handle_call({update_pin_status, DatasetId, PinInfo}, _From, State) ->
    Res = datasetdb:update_pin_status(DatasetId, PinInfo),
    {reply, Res, State};
handle_call({report_dataset, ReporterId, DatasetId, Type, Description}, _From, State) ->
    Res = datasetdb:report_dataset(ReporterId, DatasetId, Type, Description),
    {reply, Res, State};
handle_call({schedule_dataset_update, DatasetId, Frequency, CronExpression}, _From, State) ->
    Res = datasetdb:schedule_dataset_update(DatasetId, Frequency, CronExpression),
    {reply, Res, State};
handle_call({get_dataset_update_schedule, DatasetId}, _From, State) ->
    Res = datasetdb:get_dataset_update_schedule(DatasetId),
    {reply, Res, State};
handle_call({validate_dataset_file, FilePath}, _From, State) ->
    Res = datasetdb:validate_dataset_file(FilePath),
    {reply, Res, State};
handle_call({get_supported_formats}, _From, State) ->
    Res = datasetdb:get_supported_formats(),
    {reply, Res, State};
handle_call({extract_zip_metadata, ZipContent}, _From, State) ->
    Res = datasetdb:extract_zip_metadata(ZipContent),
    {reply, Res, State};
handle_call({schedule_dataset_download, DatasetId, UserId, ScheduleTime}, _From, State) ->
    Res = datasetdb:schedule_dataset_download(DatasetId, UserId, ScheduleTime),
    {reply, Res, State};
handle_call({get_dataset_download_status, DownloadId}, _From, State) ->
    Res = datasetdb:get_dataset_download_status(DownloadId),
    {reply, Res, State};
handle_call({cancel_dataset_download, DownloadId}, _From, State) ->
    Res = datasetdb:cancel_dataset_download(DownloadId),
    {reply, Res, State};
handle_call({get_dataset_download_url, DatasetId}, _From, State) ->
    Res = datasetdb:get_dataset_download_url(DatasetId),
    {reply, Res, State};
handle_call({debug_dataset_info, DatasetId}, _From, State) ->
    Res = datasetdb:debug_dataset_info(DatasetId),
    {reply, Res, State};
handle_call({list_all_datasets_debug}, _From, State) ->
    Res = datasetdb:list_all_datasets_debug(),
    {reply, Res, State};
handle_call({get_dataset_file_cid, DatasetId}, _From, State) ->
    Res = datasetdb:get_dataset_file_cid(DatasetId),
    {reply, Res, State};
handle_call({get_dataset_cid, DatasetId}, _From, State) ->
    Res = datasetdb:get_dataset_cid(DatasetId),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.
handle_cast({create_dataset_concurrent, CreatorId, Title, Description, Content, MetadataMap, License, Tags, Visibility, {Pid, RequestId}}, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_dataset_concurrent, CreatorId, Title, Description, Content, MetadataMap, License, Tags, Visibility, {Pid, RequestId}},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info({'EXIT', Pid, Reason}, State = #state{worker_pool = Workers}) ->
    case lists:member(Pid, Workers) of
        true ->
            ?LOG_WARNING("Worker ~p crashed with reason: ~p. Replacing.", [Pid, Reason]),
            NewWorker = spawn_link(fun() -> worker_loop() end),
            NewWorkers = lists:delete(Pid, Workers) ++ [NewWorker],
            {noreply, State#state{worker_pool = NewWorkers}};
        false ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, #state{worker_pool = Workers}) ->
    [Worker ! {stop, self()} || Worker <- Workers],
    [receive {stopped, _W} -> ok after 1000 -> ok end || _ <- Workers],
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
