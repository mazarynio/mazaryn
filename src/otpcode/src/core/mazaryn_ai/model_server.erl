-module(model_server).
-author("Zaryn Technologies").
-include_lib("kernel/include/logger.hrl").
-export([
    start_link/0,
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
    get_supported_frameworks/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {worker_pool_size = 100, worker_pool = []}).
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).
create_model(CreatorId, Title, Description, Framework, TaskType, ModelFile, License, Tags, Visibility, PerformanceMetrics) ->
    gen_server:call({global, ?MODULE}, {create_model, CreatorId, Title, Description, Framework, TaskType, ModelFile, License, Tags, Visibility, PerformanceMetrics}).
create_model_concurrent(CreatorId, Title, Description, Framework, TaskType, ModelFile, License, Tags, Visibility, PerformanceMetrics) ->
    gen_server:call({global, ?MODULE}, {create_model_concurrent, CreatorId, Title, Description, Framework, TaskType, ModelFile, License, Tags, Visibility, PerformanceMetrics}).
create_model_from_file(CreatorId, Title, Description, Framework, TaskType, FilePath, License, Tags, Visibility) ->
    gen_server:call({global, ?MODULE}, {create_model_from_file, CreatorId, Title, Description, Framework, TaskType, FilePath, License, Tags, Visibility}, 30000).
update_model(ModelId, CreatorId, NewTitle, NewDescription, NewFramework, NewTaskType, NewModelFile, NewLicense, NewTags, NewVisibility, NewPerformanceMetrics) ->
    gen_server:call({global, ?MODULE}, {update_model, ModelId, CreatorId, NewTitle, NewDescription, NewFramework, NewTaskType, NewModelFile, NewLicense, NewTags, NewVisibility, NewPerformanceMetrics}).
delete_model(ModelId, UserId) ->
    gen_server:call({global, ?MODULE}, {delete_model, ModelId, UserId}).
upload_model_file(ModelId, FilePath) ->
    gen_server:call({global, ?MODULE}, {upload_model_file, ModelId, FilePath}, 30000).
get_model_by_id(ModelId) ->
    gen_server:call({global, ?MODULE}, {get_model_by_id, ModelId}).
get_models_by_creator(CreatorId) ->
    gen_server:call({global, ?MODULE}, {get_models_by_creator, CreatorId}).
get_public_models() ->
    gen_server:call({global, ?MODULE}, {get_public_models}).
get_models_by_framework(Framework) ->
    gen_server:call({global, ?MODULE}, {get_models_by_framework, Framework}).
get_models_by_task(TaskType) ->
    gen_server:call({global, ?MODULE}, {get_models_by_task, TaskType}).
get_models_by_tag(Tag) ->
    gen_server:call({global, ?MODULE}, {get_models_by_tag, Tag}).
download_model(ModelId, UserId) ->
    gen_server:call({global, ?MODULE}, {download_model, ModelId, UserId}, 30000).
create_model_version(ModelId, UserId, NewModelFile, ChangeDescription) ->
    gen_server:call({global, ?MODULE}, {create_model_version, ModelId, UserId, NewModelFile, ChangeDescription}, 30000).
get_model_versions(ModelId) ->
    gen_server:call({global, ?MODULE}, {get_model_versions, ModelId}).
get_version_by_number(ModelId, VersionNum) ->
    gen_server:call({global, ?MODULE}, {get_version_by_number, ModelId, VersionNum}).
rollback_to_version(ModelId, UserId, VersionNum) ->
    gen_server:call({global, ?MODULE}, {rollback_to_version, ModelId, UserId, VersionNum}).
add_training_dataset(ModelId, DatasetCID) ->
    gen_server:call({global, ?MODULE}, {add_training_dataset, ModelId, DatasetCID}).
remove_training_dataset(ModelId, DatasetCID) ->
    gen_server:call({global, ?MODULE}, {remove_training_dataset, ModelId, DatasetCID}).
get_training_datasets(ModelId) ->
    gen_server:call({global, ?MODULE}, {get_training_datasets, ModelId}).
set_performance_metrics(ModelId, Metrics) ->
    gen_server:call({global, ?MODULE}, {set_performance_metrics, ModelId, Metrics}).
get_performance_metrics(ModelId) ->
    gen_server:call({global, ?MODULE}, {get_performance_metrics, ModelId}).
update_metric(ModelId, MetricName, MetricValue) ->
    gen_server:call({global, ?MODULE}, {update_metric, ModelId, MetricName, MetricValue}).
deploy_model(ModelId, DeploymentConfig) ->
    gen_server:call({global, ?MODULE}, {deploy_model, ModelId, DeploymentConfig}).
undeploy_model(ModelId, UserId) ->
    gen_server:call({global, ?MODULE}, {undeploy_model, ModelId, UserId}).
get_deployment_info(ModelId) ->
    gen_server:call({global, ?MODULE}, {get_deployment_info, ModelId}).
update_deployment_endpoint(ModelId, NewEndpoint) ->
    gen_server:call({global, ?MODULE}, {update_deployment_endpoint, ModelId, NewEndpoint}).
add_benchmark_result(ModelId, DatasetId, Metric, Score) ->
    gen_server:call({global, ?MODULE}, {add_benchmark_result, ModelId, DatasetId, Metric, Score}).
get_benchmark_results(ModelId) ->
    gen_server:call({global, ?MODULE}, {get_benchmark_results, ModelId}).
get_best_benchmark(ModelId, Metric) ->
    gen_server:call({global, ?MODULE}, {get_best_benchmark, ModelId, Metric}).
rate_model(ModelId, UserId, Rating) ->
    gen_server:call({global, ?MODULE}, {rate_model, ModelId, UserId, Rating}).
get_model_rating(ModelId) ->
    gen_server:call({global, ?MODULE}, {get_model_rating, ModelId}).
increment_download_count(ModelId) ->
    gen_server:call({global, ?MODULE}, {increment_download_count, ModelId}).
track_inference_time(ModelId, TimeMs) ->
    gen_server:call({global, ?MODULE}, {track_inference_time, ModelId, TimeMs}).
calculate_carbon_footprint(ModelId, TrainingHours) ->
    gen_server:call({global, ?MODULE}, {calculate_carbon_footprint, ModelId, TrainingHours}).
search_models(Query) ->
    gen_server:call({global, ?MODULE}, {search_models, Query}).
search_models_advanced(SearchParams) ->
    gen_server:call({global, ?MODULE}, {search_models_advanced, SearchParams}).
get_trending_models(Limit) ->
    gen_server:call({global, ?MODULE}, {get_trending_models, Limit}).
get_featured_models() ->
    gen_server:call({global, ?MODULE}, {get_featured_models}).
get_models_by_performance(Metric, MinScore) ->
    gen_server:call({global, ?MODULE}, {get_models_by_performance, Metric, MinScore}).
pin_model(ModelId) ->
    gen_server:call({global, ?MODULE}, {pin_model, ModelId}, 30000).
unpin_model(ModelId) ->
    gen_server:call({global, ?MODULE}, {unpin_model, ModelId}, 30000).
update_pin_status(ModelId, PinInfo) ->
    gen_server:call({global, ?MODULE}, {update_pin_status, ModelId, PinInfo}).
report_model(ReporterId, ModelId, Type, Description) ->
    gen_server:call({global, ?MODULE}, {report_model, ReporterId, ModelId, Type, Description}).
link_to_competition(ModelId, CompetitionId) ->
    gen_server:call({global, ?MODULE}, {link_to_competition, ModelId, CompetitionId}).
unlink_from_competition(ModelId, CompetitionId) ->
    gen_server:call({global, ?MODULE}, {unlink_from_competition, ModelId, CompetitionId}).
get_competition_models(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_competition_models, CompetitionId}).
export_model_card(ModelId) ->
    gen_server:call({global, ?MODULE}, {export_model_card, ModelId}).
import_model_card(ModelId, CardData) ->
    gen_server:call({global, ?MODULE}, {import_model_card, ModelId, CardData}, 30000).
validate_model_file(FilePath) ->
    gen_server:call({global, ?MODULE}, {validate_model_file, FilePath}).
get_supported_frameworks() ->
    gen_server:call({global, ?MODULE}, {get_supported_frameworks}).
init([]) ->
    ?LOG_NOTICE("Model server has been started - ~p", [self()]),
    PoolSize = application:get_env(social_network, model_worker_pool_size, 100),
    WorkerPool = initialize_worker_pool(PoolSize),
    {ok, #state{worker_pool_size = PoolSize, worker_pool = WorkerPool}}.
initialize_worker_pool(Size) ->
    [spawn_link(fun() -> worker_loop() end) || _ <- lists:seq(1, Size)].
worker_loop() ->
    receive
        {create_model, CreatorId, Title, Description, Framework, TaskType, ModelFile, License, Tags, Visibility, PerformanceMetrics, From} ->
            Result = modeldb:create_model(CreatorId, Title, Description, Framework, TaskType, ModelFile, License, Tags, Visibility, PerformanceMetrics),
            gen_server:reply(From, Result),
            worker_loop();
        {create_model_concurrent, CreatorId, Title, Description, Framework, TaskType, ModelFile, License, Tags, Visibility, PerformanceMetrics, From} ->
            Result = modeldb:create_model_concurrent(CreatorId, Title, Description, Framework, TaskType, ModelFile, License, Tags, Visibility, PerformanceMetrics),
            gen_server:reply(From, Result),
            worker_loop();
        {create_model_from_file, CreatorId, Title, Description, Framework, TaskType, FilePath, License, Tags, Visibility, From} ->
            Result = modeldb:create_model_from_file(CreatorId, Title, Description, Framework, TaskType, FilePath, License, Tags, Visibility),
            gen_server:reply(From, Result),
            worker_loop();
        {update_model, ModelId, CreatorId, NewTitle, NewDescription, NewFramework, NewTaskType, NewModelFile, NewLicense, NewTags, NewVisibility, NewPerformanceMetrics, From} ->
            Result = modeldb:update_model(ModelId, CreatorId, NewTitle, NewDescription, NewFramework, NewTaskType, NewModelFile, NewLicense, NewTags, NewVisibility, NewPerformanceMetrics),
            gen_server:reply(From, Result),
            worker_loop();
        {upload_model_file, ModelId, FilePath, From} ->
            Result = modeldb:upload_model_file(ModelId, FilePath),
            gen_server:reply(From, Result),
            worker_loop();
        {download_model, ModelId, UserId, From} ->
            Result = modeldb:download_model(ModelId, UserId),
            gen_server:reply(From, Result),
            worker_loop();
        {create_model_version, ModelId, UserId, NewModelFile, ChangeDescription, From} ->
            Result = modeldb:create_model_version(ModelId, UserId, NewModelFile, ChangeDescription),
            gen_server:reply(From, Result),
            worker_loop();
        {deploy_model, ModelId, DeploymentConfig, From} ->
            Result = modeldb:deploy_model(ModelId, DeploymentConfig),
            gen_server:reply(From, Result),
            worker_loop();
        {pin_model, ModelId, From} ->
            Result = modeldb:pin_model(ModelId),
            gen_server:reply(From, Result),
            worker_loop();
        {unpin_model, ModelId, From} ->
            Result = modeldb:unpin_model(ModelId),
            gen_server:reply(From, Result),
            worker_loop();
        {import_model_card, ModelId, CardData, From} ->
            Result = modeldb:import_model_card(ModelId, CardData),
            gen_server:reply(From, Result),
            worker_loop();
        {report_model, ReporterId, ModelId, Type, Description, From} ->
            Result = modeldb:report_model(ReporterId, ModelId, Type, Description),
            gen_server:reply(From, Result),
            worker_loop();
        {stop, From} ->
            From ! {stopped, self()};
        Other ->
            ?LOG_WARNING("Worker received unknown message: ~p", [Other]),
            worker_loop()
    end.
handle_call({create_model, CreatorId, Title, Description, Framework, TaskType, ModelFile, License, Tags, Visibility, PerformanceMetrics}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_model, CreatorId, Title, Description, Framework, TaskType, ModelFile, License, Tags, Visibility, PerformanceMetrics, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({create_model_concurrent, CreatorId, Title, Description, Framework, TaskType, ModelFile, License, Tags, Visibility, PerformanceMetrics}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_model_concurrent, CreatorId, Title, Description, Framework, TaskType, ModelFile, License, Tags, Visibility, PerformanceMetrics, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({create_model_from_file, CreatorId, Title, Description, Framework, TaskType, FilePath, License, Tags, Visibility}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_model_from_file, CreatorId, Title, Description, Framework, TaskType, FilePath, License, Tags, Visibility, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({update_model, ModelId, CreatorId, NewTitle, NewDescription, NewFramework, NewTaskType, NewModelFile, NewLicense, NewTags, NewVisibility, NewPerformanceMetrics}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {update_model, ModelId, CreatorId, NewTitle, NewDescription, NewFramework, NewTaskType, NewModelFile, NewLicense, NewTags, NewVisibility, NewPerformanceMetrics, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({delete_model, ModelId, UserId}, _From, State) ->
    Res = modeldb:delete_model(ModelId, UserId),
    {reply, Res, State};
handle_call({upload_model_file, ModelId, FilePath}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {upload_model_file, ModelId, FilePath, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_model_by_id, ModelId}, _From, State) ->
    Res = modeldb:get_model_by_id(ModelId),
    {reply, Res, State};
handle_call({get_models_by_creator, CreatorId}, _From, State) ->
    Res = modeldb:get_models_by_creator(CreatorId),
    {reply, Res, State};
handle_call({get_public_models}, _From, State) ->
    Res = modeldb:get_public_models(),
    {reply, Res, State};
handle_call({get_models_by_framework, Framework}, _From, State) ->
    Res = modeldb:get_models_by_framework(Framework),
    {reply, Res, State};
handle_call({get_models_by_task, TaskType}, _From, State) ->
    Res = modeldb:get_models_by_task(TaskType),
    {reply, Res, State};
handle_call({get_models_by_tag, Tag}, _From, State) ->
    Res = modeldb:get_models_by_tag(Tag),
    {reply, Res, State};
handle_call({download_model, ModelId, UserId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {download_model, ModelId, UserId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({create_model_version, ModelId, UserId, NewModelFile, ChangeDescription}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_model_version, ModelId, UserId, NewModelFile, ChangeDescription, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_model_versions, ModelId}, _From, State) ->
    Res = modeldb:get_model_versions(ModelId),
    {reply, Res, State};
handle_call({get_version_by_number, ModelId, VersionNum}, _From, State) ->
    Res = modeldb:get_version_by_number(ModelId, VersionNum),
    {reply, Res, State};
handle_call({rollback_to_version, ModelId, UserId, VersionNum}, _From, State) ->
    Res = modeldb:rollback_to_version(ModelId, UserId, VersionNum),
    {reply, Res, State};
handle_call({add_training_dataset, ModelId, DatasetCID}, _From, State) ->
    Res = modeldb:add_training_dataset(ModelId, DatasetCID),
    {reply, Res, State};
handle_call({remove_training_dataset, ModelId, DatasetCID}, _From, State) ->
    Res = modeldb:remove_training_dataset(ModelId, DatasetCID),
    {reply, Res, State};
handle_call({get_training_datasets, ModelId}, _From, State) ->
    Res = modeldb:get_training_datasets(ModelId),
    {reply, Res, State};
handle_call({set_performance_metrics, ModelId, Metrics}, _From, State) ->
    Res = modeldb:set_performance_metrics(ModelId, Metrics),
    {reply, Res, State};
handle_call({get_performance_metrics, ModelId}, _From, State) ->
    Res = modeldb:get_performance_metrics(ModelId),
    {reply, Res, State};
handle_call({update_metric, ModelId, MetricName, MetricValue}, _From, State) ->
    Res = modeldb:update_metric(ModelId, MetricName, MetricValue),
    {reply, Res, State};
handle_call({deploy_model, ModelId, DeploymentConfig}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {deploy_model, ModelId, DeploymentConfig, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({undeploy_model, ModelId, UserId}, _From, State) ->
    Res = modeldb:undeploy_model(ModelId, UserId),
    {reply, Res, State};
handle_call({get_deployment_info, ModelId}, _From, State) ->
    Res = modeldb:get_deployment_info(ModelId),
    {reply, Res, State};
handle_call({update_deployment_endpoint, ModelId, NewEndpoint}, _From, State) ->
    Res = modeldb:update_deployment_endpoint(ModelId, NewEndpoint),
    {reply, Res, State};
handle_call({add_benchmark_result, ModelId, DatasetId, Metric, Score}, _From, State) ->
    Res = modeldb:add_benchmark_result(ModelId, DatasetId, Metric, Score),
    {reply, Res, State};
handle_call({get_benchmark_results, ModelId}, _From, State) ->
    Res = modeldb:get_benchmark_results(ModelId),
    {reply, Res, State};
handle_call({get_best_benchmark, ModelId, Metric}, _From, State) ->
    Res = modeldb:get_best_benchmark(ModelId, Metric),
    {reply, Res, State};
handle_call({rate_model, ModelId, UserId, Rating}, _From, State) ->
    Res = modeldb:rate_model(ModelId, UserId, Rating),
    {reply, Res, State};
handle_call({get_model_rating, ModelId}, _From, State) ->
    Res = modeldb:get_model_rating(ModelId),
    {reply, Res, State};
handle_call({increment_download_count, ModelId}, _From, State) ->
    Res = modeldb:increment_download_count(ModelId),
    {reply, Res, State};
handle_call({track_inference_time, ModelId, TimeMs}, _From, State) ->
    Res = modeldb:track_inference_time(ModelId, TimeMs),
    {reply, Res, State};
handle_call({calculate_carbon_footprint, ModelId, TrainingHours}, _From, State) ->
    Res = modeldb:calculate_carbon_footprint(ModelId, TrainingHours),
    {reply, Res, State};
handle_call({search_models, Query}, _From, State) ->
    Res = modeldb:search_models(Query),
    {reply, Res, State};
handle_call({search_models_advanced, SearchParams}, _From, State) ->
    Res = modeldb:search_models_advanced(SearchParams),
    {reply, Res, State};
handle_call({get_trending_models, Limit}, _From, State) ->
    Res = modeldb:get_trending_models(Limit),
    {reply, Res, State};
handle_call({get_featured_models}, _From, State) ->
    Res = modeldb:get_featured_models(),
    {reply, Res, State};
handle_call({get_models_by_performance, Metric, MinScore}, _From, State) ->
    Res = modeldb:get_models_by_performance(Metric, MinScore),
    {reply, Res, State};
handle_call({pin_model, ModelId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {pin_model, ModelId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({unpin_model, ModelId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {unpin_model, ModelId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({update_pin_status, ModelId, PinInfo}, _From, State) ->
    Res = modeldb:update_pin_status(ModelId, PinInfo),
    {reply, Res, State};
handle_call({report_model, ReporterId, ModelId, Type, Description}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {report_model, ReporterId, ModelId, Type, Description, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({link_to_competition, ModelId, CompetitionId}, _From, State) ->
    Res = modeldb:link_to_competition(ModelId, CompetitionId),
    {reply, Res, State};
handle_call({unlink_from_competition, ModelId, CompetitionId}, _From, State) ->
    Res = modeldb:unlink_from_competition(ModelId, CompetitionId),
    {reply, Res, State};
handle_call({get_competition_models, CompetitionId}, _From, State) ->
    Res = modeldb:get_competition_models(CompetitionId),
    {reply, Res, State};
handle_call({export_model_card, ModelId}, _From, State) ->
    Res = modeldb:export_model_card(ModelId),
    {reply, Res, State};
handle_call({import_model_card, ModelId, CardData}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {import_model_card, ModelId, CardData, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({validate_model_file, FilePath}, _From, State) ->
    Res = modeldb:validate_model_file(FilePath),
    {reply, Res, State};
handle_call({get_supported_frameworks}, _From, State) ->
    Res = modeldb:get_supported_frameworks(),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.
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
