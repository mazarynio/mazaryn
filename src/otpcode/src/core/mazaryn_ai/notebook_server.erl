-module(notebook_server).
-author("Zaryn Technologies").
-include_lib("kernel/include/logger.hrl").
-export([
    start_link/0,
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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {worker_pool_size = 100, worker_pool = []}).
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).
create_notebook(CreatorId, Title, Description, Language, KernelType, DatasetCids, CompetitionId, Environment, Tags, Visibility) ->
    gen_server:call({global, ?MODULE}, {create_notebook, CreatorId, Title, Description, Language, KernelType, DatasetCids, CompetitionId, Environment, Tags, Visibility}).
update_notebook(NotebookId, UserId, NewTitle, NewDescription, NewLanguage, NewKernelType, NewDatasetCids, NewCompetitionId, NewEnvironment, NewTags, NewVisibility) ->
    gen_server:call({global, ?MODULE}, {update_notebook, NotebookId, UserId, NewTitle, NewDescription, NewLanguage, NewKernelType, NewDatasetCids, NewCompetitionId, NewEnvironment, NewTags, NewVisibility}).
delete_notebook(NotebookId, UserId) ->
    gen_server:call({global, ?MODULE}, {delete_notebook, NotebookId, UserId}).
get_notebook_by_id(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_notebook_by_id, NotebookId}).
get_notebooks_by_creator(CreatorId) ->
    gen_server:call({global, ?MODULE}, {get_notebooks_by_creator, CreatorId}).
get_public_notebooks() ->
    gen_server:call({global, ?MODULE}, {get_public_notebooks}).
get_notebooks_by_language(Language) ->
    gen_server:call({global, ?MODULE}, {get_notebooks_by_language, Language}).
get_notebooks_by_kernel(KernelType) ->
    gen_server:call({global, ?MODULE}, {get_notebooks_by_kernel, KernelType}).
get_notebooks_by_tag(Tag) ->
    gen_server:call({global, ?MODULE}, {get_notebooks_by_tag, Tag}).
get_notebook_content(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_notebook_content, NotebookId}).
update_notebook_content(NotebookId, UserId, NewContent) ->
    gen_server:call({global, ?MODULE}, {update_notebook_content, NotebookId, UserId, NewContent}, 30000).
get_notebook_outputs(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_notebook_outputs, NotebookId}).
save_notebook_outputs(NotebookId, Outputs) ->
    gen_server:call({global, ?MODULE}, {save_notebook_outputs, NotebookId, Outputs}).
add_dataset_to_notebook(NotebookId, DatasetCid) ->
    gen_server:call({global, ?MODULE}, {add_dataset_to_notebook, NotebookId, DatasetCid}).
remove_dataset_from_notebook(NotebookId, DatasetCid) ->
    gen_server:call({global, ?MODULE}, {remove_dataset_from_notebook, NotebookId, DatasetCid}).
get_notebook_datasets(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_notebook_datasets, NotebookId}).
link_notebook_to_competition(NotebookId, CompetitionId) ->
    gen_server:call({global, ?MODULE}, {link_notebook_to_competition, NotebookId, CompetitionId}).
unlink_notebook_from_competition(NotebookId, CompetitionId) ->
    gen_server:call({global, ?MODULE}, {unlink_notebook_from_competition, NotebookId, CompetitionId}).
get_competition_notebooks(CompetitionId) ->
    gen_server:call({global, ?MODULE}, {get_competition_notebooks, CompetitionId}).
add_collaborator(NotebookId, OwnerId, CollaboratorId) ->
    gen_server:call({global, ?MODULE}, {add_collaborator, NotebookId, OwnerId, CollaboratorId}).
remove_collaborator(NotebookId, OwnerId, CollaboratorId) ->
    gen_server:call({global, ?MODULE}, {remove_collaborator, NotebookId, OwnerId, CollaboratorId}).
get_notebook_collaborators(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_notebook_collaborators, NotebookId}).
is_collaborator(NotebookId, UserId) ->
    gen_server:call({global, ?MODULE}, {is_collaborator, NotebookId, UserId}).
create_version(NotebookId, UserId, ChangeDescription) ->
    gen_server:call({global, ?MODULE}, {create_version, NotebookId, UserId, ChangeDescription}).
get_notebook_versions(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_notebook_versions, NotebookId}).
get_version_by_number(NotebookId, VersionNum) ->
    gen_server:call({global, ?MODULE}, {get_version_by_number, NotebookId, VersionNum}).
rollback_to_version(NotebookId, UserId, VersionNum) ->
    gen_server:call({global, ?MODULE}, {rollback_to_version, NotebookId, UserId, VersionNum}).
compare_versions(NotebookId, VersionNum1, VersionNum2) ->
    gen_server:call({global, ?MODULE}, {compare_versions, NotebookId, VersionNum1, VersionNum2}).
fork_notebook(NotebookId, UserId) ->
    gen_server:call({global, ?MODULE}, {fork_notebook, NotebookId, UserId}).
get_notebook_forks(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_notebook_forks, NotebookId}).
get_fork_parent(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_fork_parent, NotebookId}).
increment_fork_count(NotebookId) ->
    gen_server:call({global, ?MODULE}, {increment_fork_count, NotebookId}).
execute_notebook(NotebookId, UserId) ->
    gen_server:call({global, ?MODULE}, {execute_notebook, NotebookId, UserId}, 30000).
get_execution_logs(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_execution_logs, NotebookId}).
get_execution_stats(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_execution_stats, NotebookId}).
schedule_notebook_run(NotebookId, UserId, CronExpression) ->
    gen_server:call({global, ?MODULE}, {schedule_notebook_run, NotebookId, UserId, CronExpression}).
cancel_scheduled_run(NotebookId, ScheduleId) ->
    gen_server:call({global, ?MODULE}, {cancel_scheduled_run, NotebookId, ScheduleId}).
get_scheduled_runs(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_scheduled_runs, NotebookId}).
like_notebook(NotebookId, UserId) ->
    gen_server:call({global, ?MODULE}, {like_notebook, NotebookId, UserId}).
unlike_notebook(NotebookId, UserId) ->
    gen_server:call({global, ?MODULE}, {unlike_notebook, NotebookId, UserId}).
get_notebook_likes(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_notebook_likes, NotebookId}).
has_user_liked(NotebookId, UserId) ->
    gen_server:call({global, ?MODULE}, {has_user_liked, NotebookId, UserId}).
add_comment(NotebookId, UserId, Content) ->
    gen_server:call({global, ?MODULE}, {add_comment, NotebookId, UserId, Content}, 30000).
get_notebook_comments(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_notebook_comments, NotebookId}).
delete_comment(NotebookId, CommentId, UserId) ->
    gen_server:call({global, ?MODULE}, {delete_comment, NotebookId, CommentId, UserId}).
update_comment(NotebookId, CommentId, NewContent) ->
    gen_server:call({global, ?MODULE}, {update_comment, NotebookId, CommentId, NewContent}, 30000).
set_notebook_environment(NotebookId, Environment) ->
    gen_server:call({global, ?MODULE}, {set_notebook_environment, NotebookId, Environment}).
get_notebook_environment(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_notebook_environment, NotebookId}).
set_dependencies(NotebookId, Dependencies) ->
    gen_server:call({global, ?MODULE}, {set_dependencies, NotebookId, Dependencies}, 30000).
get_dependencies(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_dependencies, NotebookId}).
add_citation(NotebookId, Citation) ->
    gen_server:call({global, ?MODULE}, {add_citation, NotebookId, Citation}).
get_notebook_citations(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_notebook_citations, NotebookId}).
remove_citation(NotebookId, Citation) ->
    gen_server:call({global, ?MODULE}, {remove_citation, NotebookId, Citation}).
add_interactive_widget(NotebookId, WidgetType, WidgetCid) ->
    gen_server:call({global, ?MODULE}, {add_interactive_widget, NotebookId, WidgetType, WidgetCid}).
remove_interactive_widget(NotebookId, WidgetId) ->
    gen_server:call({global, ?MODULE}, {remove_interactive_widget, NotebookId, WidgetId}).
get_notebook_widgets(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_notebook_widgets, NotebookId}).
set_notebook_type(NotebookId, NotebookType) ->
    gen_server:call({global, ?MODULE}, {set_notebook_type, NotebookId, NotebookType}).
get_notebooks_by_type(NotebookType) ->
    gen_server:call({global, ?MODULE}, {get_notebooks_by_type, NotebookType}).
search_notebooks(Query) ->
    gen_server:call({global, ?MODULE}, {search_notebooks, Query}).
search_notebooks_advanced(SearchParams) ->
    gen_server:call({global, ?MODULE}, {search_notebooks_advanced, SearchParams}).
get_trending_notebooks(Limit) ->
    gen_server:call({global, ?MODULE}, {get_trending_notebooks, Limit}).
get_featured_notebooks() ->
    gen_server:call({global, ?MODULE}, {get_featured_notebooks}).
get_most_forked_notebooks(Limit) ->
    gen_server:call({global, ?MODULE}, {get_most_forked_notebooks, Limit}).
get_most_liked_notebooks(Limit) ->
    gen_server:call({global, ?MODULE}, {get_most_liked_notebooks, Limit}).
report_notebook(ReporterId, NotebookId, Type, Description) ->
    gen_server:call({global, ?MODULE}, {report_notebook, ReporterId, NotebookId, Type, Description}).
get_notebook_stats(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_notebook_stats, NotebookId}).
get_user_notebook_stats(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_notebook_stats, UserId}).
pin_notebook(NotebookId) ->
    gen_server:call({global, ?MODULE}, {pin_notebook, NotebookId}, 30000).
unpin_notebook(NotebookId) ->
    gen_server:call({global, ?MODULE}, {unpin_notebook, NotebookId}, 30000).
submit_notebook_to_competition(NotebookId, UserId, CompetitionId) ->
    gen_server:call({global, ?MODULE}, {submit_notebook_to_competition, NotebookId, UserId, CompetitionId}).
get_submission_from_notebook(NotebookId) ->
    gen_server:call({global, ?MODULE}, {get_submission_from_notebook, NotebookId}).
init([]) ->
    ?LOG_NOTICE("Notebook server has been started - ~p", [self()]),
    PoolSize = application:get_env(social_network, notebook_worker_pool_size, 100),
    WorkerPool = initialize_worker_pool(PoolSize),
    {ok, #state{worker_pool_size = PoolSize, worker_pool = WorkerPool}}.
initialize_worker_pool(Size) ->
    [spawn_link(fun() -> worker_loop() end) || _ <- lists:seq(1, Size)].
worker_loop() ->
    receive
        {create_notebook, CreatorId, Title, Description, Language, KernelType, DatasetCids, CompetitionId, Environment, Tags, Visibility, From} ->
            Result = notebookdb:create_notebook(CreatorId, Title, Description, Language, KernelType, DatasetCids, CompetitionId, Environment, Tags, Visibility),
            gen_server:reply(From, Result),
            worker_loop();
        {update_notebook, NotebookId, UserId, NewTitle, NewDescription, NewLanguage, NewKernelType, NewDatasetCids, NewCompetitionId, NewEnvironment, NewTags, NewVisibility, From} ->
            Result = notebookdb:update_notebook(NotebookId, UserId, NewTitle, NewDescription, NewLanguage, NewKernelType, NewDatasetCids, NewCompetitionId, NewEnvironment, NewTags, NewVisibility),
            gen_server:reply(From, Result),
            worker_loop();
        {update_notebook_content, NotebookId, UserId, NewContent, From} ->
            Result = notebookdb:update_notebook_content(NotebookId, UserId, NewContent),
            gen_server:reply(From, Result),
            worker_loop();
        {execute_notebook, NotebookId, UserId, From} ->
            Result = notebookdb:execute_notebook(NotebookId, UserId),
            gen_server:reply(From, Result),
            worker_loop();
        {add_comment, NotebookId, UserId, Content, From} ->
            Result = notebookdb:add_comment(NotebookId, UserId, Content),
            gen_server:reply(From, Result),
            worker_loop();
        {update_comment, NotebookId, CommentId, NewContent, From} ->
            Result = notebookdb:update_comment(NotebookId, CommentId, NewContent),
            gen_server:reply(From, Result),
            worker_loop();
        {set_dependencies, NotebookId, Dependencies, From} ->
            Result = notebookdb:set_dependencies(NotebookId, Dependencies),
            gen_server:reply(From, Result),
            worker_loop();
        {submit_notebook_to_competition, NotebookId, UserId, CompetitionId, From} ->
            Result = notebookdb:submit_notebook_to_competition(NotebookId, UserId, CompetitionId),
            gen_server:reply(From, Result),
            worker_loop();
        {pin_notebook, NotebookId, From} ->
            Result = notebookdb:pin_notebook(NotebookId),
            gen_server:reply(From, Result),
            worker_loop();
        {unpin_notebook, NotebookId, From} ->
            Result = notebookdb:unpin_notebook(NotebookId),
            gen_server:reply(From, Result),
            worker_loop();
        {stop, From} ->
            From ! {stopped, self()};
        Other ->
            ?LOG_WARNING("Worker received unknown message: ~p", [Other]),
            worker_loop()
    end.
handle_call({create_notebook, CreatorId, Title, Description, Language, KernelType, DatasetCids, CompetitionId, Environment, Tags, Visibility}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {create_notebook, CreatorId, Title, Description, Language, KernelType, DatasetCids, CompetitionId, Environment, Tags, Visibility, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({update_notebook, NotebookId, UserId, NewTitle, NewDescription, NewLanguage, NewKernelType, NewDatasetCids, NewCompetitionId, NewEnvironment, NewTags, NewVisibility}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {update_notebook, NotebookId, UserId, NewTitle, NewDescription, NewLanguage, NewKernelType, NewDatasetCids, NewCompetitionId, NewEnvironment, NewTags, NewVisibility, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({delete_notebook, NotebookId, UserId}, _From, State) ->
    Res = notebookdb:delete_notebook(NotebookId, UserId),
    {reply, Res, State};
handle_call({get_notebook_by_id, NotebookId}, _From, State) ->
    Res = notebookdb:get_notebook_by_id(NotebookId),
    {reply, Res, State};
handle_call({get_notebooks_by_creator, CreatorId}, _From, State) ->
    Res = notebookdb:get_notebooks_by_creator(CreatorId),
    {reply, Res, State};
handle_call({get_public_notebooks}, _From, State) ->
    Res = notebookdb:get_public_notebooks(),
    {reply, Res, State};
handle_call({get_notebooks_by_language, Language}, _From, State) ->
    Res = notebookdb:get_notebooks_by_language(Language),
    {reply, Res, State};
handle_call({get_notebooks_by_kernel, KernelType}, _From, State) ->
    Res = notebookdb:get_notebooks_by_kernel(KernelType),
    {reply, Res, State};
handle_call({get_notebooks_by_tag, Tag}, _From, State) ->
    Res = notebookdb:get_notebooks_by_tag(Tag),
    {reply, Res, State};
handle_call({get_notebook_content, NotebookId}, _From, State) ->
    Res = notebookdb:get_notebook_content(NotebookId),
    {reply, Res, State};
handle_call({update_notebook_content, NotebookId, UserId, NewContent}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {update_notebook_content, NotebookId, UserId, NewContent, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_notebook_outputs, NotebookId}, _From, State) ->
    Res = notebookdb:get_notebook_outputs(NotebookId),
    {reply, Res, State};
handle_call({save_notebook_outputs, NotebookId, Outputs}, _From, State) ->
    Res = notebookdb:save_notebook_outputs(NotebookId, Outputs),
    {reply, Res, State};
handle_call({add_dataset_to_notebook, NotebookId, DatasetCid}, _From, State) ->
    Res = notebookdb:add_dataset_to_notebook(NotebookId, DatasetCid),
    {reply, Res, State};
handle_call({remove_dataset_from_notebook, NotebookId, DatasetCid}, _From, State) ->
    Res = notebookdb:remove_dataset_from_notebook(NotebookId, DatasetCid),
    {reply, Res, State};
handle_call({get_notebook_datasets, NotebookId}, _From, State) ->
    Res = notebookdb:get_notebook_datasets(NotebookId),
    {reply, Res, State};
handle_call({link_notebook_to_competition, NotebookId, CompetitionId}, _From, State) ->
    Res = notebookdb:link_notebook_to_competition(NotebookId, CompetitionId),
    {reply, Res, State};
handle_call({unlink_notebook_from_competition, NotebookId, CompetitionId}, _From, State) ->
    Res = notebookdb:unlink_notebook_from_competition(NotebookId, CompetitionId),
    {reply, Res, State};
handle_call({get_competition_notebooks, CompetitionId}, _From, State) ->
    Res = notebookdb:get_competition_notebooks(CompetitionId),
    {reply, Res, State};
handle_call({add_collaborator, NotebookId, OwnerId, CollaboratorId}, _From, State) ->
    Res = notebookdb:add_collaborator(NotebookId, OwnerId, CollaboratorId),
    {reply, Res, State};
handle_call({remove_collaborator, NotebookId, OwnerId, CollaboratorId}, _From, State) ->
    Res = notebookdb:remove_collaborator(NotebookId, OwnerId, CollaboratorId),
    {reply, Res, State};
handle_call({get_notebook_collaborators, NotebookId}, _From, State) ->
    Res = notebookdb:get_notebook_collaborators(NotebookId),
    {reply, Res, State};
handle_call({is_collaborator, NotebookId, UserId}, _From, State) ->
    Res = notebookdb:is_collaborator(NotebookId, UserId),
    {reply, Res, State};
handle_call({create_version, NotebookId, UserId, ChangeDescription}, _From, State) ->
    Res = notebookdb:create_version(NotebookId, UserId, ChangeDescription),
    {reply, Res, State};
handle_call({get_notebook_versions, NotebookId}, _From, State) ->
    Res = notebookdb:get_notebook_versions(NotebookId),
    {reply, Res, State};
handle_call({get_version_by_number, NotebookId, VersionNum}, _From, State) ->
    Res = notebookdb:get_version_by_number(NotebookId, VersionNum),
    {reply, Res, State};
handle_call({rollback_to_version, NotebookId, UserId, VersionNum}, _From, State) ->
    Res = notebookdb:rollback_to_version(NotebookId, UserId, VersionNum),
    {reply, Res, State};
handle_call({compare_versions, NotebookId, VersionNum1, VersionNum2}, _From, State) ->
    Res = notebookdb:compare_versions(NotebookId, VersionNum1, VersionNum2),
    {reply, Res, State};
handle_call({fork_notebook, NotebookId, UserId}, _From, State) ->
    Res = notebookdb:fork_notebook(NotebookId, UserId),
    {reply, Res, State};
handle_call({get_notebook_forks, NotebookId}, _From, State) ->
    Res = notebookdb:get_notebook_forks(NotebookId),
    {reply, Res, State};
handle_call({get_fork_parent, NotebookId}, _From, State) ->
    Res = notebookdb:get_fork_parent(NotebookId),
    {reply, Res, State};
handle_call({increment_fork_count, NotebookId}, _From, State) ->
    Res = notebookdb:increment_fork_count(NotebookId),
    {reply, Res, State};
handle_call({execute_notebook, NotebookId, UserId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {execute_notebook, NotebookId, UserId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_execution_logs, NotebookId}, _From, State) ->
    Res = notebookdb:get_execution_logs(NotebookId),
    {reply, Res, State};
handle_call({get_execution_stats, NotebookId}, _From, State) ->
    Res = notebookdb:get_execution_stats(NotebookId),
    {reply, Res, State};
handle_call({schedule_notebook_run, NotebookId, UserId, CronExpression}, _From, State) ->
    Res = notebookdb:schedule_notebook_run(NotebookId, UserId, CronExpression),
    {reply, Res, State};
handle_call({cancel_scheduled_run, NotebookId, ScheduleId}, _From, State) ->
    Res = notebookdb:cancel_scheduled_run(NotebookId, ScheduleId),
    {reply, Res, State};
handle_call({get_scheduled_runs, NotebookId}, _From, State) ->
    Res = notebookdb:get_scheduled_runs(NotebookId),
    {reply, Res, State};
handle_call({like_notebook, NotebookId, UserId}, _From, State) ->
    Res = notebookdb:like_notebook(NotebookId, UserId),
    {reply, Res, State};
handle_call({unlike_notebook, NotebookId, UserId}, _From, State) ->
    Res = notebookdb:unlike_notebook(NotebookId, UserId),
    {reply, Res, State};
handle_call({get_notebook_likes, NotebookId}, _From, State) ->
    Res = notebookdb:get_notebook_likes(NotebookId),
    {reply, Res, State};
handle_call({has_user_liked, NotebookId, UserId}, _From, State) ->
    Res = notebookdb:has_user_liked(NotebookId, UserId),
    {reply, Res, State};
handle_call({add_comment, NotebookId, UserId, Content}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {add_comment, NotebookId, UserId, Content, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_notebook_comments, NotebookId}, _From, State) ->
    Res = notebookdb:get_notebook_comments(NotebookId),
    {reply, Res, State};
handle_call({delete_comment, NotebookId, CommentId, UserId}, _From, State) ->
    Res = notebookdb:delete_comment(NotebookId, CommentId, UserId),
    {reply, Res, State};
handle_call({update_comment, NotebookId, CommentId, NewContent}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {update_comment, NotebookId, CommentId, NewContent, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({set_notebook_environment, NotebookId, Environment}, _From, State) ->
    Res = notebookdb:set_notebook_environment(NotebookId, Environment),
    {reply, Res, State};
handle_call({get_notebook_environment, NotebookId}, _From, State) ->
    Res = notebookdb:get_notebook_environment(NotebookId),
    {reply, Res, State};
handle_call({set_dependencies, NotebookId, Dependencies}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {set_dependencies, NotebookId, Dependencies, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_dependencies, NotebookId}, _From, State) ->
    Res = notebookdb:get_dependencies(NotebookId),
    {reply, Res, State};
handle_call({add_citation, NotebookId, Citation}, _From, State) ->
    Res = notebookdb:add_citation(NotebookId, Citation),
    {reply, Res, State};
handle_call({get_notebook_citations, NotebookId}, _From, State) ->
    Res = notebookdb:get_notebook_citations(NotebookId),
    {reply, Res, State};
handle_call({remove_citation, NotebookId, Citation}, _From, State) ->
    Res = notebookdb:remove_citation(NotebookId, Citation),
    {reply, Res, State};
handle_call({add_interactive_widget, NotebookId, WidgetType, WidgetCid}, _From, State) ->
    Res = notebookdb:add_interactive_widget(NotebookId, WidgetType, WidgetCid),
    {reply, Res, State};
handle_call({remove_interactive_widget, NotebookId, WidgetId}, _From, State) ->
    Res = notebookdb:remove_interactive_widget(NotebookId, WidgetId),
    {reply, Res, State};
handle_call({get_notebook_widgets, NotebookId}, _From, State) ->
    Res = notebookdb:get_notebook_widgets(NotebookId),
    {reply, Res, State};
handle_call({set_notebook_type, NotebookId, NotebookType}, _From, State) ->
    Res = notebookdb:set_notebook_type(NotebookId, NotebookType),
    {reply, Res, State};
handle_call({get_notebooks_by_type, NotebookType}, _From, State) ->
    Res = notebookdb:get_notebooks_by_type(NotebookType),
    {reply, Res, State};
handle_call({search_notebooks, Query}, _From, State) ->
    Res = notebookdb:search_notebooks(Query),
    {reply, Res, State};
handle_call({search_notebooks_advanced, SearchParams}, _From, State) ->
    Res = notebookdb:search_notebooks_advanced(SearchParams),
    {reply, Res, State};
handle_call({get_trending_notebooks, Limit}, _From, State) ->
    Res = notebookdb:get_trending_notebooks(Limit),
    {reply, Res, State};
handle_call({get_featured_notebooks}, _From, State) ->
    Res = notebookdb:get_featured_notebooks(),
    {reply, Res, State};
handle_call({get_most_forked_notebooks, Limit}, _From, State) ->
    Res = notebookdb:get_most_forked_notebooks(Limit),
    {reply, Res, State};
handle_call({get_most_liked_notebooks, Limit}, _From, State) ->
    Res = notebookdb:get_most_liked_notebooks(Limit),
    {reply, Res, State};
handle_call({report_notebook, ReporterId, NotebookId, Type, Description}, _From, State) ->
    Res = notebookdb:report_notebook(ReporterId, NotebookId, Type, Description),
    {reply, Res, State};
handle_call({get_notebook_stats, NotebookId}, _From, State) ->
    Res = notebookdb:get_notebook_stats(NotebookId),
    {reply, Res, State};
handle_call({get_user_notebook_stats, UserId}, _From, State) ->
    Res = notebookdb:get_user_notebook_stats(UserId),
    {reply, Res, State};
handle_call({pin_notebook, NotebookId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {pin_notebook, NotebookId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({unpin_notebook, NotebookId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {unpin_notebook, NotebookId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({submit_notebook_to_competition, NotebookId, UserId, CompetitionId}, From, State = #state{worker_pool = [Worker|RestWorkers]}) ->
    Worker ! {submit_notebook_to_competition, NotebookId, UserId, CompetitionId, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};
handle_call({get_submission_from_notebook, NotebookId}, _From, State) ->
    Res = notebookdb:get_submission_from_notebook(NotebookId),
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
