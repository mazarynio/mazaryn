-module(notebook_client).
-author("Zaryn Technologies").

-export([
    create_session/3,
    close_session/1,
    execute_code/3,
    get_kernel_status/1,
    interrupt_kernel/1,
    restart_kernel/1
]).

-record(kernel_session, {
    session_id,
    kernel_id,
    notebook_id,
    user_id,
    language,
    kernel_pid,
    status = idle,
    created_at,
    last_activity
}).

create_session(NotebookId, UserId, Language) ->
    SessionId = generate_session_id(),
    KernelId = generate_kernel_id(),

    case start_kernel(Language) of
        {ok, KernelPid} ->
            Now = calendar:universal_time(),
            Session = #kernel_session{
                session_id = SessionId,
                kernel_id = KernelId,
                notebook_id = NotebookId,
                user_id = UserId,
                language = Language,
                kernel_pid = KernelPid,
                status = idle,
                created_at = Now,
                last_activity = Now
            },

            ets:insert(kernel_sessions, {SessionId, Session}),

            {ok, SessionId, KernelId};
        {error, Reason} ->
            {error, Reason}
    end.

close_session(SessionId) ->
    case ets:lookup(kernel_sessions, SessionId) of
        [{SessionId, Session}] ->
            case Session#kernel_session.kernel_pid of
                undefined -> ok;
                Pid ->
                    stop_kernel(Pid),
                    ok
            end,
            ets:delete(kernel_sessions, SessionId),
            ok;
        [] ->
            {error, session_not_found}
    end.

execute_code(SessionId, Code, CellId) ->
    case ets:lookup(kernel_sessions, SessionId) of
        [{SessionId, Session}] ->
            KernelPid = Session#kernel_session.kernel_pid,
            Language = Session#kernel_session.language,

            ExecutionId = generate_execution_id(),

            Result = case Language of
                python -> execute_python(KernelPid, Code, ExecutionId);
                julia -> execute_julia(KernelPid, Code, ExecutionId);
                elixir -> execute_elixir(KernelPid, Code, ExecutionId);
                rust -> execute_rust(KernelPid, Code, ExecutionId);
                _ -> {error, unsupported_language}
            end,

            Now = calendar:universal_time(),
            UpdatedSession = Session#kernel_session{
                last_activity = Now,
                status = idle
            },
            ets:insert(kernel_sessions, {SessionId, UpdatedSession}),

            Result;
        [] ->
            {error, session_not_found}
    end.

get_kernel_status(SessionId) ->
    case ets:lookup(kernel_sessions, SessionId) of
        [{SessionId, Session}] ->
            {ok, Session#kernel_session.status};
        [] ->
            {error, session_not_found}
    end.

interrupt_kernel(SessionId) ->
    case ets:lookup(kernel_sessions, SessionId) of
        [{SessionId, Session}] ->
            case Session#kernel_session.kernel_pid of
                undefined -> {error, no_kernel};
                Pid ->
                    Pid ! {interrupt},
                    ok
            end;
        [] ->
            {error, session_not_found}
    end.

restart_kernel(SessionId) ->
    case ets:lookup(kernel_sessions, SessionId) of
        [{SessionId, Session}] ->
            Language = Session#kernel_session.language,

            case Session#kernel_session.kernel_pid of
                undefined -> ok;
                OldPid -> stop_kernel(OldPid)
            end,

            case start_kernel(Language) of
                {ok, NewPid} ->
                    UpdatedSession = Session#kernel_session{
                        kernel_pid = NewPid,
                        status = idle,
                        last_activity = calendar:universal_time()
                    },
                    ets:insert(kernel_sessions, {SessionId, UpdatedSession}),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        [] ->
            {error, session_not_found}
    end.

start_kernel(python) ->
    case python_kernel_manager:start_kernel() of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end;
start_kernel(julia) ->
    case julia_kernel_manager:start_kernel() of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end;
start_kernel(elixir) ->
    case elixir_kernel_manager:start_kernel() of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end;
start_kernel(rust) ->
    case rust_kernel_manager:start_kernel() of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end;
start_kernel(_) ->
    {error, unsupported_language}.

stop_kernel(Pid) when is_pid(Pid) ->
    try
        Pid ! {shutdown},
        ok
    catch
        _:_ -> ok
    end.

execute_python(KernelPid, Code, ExecutionId) ->
    StartTime = erlang:system_time(millisecond),

    KernelPid ! {execute, self(), Code, ExecutionId},

    receive
        {execution_result, ExecutionId, Outputs} ->
            EndTime = erlang:system_time(millisecond),
            ExecutionTime = EndTime - StartTime,

            {ok, #{
                execution_id => ExecutionId,
                outputs => Outputs,
                execution_time_ms => ExecutionTime,
                status => success
            }};
        {execution_error, ExecutionId, Error} ->
            {error, Error}
    after 30000 ->
        {error, timeout}
    end.

execute_julia(KernelPid, Code, ExecutionId) ->
    execute_python(KernelPid, Code, ExecutionId).

execute_elixir(KernelPid, Code, ExecutionId) ->
    execute_python(KernelPid, Code, ExecutionId).

execute_rust(KernelPid, Code, ExecutionId) ->
    execute_python(KernelPid, Code, ExecutionId).

generate_session_id() ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(16))).

generate_kernel_id() ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(12))).

generate_execution_id() ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(8))).
