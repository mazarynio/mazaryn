-module(python_kernel_manager).
-author("Zaryn Technologies").
-behaviour(gen_server).

-export([start_kernel/0, stop_kernel/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    port,
    outputs = [],
    current_execution = undefined
}).

start_kernel() ->
    gen_server:start_link(?MODULE, [], []).

stop_kernel(Pid) ->
    gen_server:stop(Pid).

init([]) ->
    process_flag(trap_exit, true),

    case find_python() of
        {ok, PythonCmd} ->
            PythonScript = get_python_kernel_script(),

            Command = PythonCmd ++ " -u -c \"" ++ escape_for_shell(PythonScript) ++ "\"",

            Port = open_port({spawn, Command},
                           [stream, {line, 10000}, exit_status, stderr_to_stdout]),

            {ok, #state{port = Port}};

        {error, Reason} ->
            {stop, {python_not_found, Reason}}
    end.

find_python() ->
    case os:find_executable("python3") of
        false ->
            case os:find_executable("python") of
                false ->
                    {error, "Python not found in PATH"};
                PythonPath ->
                    {ok, PythonPath}
            end;
        Python3Path ->
            {ok, Python3Path}
    end.

escape_for_shell(String) ->
    lists:flatten(
        lists:map(
            fun($") -> "\\\"";
               ($\\) -> "\\\\";
               (C) -> C
            end,
            String
        )
    ).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({execute, From, Code, ExecutionId}, State) ->
    Port = State#state.port,

    CodeStr = ensure_string(Code),
    ExecIdStr = ensure_string(ExecutionId),

    EncodedCode = base64:encode_to_string(CodeStr),
    Command = "EXEC:" ++ ExecIdStr ++ ":" ++ EncodedCode ++ "\n",

    try
        port_command(Port, Command),

        {noreply, State#state{
            current_execution = {ExecIdStr, From},
            outputs = []
        }}
    catch
        error:Reason ->
            From ! {execution_error, ExecIdStr, Reason},
            {noreply, State}
    end;

handle_info({Port, {data, {eol, Line}}}, #state{port = Port} = State) ->
    case parse_output_line(Line) of
        {output, ExecutionId, Output} ->
            case State#state.current_execution of
                {ExecutionId, _From} ->
                    NewOutputs = [Output | State#state.outputs],
                    {noreply, State#state{outputs = NewOutputs}};
                _ ->
                    {noreply, State}
            end;

        {complete, ExecutionId} ->
            case State#state.current_execution of
                {ExecutionId, From} ->
                    Outputs = lists:reverse(State#state.outputs),
                    From ! {execution_result, ExecutionId, Outputs},
                    {noreply, State#state{
                        current_execution = undefined,
                        outputs = []
                    }};
                _ ->
                    {noreply, State}
            end;

        {error, ExecutionId, Error} ->
            case State#state.current_execution of
                {ExecutionId, From} ->
                    From ! {execution_error, ExecutionId, Error},
                    {noreply, State#state{
                        current_execution = undefined,
                        outputs = []
                    }};
                _ ->
                    {noreply, State}
            end;

        unknown ->
            {noreply, State}
    end;

handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    case State#state.current_execution of
        {ExecutionId, From} ->
            From ! {execution_error, ExecutionId, {port_exit, Status}};
        _ ->
            ok
    end,
    {stop, {port_exit, Status}, State};

handle_info({interrupt}, State) ->
    Port = State#state.port,
    port_command(Port, "INTERRUPT\n"),
    {noreply, State};

handle_info({shutdown}, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port}) ->
    catch port_close(Port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

ensure_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
ensure_string(Value) when is_list(Value) ->
    Value;
ensure_string(Value) ->
    lists:flatten(io_lib:format("~p", [Value])).

parse_output_line(Line) ->
    case string:split(Line, ":", all) of
        ["OUTPUT", ExecutionId, Type, Data] ->
            Output = #{
                output_type => list_to_atom(Type),
                data => parse_output_data(Type, Data)
            },
            {output, ExecutionId, Output};

        ["COMPLETE", ExecutionId] ->
            {complete, ExecutionId};

        ["ERROR", ExecutionId | ErrorParts] ->
            Error = string:join(ErrorParts, ":"),
            {error, ExecutionId, Error};

        _ ->
            unknown
    end.

parse_output_data("text", Data) ->
    try
        DecodedData = base64:decode_to_string(Data),
        {text, DecodedData}
    catch
        _:_ -> {text, Data}
    end;
parse_output_data("html", Data) ->
    try
        DecodedData = base64:decode_to_string(Data),
        {html, DecodedData}
    catch
        _:_ -> {html, Data}
    end;
parse_output_data("json", Data) ->
    try
        DecodedData = base64:decode_to_string(Data),
        ParsedJson = jsx:decode(list_to_binary(DecodedData)),
        {json, ParsedJson}
    catch
        _:_ -> {text, Data}
    end;
parse_output_data(_, Data) ->
    {text, Data}.

get_python_kernel_script() ->
    "import sys\n"
    "import base64\n"
    "import traceback\n"
    "from io import StringIO\n"
    "\n"
    "def execute_code(exec_id, code):\n"
    "    try:\n"
    "        old_stdout = sys.stdout\n"
    "        old_stderr = sys.stderr\n"
    "        sys.stdout = StringIO()\n"
    "        sys.stderr = StringIO()\n"
    "        \n"
    "        exec(code, globals())\n"
    "        \n"
    "        output = sys.stdout.getvalue()\n"
    "        errors = sys.stderr.getvalue()\n"
    "        \n"
    "        sys.stdout = old_stdout\n"
    "        sys.stderr = old_stderr\n"
    "        \n"
    "        if errors:\n"
    "            encoded = base64.b64encode(errors.encode('utf-8')).decode('ascii')\n"
    "            print(f'OUTPUT:{exec_id}:text:{encoded}')\n"
    "            sys.stdout.flush()\n"
    "        \n"
    "        if output:\n"
    "            encoded = base64.b64encode(output.encode('utf-8')).decode('ascii')\n"
    "            print(f'OUTPUT:{exec_id}:text:{encoded}')\n"
    "            sys.stdout.flush()\n"
    "        \n"
    "        print(f'COMPLETE:{exec_id}')\n"
    "        sys.stdout.flush()\n"
    "        \n"
    "    except Exception as e:\n"
    "        sys.stdout = old_stdout\n"
    "        sys.stderr = old_stderr\n"
    "        error_msg = traceback.format_exc()\n"
    "        encoded = base64.b64encode(error_msg.encode('utf-8')).decode('ascii')\n"
    "        print(f'ERROR:{exec_id}:{encoded}')\n"
    "        sys.stdout.flush()\n"
    "\n"
    "while True:\n"
    "    try:\n"
    "        line = input()\n"
    "        if line.startswith('EXEC:'):\n"
    "            parts = line.split(':', 2)\n"
    "            if len(parts) == 3:\n"
    "                exec_id = parts[1]\n"
    "                encoded_code = parts[2]\n"
    "                code = base64.b64decode(encoded_code).decode('utf-8')\n"
    "                execute_code(exec_id, code)\n"
    "        elif line == 'INTERRUPT':\n"
    "            break\n"
    "    except EOFError:\n"
    "        break\n"
    "    except Exception as e:\n"
    "        print(f'ERROR:unknown:{e}')\n"
    "        sys.stdout.flush()\n".
