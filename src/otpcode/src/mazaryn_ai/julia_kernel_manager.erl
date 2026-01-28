-module(julia_kernel_manager).
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

    case check_julia_available() of
        true ->
            JuliaScript = get_julia_kernel_script(),
            ScriptFile = "/tmp/julia_kernel_" ++ integer_to_list(erlang:system_time()) ++ ".jl",
            file:write_file(ScriptFile, JuliaScript),

            Command = "julia --startup-file=no " ++ ScriptFile,
            Port = open_port({spawn, Command},
                           [stream, {line, 10000}, exit_status, stderr_to_stdout]),

            {ok, #state{port = Port}};
        false ->
            {stop, {julia_not_found, "julia not found in PATH"}}
    end.

check_julia_available() ->
    case os:find_executable("julia") of
        false -> false;
        _ -> true
    end.

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

get_julia_kernel_script() ->
    "using Base64\n"
    "\n"
    "function execute_code(exec_id::String, code::String)\n"
    "    try\n"
    "        output_buffer = IOBuffer()\n"
    "        redirect_stdout(output_buffer) do\n"
    "            redirect_stderr(output_buffer) do\n"
    "                result = eval(Meta.parse(code))\n"
    "                if result !== nothing\n"
    "                    println(result)\n"
    "                end\n"
    "            end\n"
    "        end\n"
    "        \n"
    "        output_content = String(take!(output_buffer))\n"
    "        if !isempty(output_content)\n"
    "            encoded = base64encode(output_content)\n"
    "            println(\"OUTPUT:\", exec_id, \":text:\", encoded)\n"
    "            flush(stdout)\n"
    "        end\n"
    "        \n"
    "        println(\"COMPLETE:\", exec_id)\n"
    "        flush(stdout)\n"
    "    catch e\n"
    "        error_msg = sprint(showerror, e, catch_backtrace())\n"
    "        encoded_error = base64encode(error_msg)\n"
    "        println(\"ERROR:\", exec_id, \":\", encoded_error)\n"
    "        flush(stdout)\n"
    "    end\n"
    "end\n"
    "\n"
    "while true\n"
    "    try\n"
    "        line = readline(stdin)\n"
    "        if startswith(line, \"EXEC:\")\n"
    "            parts = split(line, \":\", limit=3)\n"
    "            if length(parts) == 3\n"
    "                exec_id = parts[2]\n"
    "                encoded_code = parts[3]\n"
    "                code = String(base64decode(encoded_code))\n"
    "                execute_code(exec_id, code)\n"
    "            end\n"
    "        elseif line == \"INTERRUPT\"\n"
    "            break\n"
    "        end\n"
    "    catch e\n"
    "        println(\"ERROR:unknown:\", e)\n"
    "        flush(stdout)\n"
    "    end\n"
    "end\n".
