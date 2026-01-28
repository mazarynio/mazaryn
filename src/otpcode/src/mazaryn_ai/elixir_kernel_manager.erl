-module(elixir_kernel_manager).
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

    case check_elixir_available() of
        true ->
            ElixirScript = get_elixir_kernel_script(),
            ScriptFile = "/tmp/elixir_kernel_" ++ integer_to_list(erlang:system_time()) ++ ".exs",
            file:write_file(ScriptFile, ElixirScript),

            Command = "elixir " ++ ScriptFile,
            Port = open_port({spawn, Command},
                           [stream, {line, 10000}, exit_status, stderr_to_stdout]),

            {ok, #state{port = Port}};
        false ->
            {stop, {elixir_not_found, "elixir not found in PATH"}}
    end.

check_elixir_available() ->
    case os:find_executable("elixir") of
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

get_elixir_kernel_script() ->
    "defmodule NotebookKernel do\n"
    "  def execute_code(exec_id, code) do\n"
    "    try do\n"
    "      # Capture IO output\n"
    "      output = ExUnit.CaptureIO.capture_io(fn ->\n"
    "        {result, _binding} = Code.eval_string(code)\n"
    "        \n"
    "        # Only print result if it's not the return value of defmodule or other definitions\n"
    "        case result do\n"
    "          {:module, _, _, _} -> :ok  # Don't print module definitions\n"
    "          :ok -> :ok  # Don't print :ok\n"
    "          nil -> :ok  # Don't print nil\n"
    "          _ -> IO.puts(inspect(result, pretty: true))\n"
    "        end\n"
    "      end)\n"
    "      \n"
    "      # Send captured output if any\n"
    "      if output != \"\" and output != nil do\n"
    "        encoded = Base.encode64(output)\n"
    "        IO.puts(\"OUTPUT:#{exec_id}:text:#{encoded}\")\n"
    "        IO.write(\"\")\n"
    "      end\n"
    "      \n"
    "      IO.puts(\"COMPLETE:#{exec_id}\")\n"
    "      IO.write(\"\")\n"
    "    rescue\n"
    "      e ->\n"
    "        error_msg = Exception.format(:error, e, __STACKTRACE__)\n"
    "        encoded_error = Base.encode64(error_msg)\n"
    "        IO.puts(\"ERROR:#{exec_id}:#{encoded_error}\")\n"
    "        IO.write(\"\")\n"
    "    end\n"
    "  end\n"
    "  \n"
    "  def loop do\n"
    "    case IO.gets(\"\") do\n"
    "      :eof -> :ok\n"
    "      {:error, _} -> :ok\n"
    "      line ->\n"
    "        line = String.trim(line)\n"
    "        \n"
    "        if String.starts_with?(line, \"EXEC:\") do\n"
    "          [_, exec_id, encoded_code] = String.split(line, \":\", parts: 3)\n"
    "          code = Base.decode64!(encoded_code)\n"
    "          execute_code(exec_id, code)\n"
    "        end\n"
    "        \n"
    "        loop()\n"
    "    end\n"
    "  end\n"
    "end\n"
    "\n"
    "# Load ExUnit for CaptureIO\n"
    "Application.ensure_all_started(:ex_unit)\n"
    "\n"
    "NotebookKernel.loop()\n".
