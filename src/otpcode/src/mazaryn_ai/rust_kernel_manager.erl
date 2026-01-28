-module(rust_kernel_manager).
-author("Zaryn Technologies").
-behaviour(gen_server).

-export([start_kernel/0, stop_kernel/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    temp_dir,
    outputs = [],
    current_execution = undefined
}).

start_kernel() ->
    gen_server:start_link(?MODULE, [], []).

stop_kernel(Pid) ->
    gen_server:stop(Pid).

init([]) ->
    process_flag(trap_exit, true),

    case check_rust_available() of
        true ->
            TempDir = create_temp_dir(),
            {ok, #state{temp_dir = TempDir}};
        false ->
            {stop, {rust_not_found, "rustc not found in PATH. Please install Rust from https://rustup.rs/"}}
    end.

check_rust_available() ->
    case os:find_executable("rustc") of
        false -> false;
        _ -> true
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({execute, From, Code, ExecutionId}, State) ->
    TempDir = State#state.temp_dir,

    CodeStr = ensure_string(Code),
    ExecIdStr = sanitize_filename(ensure_string(ExecutionId)),

    CodeFile = filename:join(TempDir, "code_" ++ ExecIdStr ++ ".rs"),
    ExeFile = filename:join(TempDir, "code_" ++ ExecIdStr),

    FullCode = "fn main() {\n" ++ CodeStr ++ "\n}",

    case file:write_file(CodeFile, FullCode) of
        ok ->
            CompileCmd = "rustc " ++ CodeFile ++ " -o " ++ ExeFile ++ " 2>&1",
            CompileOutput = os:cmd(CompileCmd),

            case filelib:is_file(ExeFile) of
                true ->
                    RunCmd = ExeFile ++ " 2>&1",
                    RunOutput = os:cmd(RunCmd),

                    file:delete(CodeFile),
                    file:delete(ExeFile),

                    Output = #{
                        output_type => text,
                        data => {text, RunOutput}
                    },

                    From ! {execution_result, ExecutionId, [Output]},
                    {noreply, State};

                false ->
                    file:delete(CodeFile),

                    ErrorOutput = #{
                        output_type => text,
                        data => {text, "Compilation failed:\n" ++ CompileOutput}
                    },

                    From ! {execution_result, ExecutionId, [ErrorOutput]},
                    {noreply, State}
            end;

        {error, Reason} ->
            From ! {execution_error, ExecutionId, Reason},
            {noreply, State}
    end;

handle_info({interrupt}, State) ->
    {noreply, State};

handle_info({shutdown}, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{temp_dir = TempDir}) ->
    cleanup_temp_dir(TempDir),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

ensure_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
ensure_string(Value) when is_list(Value) ->
    Value;
ensure_string(Value) ->
    lists:flatten(io_lib:format("~p", [Value])).

sanitize_filename(Filename) ->
    lists:map(
        fun(C) when C >= $a, C =< $z -> C;
           (C) when C >= $A, C =< $Z -> C;
           (C) when C >= $0, C =< $9 -> C;
           ($-) -> $-;
           ($_) -> $_;
           (_) -> $_
        end,
        Filename
    ).

create_temp_dir() ->
    TempBase = "/tmp/rust_kernel_" ++ integer_to_list(erlang:system_time()),
    file:make_dir(TempBase),
    TempBase.

cleanup_temp_dir(undefined) ->
    ok;
cleanup_temp_dir(Dir) ->
    os:cmd("rm -rf " ++ Dir),
    ok.

get_rust_kernel_script() ->
    "use std::io::{self, BufRead, Write};\n"
    "use std::process::Command;\n"
    "use base64::{Engine as _, engine::general_purpose};\n"
    "\n"
    "fn execute_code(exec_id: &str, code_path: &str) {\n"
    "    let output_path = format!(\"/tmp/rust_output_{}\", exec_id);\n"
    "    \n"
    "    let compile_result = Command::new(\"rustc\")\n"
    "        .arg(code_path)\n"
    "        .arg(\"-o\")\n"
    "        .arg(&output_path)\n"
    "        .output();\n"
    "    \n"
    "    match compile_result {\n"
    "        Ok(output) => {\n"
    "            if output.status.success() {\n"
    "                let run_result = Command::new(&output_path).output();\n"
    "                \n"
    "                match run_result {\n"
    "                    Ok(run_output) => {\n"
    "                        let stdout = String::from_utf8_lossy(&run_output.stdout);\n"
    "                        if !stdout.is_empty() {\n"
    "                            let encoded = general_purpose::STANDARD.encode(stdout.as_bytes());\n"
    "                            println!(\"OUTPUT:{}:text:{}\", exec_id, encoded);\n"
    "                        }\n"
    "                        \n"
    "                        let stderr = String::from_utf8_lossy(&run_output.stderr);\n"
    "                        if !stderr.is_empty() {\n"
    "                            let encoded = general_purpose::STANDARD.encode(stderr.as_bytes());\n"
    "                            println!(\"OUTPUT:{}:text:{}\", exec_id, encoded);\n"
    "                        }\n"
    "                        \n"
    "                        println!(\"COMPLETE:{}\", exec_id);\n"
    "                    }\n"
    "                    Err(e) => {\n"
    "                        let error_msg = format!(\"Runtime error: {}\", e);\n"
    "                        let encoded = general_purpose::STANDARD.encode(error_msg.as_bytes());\n"
    "                        println!(\"ERROR:{}:{}\", exec_id, encoded);\n"
    "                    }\n"
    "                }\n"
    "                \n"
    "                let _ = std::fs::remove_file(output_path);\n"
    "            } else {\n"
    "                let stderr = String::from_utf8_lossy(&output.stderr);\n"
    "                let encoded = general_purpose::STANDARD.encode(stderr.as_bytes());\n"
    "                println!(\"ERROR:{}:{}\", exec_id, encoded);\n"
    "            }\n"
    "        }\n"
    "        Err(e) => {\n"
    "            let error_msg = format!(\"Compilation failed: {}\", e);\n"
    "            let encoded = general_purpose::STANDARD.encode(error_msg.as_bytes());\n"
    "            println!(\"ERROR:{}:{}\", exec_id, encoded);\n"
    "        }\n"
    "    }\n"
    "}\n"
    "\n"
    "fn main() {\n"
    "    let stdin = io::stdin();\n"
    "    \n"
    "    for line in stdin.lock().lines() {\n"
    "        if let Ok(line) = line {\n"
    "            if line.starts_with(\"EXEC:\") {\n"
    "                let parts: Vec<&str> = line.splitn(3, ':').collect();\n"
    "                if parts.len() == 3 {\n"
    "                    let exec_id = parts[1];\n"
    "                    let encoded_path = parts[2];\n"
    "                    \n"
    "                    if let Ok(decoded) = general_purpose::STANDARD.decode(encoded_path) {\n"
    "                        if let Ok(code_path) = String::from_utf8(decoded) {\n"
    "                            execute_code(exec_id, &code_path);\n"
    "                        }\n"
    "                    }\n"
    "                }\n"
    "            }\n"
    "        }\n"
    "    }\n"
    "}\n".
