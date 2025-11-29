-module(notebook_client).
-author("Zaryn Technologies").

-export([
    create_session/3,
    execute_code/3,
    close_session/1,
    get_session_status/1
]).

-define(RUST_API_BASE, "http://localhost:2020").
-define(DEFAULT_TIMEOUT, 120000).

create_session(NotebookId, UserId, Language) ->
    RequestBody = #{
        notebook_id => ensure_binary(NotebookId),
        user_id => ensure_binary(UserId),
        language => language_to_atom(Language)
    },

    Json = jsx:encode(RequestBody),
    ApiUrl = ?RUST_API_BASE ++ "/notebooks/sessions",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"session_id">> := SessionId, <<"kernel_id">> := KernelId} ->
                    {ok, binary_to_list(SessionId), binary_to_list(KernelId)};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

execute_code(SessionId, Code, CellId) ->
    RequestBody = #{
        session_id => ensure_binary(SessionId),
        code => ensure_binary(Code),
        cell_id => ensure_binary(CellId)
    },

    Json = jsx:encode(RequestBody),
    ApiUrl = ?RUST_API_BASE ++ "/notebooks/execute",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Result when is_map(Result) ->
                    {ok, parse_execution_result(Result)};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

close_session(SessionId) ->
    ApiUrl = ?RUST_API_BASE ++ "/notebooks/sessions/" ++ ensure_string(SessionId),

    case httpc:request(delete, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_session_status(SessionId) ->
    ApiUrl = ?RUST_API_BASE ++ "/notebooks/sessions/" ++ ensure_string(SessionId),

    case httpc:request(get, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Session when is_map(Session) ->
                    {ok, parse_session(Session)};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

parse_execution_result(Result) ->
    #{
        execution_id => binary_to_list(maps:get(<<"execution_id">>, Result)),
        status => parse_status(maps:get(<<"status">>, Result)),
        outputs => parse_outputs(maps:get(<<"outputs">>, Result, [])),
        execution_time_ms => maps:get(<<"execution_time_ms">>, Result),
        error => parse_error(maps:get(<<"error">>, Result, null))
    }.

parse_status(<<"Success">>) -> success;
parse_status(<<"Error">>) -> error;
parse_status(<<"Timeout">>) -> timeout;
parse_status(<<"Cancelled">>) -> cancelled;
parse_status(_) -> unknown.

parse_outputs(Outputs) when is_list(Outputs) ->
    [parse_output(O) || O <- Outputs];
parse_outputs(_) ->
    [].

parse_output(Output) ->
    #{
        output_type => parse_output_type(maps:get(<<"output_type">>, Output)),
        data => parse_output_data(maps:get(<<"data">>, Output)),
        metadata => maps:get(<<"metadata">>, Output, #{})
    }.

parse_output_type(<<"Stream">>) -> stream;
parse_output_type(<<"DisplayData">>) -> display_data;
parse_output_type(<<"ExecuteResult">>) -> execute_result;
parse_output_type(<<"Error">>) -> error;
parse_output_type(_) -> unknown.

parse_output_data(Data) when is_map(Data) ->
    case maps:keys(Data) of
        [<<"Text">>] -> {text, binary_to_list(maps:get(<<"Text">>, Data))};
        [<<"Html">>] -> {html, binary_to_list(maps:get(<<"Html">>, Data))};
        [<<"Json">>] -> {json, maps:get(<<"Json">>, Data)};
        _ -> {unknown, Data}
    end;
parse_output_data(Data) ->
    {unknown, Data}.

parse_error(null) -> undefined;
parse_error(Error) when is_map(Error) ->
    #{
        error_name => binary_to_list(maps:get(<<"error_name">>, Error)),
        error_value => binary_to_list(maps:get(<<"error_value">>, Error)),
        traceback => [binary_to_list(T) || T <- maps:get(<<"traceback">>, Error, [])]
    };
parse_error(_) -> undefined.

parse_session(Session) ->
    #{
        session_id => binary_to_list(maps:get(<<"session_id">>, Session)),
        notebook_id => binary_to_list(maps:get(<<"notebook_id">>, Session)),
        user_id => binary_to_list(maps:get(<<"user_id">>, Session)),
        kernel_id => binary_to_list(maps:get(<<"kernel_id">>, Session)),
        status => parse_session_status(maps:get(<<"status">>, Session))
    }.

parse_session_status(<<"Active">>) -> active;
parse_session_status(<<"Idle">>) -> idle;
parse_session_status(<<"Busy">>) -> busy;
parse_session_status(<<"Terminated">>) -> terminated;
parse_session_status(_) -> unknown.

language_to_atom(python) -> <<"Python">>;
language_to_atom(r) -> <<"R">>;
language_to_atom(julia) -> <<"Julia">>;
language_to_atom(scala) -> <<"Scala">>;
language_to_atom(sql) -> <<"SQL">>;
language_to_atom(Language) when is_binary(Language) -> Language;
language_to_atom(Language) when is_list(Language) -> list_to_binary(Language).

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8).

ensure_string(Value) when is_list(Value) -> Value;
ensure_string(Value) when is_binary(Value) -> binary_to_list(Value);
ensure_string(Value) when is_atom(Value) -> atom_to_list(Value).
