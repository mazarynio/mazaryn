-module(solana_2fa).
-author("Zaryn Technologies").

-export([
    setup/1,
    enable/3,
    disable/2,
    verify/2,
    get_status/1
]).

-define(SOLANA_API_BASE, "http://localhost:3020").
-define(DEFAULT_TIMEOUT, 30000).

setup(Token) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/2fa/setup",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", "{}"},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"secret">> := Secret, <<"qr_code">> := QRCode} = Response ->
                    {ok, #{
                        secret => binary_to_list(Secret),
                        qr_code => binary_to_list(QRCode),
                        manual_entry_key => binary_to_list(maps:get(<<"manual_entry_key">>, Response)),
                        message => binary_to_list(maps:get(<<"message">>, Response))
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

enable(Token, Secret, TwoFAToken) ->
    RequestBody = #{
        secret => ensure_binary(Secret),
        token => ensure_binary(TwoFAToken)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/2fa/enable",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"success">> := true, <<"backup_codes">> := BackupCodes} = Response ->
                    {ok, #{
                        success => true,
                        message => binary_to_list(maps:get(<<"message">>, Response)),
                        backup_codes => lists:map(fun binary_to_list/1, BackupCodes),
                        warning => binary_to_list(maps:get(<<"warning">>, Response))
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

disable(Token, TwoFAToken) ->
    RequestBody = #{token => ensure_binary(TwoFAToken)},
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/2fa/disable",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"success">> := true} = Response ->
                    {ok, #{
                        success => true,
                        message => binary_to_list(maps:get(<<"message">>, Response))
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

verify(Token, TwoFAToken) ->
    RequestBody = #{token => ensure_binary(TwoFAToken)},
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/2fa/verify",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"valid">> := Valid} = Response ->
                    {ok, #{
                        valid => Valid,
                        message => binary_to_list(maps:get(<<"message">>, Response))
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_status(Token) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/2fa/status",
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"enabled">> := Enabled} = Response ->
                    {ok, #{
                        enabled => Enabled,
                        message => binary_to_list(maps:get(<<"message">>, Response))
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

parse_error_response(ErrorBody) ->
    try
        case jsx:decode(list_to_binary(ErrorBody), [return_maps]) of
            #{<<"error">> := ErrorMsg} ->
                {error, binary_to_list(ErrorMsg)};
            _ ->
                {error, unknown_error}
        end
    catch
        _:_ -> {error, invalid_error_response}
    end.

auth_headers(Token) ->
    [{"Authorization", "Bearer " ++ ensure_string(Token)}].

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
ensure_binary(Value) -> list_to_binary(io_lib:format("~p", [Value])).

ensure_string(Value) when is_list(Value) -> Value;
ensure_string(Value) when is_binary(Value) -> binary_to_list(Value);
ensure_string(Value) when is_atom(Value) -> atom_to_list(Value);
ensure_string(Value) -> lists:flatten(io_lib:format("~p", [Value])).
