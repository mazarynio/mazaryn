-module(solana_auth).
-author("Zaryn Technologies").

-export([
    register/2,
    register/3,
    login/2,
    login/3,
    logout/2,
    logout_all/1,
    verify_token/1,
    get_sessions/1,
    get_me/1,
    get_security_status/1
]).

-define(SOLANA_API_BASE, "http://localhost:3020").
-define(DEFAULT_TIMEOUT, 30000).

register(Username, Password) ->
    register(Username, Password, undefined).

register(Username, Password, Email) ->
    RequestBody0 = #{
        username => ensure_binary(Username),
        password => ensure_binary(Password)
    },

    RequestBody = case Email of
        undefined -> RequestBody0;
        _ -> maps:put(email, ensure_binary(Email), RequestBody0)
    end,

    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/auth/register",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            parse_auth_response(ResponseBody);
        {ok, {{_, 409, _}, _, _}} ->
            {error, username_already_exists};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limited};
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

login(Username, Password) ->
    login(Username, Password, undefined).

login(Username, Password, TwoFAToken) ->
    RequestBody0 = #{
        username => ensure_binary(Username),
        password => ensure_binary(Password)
    },

    RequestBody = case TwoFAToken of
        undefined -> RequestBody0;
        _ -> maps:put(two_fa_token, ensure_binary(TwoFAToken), RequestBody0)
    end,

    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/auth/login",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"requires_2fa">> := true} = Response ->
                    {ok, #{
                        requires_2fa => true,
                        message => binary_to_list(maps:get(<<"message">>, Response, <<"2FA required">>))
                    }};
                #{<<"user_id">> := UserId, <<"username">> := User, <<"token">> := Token} = Response ->
                    {ok, #{
                        user_id => binary_to_list(UserId),
                        username => binary_to_list(User),
                        token => binary_to_list(Token),
                        expires_in => maps:get(<<"expires_in">>, Response),
                        session_id => binary_to_list(maps:get(<<"session_id">>, Response, <<"">>))
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, invalid_credentials};
        {ok, {{_, 403, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limited};
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

logout(Token, SessionId) ->
    RequestBody = #{session_id => ensure_binary(SessionId)},
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/auth/logout",
    Headers = auth_headers(Token),

    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"success">> := true} ->
                    {ok, logged_out};
                _ ->
                    {error, logout_failed}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limited};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

logout_all(Token) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/auth/logout-all",
    Headers = auth_headers(Token),

    case httpc:request(post, {ApiUrl, Headers, "application/json", "{}"},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            {ok, all_sessions_terminated};
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limited};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

verify_token(Token) ->
    RequestBody = #{token => ensure_binary(Token)},
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/auth/verify",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"valid">> := true} = Response ->
                    {ok, #{
                        valid => true,
                        user_id => binary_to_list(maps:get(<<"user_id">>, Response)),
                        username => binary_to_list(maps:get(<<"username">>, Response)),
                        session_id => binary_to_list(maps:get(<<"session_id">>, Response, <<"">>)),
                        expires_at => maps:get(<<"expires_at">>, Response)
                    }};
                _ ->
                    {error, invalid_token}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, invalid_token};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limited};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

get_sessions(Token) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/auth/sessions",
    Headers = auth_headers(Token),

    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"sessions">> := Sessions} = Response ->
                    ParsedSessions = lists:map(fun(S) ->
                        #{
                            session_id => binary_to_list(maps:get(<<"session_id">>, S)),
                            ip_address => case maps:get(<<"ip_address">>, S, null) of
                                null -> undefined;
                                IP -> binary_to_list(IP)
                            end,
                            created_at => maps:get(<<"created_at">>, S),
                            last_activity => maps:get(<<"last_activity">>, S),
                            expires_at => maps:get(<<"expires_at">>, S)
                        }
                    end, Sessions),
                    {ok, #{
                        user_id => binary_to_list(maps:get(<<"user_id">>, Response)),
                        sessions => ParsedSessions,
                        total => maps:get(<<"total">>, Response)
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limited};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

get_me(Token) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/auth/me",
    Headers = auth_headers(Token),

    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"user_id">> := UserId} = Response ->
                    {ok, #{
                        user_id => binary_to_list(UserId),
                        username => binary_to_list(maps:get(<<"username">>, Response)),
                        email => case maps:get(<<"email">>, Response, null) of
                            null -> undefined;
                            Email -> binary_to_list(Email)
                        end,
                        created_at => maps:get(<<"created_at">>, Response),
                        last_login => maps:get(<<"last_login">>, Response, undefined),
                        two_fa_enabled => maps:get(<<"two_fa_enabled">>, Response)
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limited};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

get_security_status(Token) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/auth/security-status",
    Headers = auth_headers(Token),

    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"active_sessions">> := Sessions} = Response ->
                    {ok, #{
                        two_fa_enabled => maps:get(<<"two_fa_enabled">>, Response),
                        active_sessions => Sessions,
                        failed_login_attempts_last_hour => maps:get(<<"failed_login_attempts_last_hour">>, Response),
                        account_locked => maps:get(<<"account_locked">>, Response),
                        password_age_days => maps:get(<<"password_age_days">>, Response, undefined),
                        last_login => maps:get(<<"last_login">>, Response, undefined),
                        security_recommendations => lists:map(
                            fun binary_to_list/1,
                            maps:get(<<"security_recommendations">>, Response, [])
                        )
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limited};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

parse_auth_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"user_id">> := UserId, <<"username">> := Username, <<"token">> := Token} = Response ->
                {ok, #{
                    user_id => binary_to_list(UserId),
                    username => binary_to_list(Username),
                    token => binary_to_list(Token),
                    expires_in => maps:get(<<"expires_in">>, Response),
                    session_id => binary_to_list(maps:get(<<"session_id">>, Response, <<"">>))
                }};
            _ ->
                {error, invalid_response}
        end
    catch
        _:_ ->
            {error, parse_failed}
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
        _:_ ->
            {error, invalid_error_response}
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
