%%%-------------------------------------------------------------------
%%% @author tandathuynh148
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Nov 2022 1:10 PM
%%%-------------------------------------------------------------------
-module(user_reg).

-behaviour(gen_statem).

%% API
-export([start_link/2]).

%% state callback functions
-export([send_mailjet/2]).

-export([send_token/3, waiting_for_rep/3,
         token_validated/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, terminate/3,
         code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).
-define(MAX_RETRIES, 4).
-define(RANDOM_TOKEN, 45612).
-define(DEFAULT_TIMEOUT, 60*1000*10).

-define(MAILJET_API_KEY, os:getenv("MAILJET_APIKEY")).
-define(MAILJET_SECRET_KEY, os:getenv("MAILJET_SECRETKEY")).

-record(data, {username,
               email,
               token,
               max_retries = 0,
               init_time}).



%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(Username, Email) ->
  gen_statem:start_link({global, Email}, ?MODULE, {Username, Email}, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init({Username, Email}) ->
  Token = lists:concat(id_gen:random_numbers()),
  Data = #data{username = Username,
               email = Email,
               token = Token,
               init_time = erlang:system_time(second)},
  {ok, send_token, Data, {next_event, internal, []}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
send_token(internal, [], #data{token=Token, email = Email}=Data) ->
  send_mailjet(Email, Token),
  {next_state, waiting_for_rep, Data, {next_event, internal, []}}.

waiting_for_rep(internal, [], _Data) ->
  {keep_state_and_data, {state_timeout, ?DEFAULT_TIMEOUT, timeout}};

waiting_for_rep(_EventType, {token_resp, _TokenRep}, Data=#data{max_retries = ?MAX_RETRIES, email= Email}) ->
  ets:delete(user_reg, Email),
  {stop, normal};
waiting_for_rep(_EventType, {token_resp, TokenRep}, Data=#data{username = Username,
                                                               email = Email,
                                                               max_retries = MaxRetries,
                                                               token = Token}) ->
  case TokenRep of
    Token ->
      %% update user verify field here
      userdb:set_user_info(Username, [verified], [true]),
      ets:delete(user_reg, Email),
      {next_state, token_validated, Data, {next_event, internal, completed}};
    _ ->
      %% wrong token
      SpendTime = erlang:system_time(seconds) - Data#data.init_time,
      {keep_state,
       Data#data{max_retries = MaxRetries+1},
       {state_timeout, ?DEFAULT_TIMEOUT - SpendTime*1000, timeout}}
  end;

waiting_for_rep(state_timeout, timeout, #data{email = Email}) ->
  ets:delete(user_reg, Email),
  {stop, normal}.

token_validated(internal, completed, _Data) ->
  {stop, normal}.


%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _Data) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% send mailjet api, retries 3 times if any errors
send_mailjet(UserMail, Token) ->
  lists:any(fun(_) -> send_mailjet_handler(UserMail, Token) end, [1,2,3]).

send_mailjet_handler(UserMail, Token) ->
  URL = "https://api.mailjet.com/v3.1/send",
  AuthStr = base64:encode_to_string(?MAILJET_API_KEY ++ ":" ++ ?MAILJET_SECRET_KEY),
  Headers = [{"Authorization", "Basic " ++ AuthStr}],
  ContentType = "application/json",

  %%  mailjet email template
  From = {<<"From">>, {[{<<"Email">>, <<"support@mazaryn.io">>}, {<<"NAME">>, <<"Mazaryn">>}]}},
  To = {<<"To">>, [{[{<<"Email">>, list_to_binary(UserMail)}]}]},

  Subject = {<<"Subject">>, <<"Verification Token from Mazaryn">>},
  TextPart = {<<"TextPart">>, <<"Greetings from Mazaryn!">>},
  HTMLStr = io_lib:format("<h3>Welcome to <a href=\"https://www.mazaryn.io/\">Mazaryn</a>!</h3><h3>Here is your token: ~p. It will expire in 10 minutes.<h3>", [Token]),
  BrStr = "<br />Complete the last step to explore the social network!!",
  HTMLPart = {<<"HTMLPArt">>, list_to_binary(HTMLStr++BrStr)},
  MailFormat = {[
    {<<"Messages">>, [{[From, To, Subject, TextPart, HTMLPart]}]}
  ]},
  JSONMail = jiffy:encode(MailFormat),

  %%  {url(), headers(), content_type(), body()}
  Request = {URL, Headers, ContentType, JSONMail},
  Method = post,
  Res = httpc:request(Method, Request, [], []),
  check_http_res(Res).

check_http_res({ok, Result}) ->
  {{_, Code, Desc}, _, _} = Result,
  case Code of
    200 -> true;
    _ -> false %% try again
  end;
check_http_res({error, _Error}) ->
  false.
