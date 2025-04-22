-module(notif_event).
-author("Zaryn Technologies").

-include_lib("kernel/include/logger.hrl").
-include("../../records.hrl").

-behaviour(gen_event).

-export([
    start_link/0, subscribe/1,
    welcome/2, follow/3, mention/3, chat/3, notif/2,
    get_notif/1, get_notif_message/1, get_all_notifs/1,
    get_notif_time/1, get_five_latest_notif_ids/1,
    get_five_latest_notif_messages/1
]).

-export([
    init/1, handle_event/2, handle_call/2,
    handle_info/2, terminate/2, code_change/3
]).

-record(state, {subscribers = []}).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

subscribe(Pid) ->
    gen_event:call(?MODULE, {subscribe, Pid}).

welcome(UserID, Message) ->
    wrap_db_call(fun() -> notifdb:welcome(UserID, Message) end).

follow(FollowerID, UserID, Message) ->
    wrap_db_call(fun() -> notifdb:follow(FollowerID, UserID, Message) end).

mention(MentionerID, UserID, Message) ->
    wrap_db_call(fun() -> notifdb:mention(MentionerID, UserID, Message) end).

chat(SenderID, ReceiverID, Message) ->
    wrap_db_call(fun() -> notifdb:chat(SenderID, ReceiverID, Message) end).

notif(UserID, Message) ->
    wrap_db_call(fun() -> notifdb:insert(UserID, Message) end).

get_notif(NotifId) ->
    wrap_db_call(fun() -> notifdb:get_single_notif(NotifId) end).

get_notif_message(NotifID) ->
    wrap_db_call(fun() -> notifdb:get_notif_message(NotifID) end).

get_all_notifs(UserID) ->
    wrap_db_call(fun() -> notifdb:get_all_notifs(UserID) end).

get_notif_time(NotifID) ->
    wrap_db_call(fun() -> notifdb:get_notif_time(NotifID) end).

get_five_latest_notif_ids(UserID) ->
    wrap_db_call(fun() -> notifdb:get_five_latest_notif_ids(UserID) end).

get_five_latest_notif_messages(UserID) ->
    wrap_db_call(fun() -> notifdb:get_five_latest_notif_messages(UserID) end).

wrap_db_call(Fun) ->
    case Fun() of
        Result when is_tuple(Result), element(1, Result) =:= error ->
            error_logger:error_msg("***Error in notif*** ~p~n", [element(2, Result)]),
            Result;
        Other ->
            Other
    end.

init([]) ->
    ?LOG_NOTICE("Notification event has been started - ~p", [self()]),
    {ok, #state{}}.

handle_event({welcome, UserId, Message}, State) ->
    handle_db_event(fun() -> notifdb:welcome(UserId, Message) end, State);

handle_event({follow, FollowerID, UserId, Message}, State) ->
    handle_db_event(fun() -> notifdb:follow(FollowerID, UserId, Message) end, State);

handle_event({mention, MentionerID, UserId, Message}, State) ->
    handle_db_event(fun() -> notifdb:mention(MentionerID, UserId, Message) end, State);

handle_event({chat, SenderID, ReceiverId, Message}, State) ->
    handle_db_event(fun() -> notifdb:chat(SenderID, ReceiverId, Message) end, State);

handle_event({notif, UserId, Message}, State) ->
    handle_db_event(fun() -> notifdb:insert(UserId, Message) end, State);

handle_event({get_notif, NotifId}, State) ->
    handle_db_result(fun() -> notifdb:get_single_notif(NotifId) end, State);

handle_event({get_notif_message, NotifID}, State) ->
    handle_db_result(fun() -> notifdb:get_notif_message(NotifID) end, State);

handle_event(Unknown, State) ->
    io:format("***Error in handle_event*** ~p~n", [Unknown]),
    {ok, State}.

handle_db_event(Fun, State) ->
    case Fun() of
        {ok, Result} -> {ok, Result, State};
        {error, Reason} ->
            error_logger:error_msg("***Error in handle_event*** ~p~n", [Reason]),
            {error, State}
    end.

handle_db_result(Fun, State) ->
    case Fun() of
        Result when is_tuple(Result), element(1, Result) =:= error ->
            error_logger:error_msg("***Error in handle_event*** ~p~n", [element(2, Result)]),
            {error, State};
        Other ->
            {ok, Other, State}
    end.

handle_call({subscribe, Pid}, State) ->
    case lists:member(Pid, State#state.subscribers) of
        true ->
            {ok, "Already subscribed", State};
        false ->
            {ok, "Subscribed successfully", State#state{subscribers = [Pid | State#state.subscribers]}}
    end.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
