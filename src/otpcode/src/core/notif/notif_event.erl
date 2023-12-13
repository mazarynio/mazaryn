-module(notif_event).
-author("Zaryn Technologies").
-include_lib("kernel/include/logger.hrl").
-include("../../records.hrl").
-behaviour(gen_event).
-export([start_link/0, subscribe/1, welcome/2, follow/2, notif/2, get_notif/1, get_notif_message/1,
get_all_notifs/1]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {subscribers = []}).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

subscribe(Pid) ->
    gen_event:call(?MODULE, {subscribe, Pid}).

welcome(UserID, Message) ->
    case notifdb:welcome(UserID, Message) of 
        Id ->
            Id;
        {error, Reason} ->
            error_logger:error_msg("***Error in notif*** ~p~n", [Reason]),
            {error, Reason}
    end.

follow(UserID, Message) ->
    case notifdb:follow(UserID, Message) of 
        Id ->
            Id;
        {error, Reason} ->
            error_logger:error_msg("***Error in notif*** ~p~n", [Reason]),
            {error, Reason}
    end.

notif(UserID, Message) ->
    case notifdb:insert(UserID, Message) of
        NotifId -> 
            NotifId;
        {error, Reason} ->
            error_logger:error_msg("***Error in notif*** ~p~n", [Reason]),
            {error, Reason}
    end.

get_notif(NotifId) ->
    case notifdb:get_notif_message(NotifId) of
        Notif ->
            Notif;
        {error, Reason} ->
            error_logger:error_msg("***Error in get_notif*** ~p~n", [Reason]),
            {error, Reason}
    end.

get_notif_message(NotifID) -> 
    case notifdb:get_notif_message(NotifID) of 
        Message ->
            Message;
        {error, Reason} ->
            error_logger:error_msg("***Error in notif*** ~p~n", [Reason]),
            {error, Reason}
    end.

get_all_notifs(UserID) ->
    case notifdb:get_all_notifs(UserID) of
        Notifications ->
            Notifications;
        {error, _Reason} ->
            error_logger:error_msg("***Error in notif*** ~p~n", [_Reason]),
            {error, _Reason}
    end.

init([]) ->
    ?LOG_NOTICE("Notification event has been started - ~p", [self()]),
    State = #state{},
    {ok, State}.

handle_event({welcome, UserId, Message}, State) ->
    case notifdb:welcome(UserId, Message) of
        {ok, Message} -> 
            {ok, Message, State};
        {error, Reason} ->
            error_logger:error_msg("***Error in handle_event*** ~p~n", [Reason]),
            {error, State}
    end;

handle_event({follow, UserId, Message}, State) ->
    case notifdb:follow(UserId, Message) of
        {ok, Id} -> 
            {ok, Id, State};
        {error, Reason} ->
            error_logger:error_msg("***Error in handle_event*** ~p~n", [Reason]),
            {error, State}
    end;

handle_event({notif, UserId, Message}, State) ->
    case notifdb:insert(UserId, Message) of
        {ok, NotifId} -> 
            {ok, NotifId, State};
        {error, Reason} ->
            error_logger:error_msg("***Error in handle_event*** ~p~n", [Reason]),
            {error, State}
    end;

handle_event({get_notif, NotifId}, State) ->
    case notifdb:get_notif_message(NotifId) of
        Notif ->
            Notif;
        {error, Reason} ->
            error_logger:error_msg("***Error in handle_event*** ~p~n", [Reason]),
            {error, State}
    end;

handle_event({get_notif_message, NotifID}, State) ->
    case notifdb:get_notif_message(NotifID) of
        Message ->
            Message;
        {error, Reason} ->
            error_logger:error_msg("***Error in handle_event*** ~p~n", [Reason]),
            {error, State}
    end;

handle_event(ErrorMsg, State) ->
    io:format("***Error in handle_event*** ~p~n", [ErrorMsg]),
    {ok, State}.

handle_call({subscribe, Pid}, State) ->
    subscribe(Pid, State).

% Subscription confirmation
subscribe(Pid, State) ->
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
