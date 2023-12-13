-module(chat_server).
-author("Zaryn Technologies").

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

%%API
-export([
    start_link/0,
    send_msg/4, send_msg_bot/2,
    get_msg/1,
    get_msg_bot/1,
    get_all_msg/1,
    get_all_msg_bot/1,
    edit_msg/2,
    delete_msg/1,
    create_chat/2,
    get_chat_by_id/1,
    get_user_chats/1,
    list_chats/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).
 %% Start Chat Server 
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

send_msg(UserID, RecipientID, Body, Media) ->
    gen_server:call({global, ?MODULE}, {send_msg, UserID, RecipientID, Body, Media}).

send_msg_bot(UserID, Body) ->
    gen_server:call({global, ?MODULE}, {send_msg_bot, UserID, Body}).

get_msg(ChatID) ->
    gen_server:call({global, ?MODULE}, {get_msg, ChatID}).

get_msg_bot(ChatID) ->
    gen_server:call({global, ?MODULE}, {get_msg_bot, ChatID}).

get_all_msg(RecipientID) ->
    gen_server:call({global, ?MODULE}, {get_all_msg, RecipientID}).

get_all_msg_bot(RecipientID) ->
    gen_server:call({global, ?MODULE}, {get_all_msg_bot, RecipientID}).

edit_msg(ChatID, NewContent) ->
    gen_server:call({global, ?MODULE}, {edit_msg, ChatID, NewContent}).

delete_msg(ChatID) ->
    gen_server:call({global, ?MODULE}, {delete_msg, ChatID}).

create_chat(Peer_Ids, Title) ->
    gen_server:call({global, ?MODULE}, {insert, Peer_Ids, Title}).

get_chat_by_id(Id) ->
    gen_server:call({global, ?MODULE}, {get_by_id, Id}).

get_user_chats(Id) ->
    gen_server:call({global, ?MODULE}, {get_user_chats, Id}).

list_chats() ->
    gen_server:call({global, ?MODULE}, list_chats).

init([]) ->
    ?LOG_NOTICE("Chat server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({send_msg, UserID, RecipientID, Body, Media}, _From, State = #state{}) ->
    Res = chatdb:send_msg(UserID, RecipientID, Body, Media),
    {reply, Res, State};
handle_call({send_msg_bot, UserID, Body}, _From, State = #state{}) ->
    Res = chatbot:send_msg(UserID, Body),
    {reply, Res, State};
handle_call({get_msg, ChatID}, _From, State) ->
    Res = chatdb:get_msg(ChatID),
    {reply, Res, State};
handle_call({get_msg_bot, ChatID}, _From, State) ->
    Res = chatbot:get_msg(ChatID),
    {reply, Res, State};
handle_call({get_all_msg, RecipientID}, _From, State) ->
    Res = chatdb:get_all_msg(RecipientID),
    {reply, Res, State};
handle_call({get_all_msg_bot, RecipientID}, _From, State) ->
    Res = chatbot:get_all_msg(RecipientID),
    {reply, Res, State};
handle_call({edit_msg, ChatID, NewContent}, _From, State) ->
    Res = chatdb:edit_msg(ChatID, NewContent),
    {reply, Res, State};
handle_call({delete_msg, ChatID}, _From, State) ->
    Res = chatdb:delete_msg(ChatID),
    {reply, Res, State};
handle_call({insert, Peer_Ids, Title}, _From, State) ->
    Id = chatdb:create_chat(Peer_Ids, Title),
    Chat = chatdb:get_chat_by_id(Id),
    {reply, Chat, State};
handle_call({get_by_id, Id}, _From, State) ->
    Chat = chatdb:get_chat_by_id(Id),
    {reply, Chat, State};
handle_call({get_user_chats, Id}, _From, State) ->
    Chats = chatdb:get_user_chats(Id),
    {reply, Chats, State};
handle_call(list_chats, _From, State) ->
    Chats = chatdb:list_chats(),
    {reply, Chats, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
