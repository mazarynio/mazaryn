-module(content_cache).
-author("Zaryn Technologies").
-export([
    init/0, 
    start_link/0,
    set/2, 
    set/3,
    get/1, 
    delete/1, 
    flush/0,
    get_stats/0
]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CACHE_TABLE, content_cache_table).
-define(DEFAULT_CACHE_LIFETIME, 600).  
-define(CLEANUP_INTERVAL, 60000).      
-define(MAX_CACHE_SIZE, 10000000).     
-define(MEMORY_LIMIT, 1073741824).     

%% API Functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init() ->
    application:ensure_all_started(content_cache),
    ok.

set(Key, Value) ->
    set(Key, Value, ?DEFAULT_CACHE_LIFETIME).

set(Key, Value, TTL) when is_integer(TTL), TTL > 0 ->
    ValueSize = size_of(Value),
    if
        ValueSize > ?MAX_CACHE_SIZE ->
            {error, content_too_large};
        true ->
            Now = erlang:system_time(second),
            Expiry = Now + TTL,
            ets:insert(?CACHE_TABLE, {Key, Value, Expiry}),
            ok
    end.

get(Key) ->
    try ets:lookup(?CACHE_TABLE, Key) of
        [{_, Value, Expiry}] ->
            Now = erlang:system_time(second),
            if 
                Expiry > Now -> 
                    Value;
                true -> 
                    ets:delete(?CACHE_TABLE, Key),
                    undefined
            end;
        [] ->
            undefined
    catch
        error:badarg ->
            undefined
    end.

delete(Key) ->
    gen_server:cast(?SERVER, {delete, Key}).

flush() ->
    gen_server:cast(?SERVER, flush).

get_stats() ->
    gen_server:call(?SERVER, get_stats).

init([]) ->
    process_flag(trap_exit, true),
    TableOptions = [
        named_table, 
        public, 
        {read_concurrency, true},
        {write_concurrency, true}
    ],
    
    Table = case ets:info(?CACHE_TABLE) of
        undefined -> 
            ets:new(?CACHE_TABLE, TableOptions);
        _ -> 
            ?CACHE_TABLE
    end,
    
    Stats = #{
        hits => 0,
        misses => 0,
        memory_used => 0,
        items => 0,
        evictions => 0
    },
    
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    
    {ok, #{
        table => Table,
        stats => Stats,
        memory_used => 0
    }}.

handle_call({set, Key, Value, TTL}, _From, State = #{table := Table, stats := Stats, memory_used := MemoryUsed}) ->
    Now = erlang:system_time(second),
    Expiry = Now + TTL,
    ValueSize = size_of(Value),
    
    {NewMemoryUsed, NewStats} = case MemoryUsed + ValueSize > ?MEMORY_LIMIT of
        true ->
            {EvictedSize, EvictionCount} = evict_entries(Table, ValueSize),
            {
                MemoryUsed - EvictedSize + ValueSize,
                Stats#{evictions => maps:get(evictions, Stats, 0) + EvictionCount}
            };
        false ->
            {MemoryUsed + ValueSize, Stats}
    end,
    
    ets:insert(Table, {Key, Value, Expiry}),
    
    UpdatedStats = NewStats#{
        items => maps:get(items, NewStats, 0) + 1,
        memory_used => NewMemoryUsed
    },
    
    {reply, ok, State#{stats := UpdatedStats, memory_used := NewMemoryUsed}};

handle_call(get_stats, _From, State = #{stats := Stats}) ->
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({delete, Key}, State = #{table := Table, stats := Stats, memory_used := MemoryUsed}) ->
    ValueSize = case ets:lookup(Table, Key) of
        [{_, Value, _}] -> size_of(Value);
        _ -> 0
    end,
    
    ets:delete(Table, Key),
    
    NewMemoryUsed = max(0, MemoryUsed - ValueSize),
    NewStats = Stats#{
        items => max(0, maps:get(items, Stats, 0) - 1),
        memory_used => NewMemoryUsed
    },
    
    {noreply, State#{stats := NewStats, memory_used := NewMemoryUsed}};

handle_cast(flush, State = #{table := Table}) ->
    ets:delete_all_objects(Table),
    
    NewStats = #{
        hits => 0,
        misses => 0,
        memory_used => 0,
        items => 0,
        evictions => 0
    },
    
    {noreply, State#{stats := NewStats, memory_used := 0}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(cleanup, State = #{table := Table, stats := Stats, memory_used := MemoryUsed}) ->
    Now = erlang:system_time(second),
    
    ExpiredEntries = ets:select(Table, [
        {{'$1', '$2', '$3'}, [{'<', '$3', {const, Now}}], [['$1', '$2']]}
    ]),
    
    ExpiredSize = lists:foldl(
        fun([Key, Value], Acc) ->
            ets:delete(Table, Key),
            Acc + size_of(Value)
        end,
        0,
        ExpiredEntries
    ),
    
    NewMemoryUsed = max(0, MemoryUsed - ExpiredSize),
    ExpiredCount = length(ExpiredEntries),
    NewStats = Stats#{
        items => max(0, maps:get(items, Stats, 0) - ExpiredCount),
        memory_used => NewMemoryUsed
    },
    
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    
    {noreply, State#{stats := NewStats, memory_used := NewMemoryUsed}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

size_of(Term) ->
    try
        erts_debug:flat_size(Term) * erlang:system_info(wordsize)
    catch
        _:_ ->
            byte_size(term_to_binary(Term))
    end.

evict_entries(Table, RequiredSize) ->
    AllEntries = ets:tab2list(Table),
    
    SortedEntries = lists:sort(
        fun({_, _, Expiry1}, {_, _, Expiry2}) ->
            Expiry1 =< Expiry2
        end,
        AllEntries
    ),
    
    evict_entries_loop(Table, SortedEntries, RequiredSize, 0, 0).

evict_entries_loop(_, [], _, TotalFreed, Count) ->
    {TotalFreed, Count};
evict_entries_loop(Table, [{Key, Value, _} | Rest], RequiredSize, TotalFreed, Count) ->
    ValueSize = size_of(Value),
    ets:delete(Table, Key),
    NewTotalFreed = TotalFreed + ValueSize,
    
    case NewTotalFreed >= RequiredSize of
        true ->
            {NewTotalFreed, Count + 1};
        false ->
            evict_entries_loop(Table, Rest, RequiredSize, NewTotalFreed, Count + 1)
    end.