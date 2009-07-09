% Taken from Nitrogen
%
% Copyright Rusty Klophaus under MIT license

-module(ale_cache_ets).

-behaviour(gen_server).

-export ([
    cache/3,

    start_link/0,
    init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-record(cacheitem, {key, value, options, pid}).

cache(Key, Fun, Options) ->
    case gen_server:call(?MODULE, {read, Key}, infinity) of
        [CacheItem] ->
            error_logger:info_msg("Cache Hit: ~p", [Key]),
            return_cache_item(CacheItem);

        _ ->
            error_logger:info_msg("Cache Miss: ~p", [Key]),
            recache(Key, Fun, Options)
    end.

%% Check the options to see if this is a sliding cache. If so, then signal a
%% cache hit.
return_cache_item(CacheItem) ->
    % Sliding Cache? Signal cache hit...
    Options = CacheItem#cacheitem.options,
    case proplists:get_value(slide, Options) of
        false -> ignore;

        _ ->
            Pid = CacheItem#cacheitem.pid,
            Pid ! cache_hit
    end,
    
    % Return the item.
    CacheItem#cacheitem.value.  

%% Write to the cache table.
recache(Key, Fun, Options) ->
    Value = ale_cache:value(Fun, Options),

    % Get the time to live in milliseconds
    TTL = case proplists:get_value(ttl, Options) of
        infinity  -> infinity;
        undefined -> infinity;
        Seconds   -> Seconds*1000
    end,

    % Spawn the cache monitor function
    Pid = erlang:spawn(fun() -> cache_loop(Key, TTL) end),

    % Write to the cache table
    CacheItem = #cacheitem{key = Key, value = Value, options = Options, pid = Pid},
    gen_server:cast(?MODULE, {write, CacheItem}),

    Value.

%% If there is a cache-hit, then restart the timer. 
%% Otherwise, kill the cache after the time to live (TTL) has expired.
cache_loop(Key, TTL) ->
    receive 
        cache_hit -> cache_loop(Key, TTL)
    after TTL ->
        gen_server:cast(?MODULE, {delete, Key})
    end.

%-------------------------------------------------------------------------------

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    Table = ets:new(cache_server, [public, set, {keypos, 2}]),
    {ok, Table}.

handle_call({read, Key}, _From, Table) -> 
    Result = ets:lookup(Table, Key),
    {reply, Result, Table}.

handle_cast({write, Tuple}, Table) ->
    ets:insert(Table, Tuple),
    {noreply, Table};

handle_cast({delete, Key}, Table) ->
    ets:delete(Table, Key),
    {noreply, Table}.

handle_info(Info, Table) ->
    {stop, {unexpected_message, Info}, Table}.

terminate(_Reason, Table) ->
    ets:delete(Table).

code_change(_OldVsn, State, _Extra) -> {ok, State}.
