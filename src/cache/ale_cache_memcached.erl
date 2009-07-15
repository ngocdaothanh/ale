%%% http://github.com/joewilliams/merle

-module(ale_cache_memcached).

-compile(export_all).

start_link(Host, Port) -> merle:connect(Host, Port).

r(Key) ->
    case merle:getkey(Key) of
        undefined ->
            log4erl:debug("Cache Miss: ~p", [Key]),
            not_found;

        Value ->
            log4erl:debug("Cache Hit: ~p", [Key]),
            {ok, Value}
    end.

r(Key, Fun, Options) ->
    case merle:getkey(Key) of
        undefined ->
            log4erl:debug("Cache Miss: ~p", [Key]),
            w(Key, Fun, Options);

        Value -> Value
    end.

w(Key, Fun, Options) ->
    Value = ale_cache:value(Fun, Options),
    log4erl:debug("Cache Write: ~p", [Key]),
    merle:set(Key, Value),
    Value.
