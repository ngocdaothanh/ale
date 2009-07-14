%%% http://github.com/joewilliams/merle

-module(ale_cache_memcached).

-compile(export_all).

start_link(Host, Port) -> merle:connect(Host, Port).

r(Key) ->
    case merle:getkey(Key) of
        undefined -> not_found;
        Value     -> {ok, Value}
    end.

r(Key, Fun, Options) ->
    case merle:getkey(Key) of
        undefined ->
            error_logger:info_msg("Cache Miss: ~p", [Key]),
            w(Key, Fun, Options);

        Value -> Value
    end.

w(Key, Fun, Options) ->
    Value = ale_cache:value(Fun, Options),
    merle:set(Key, Value),
    Value.
