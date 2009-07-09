-module(ale_cache_memcached).

-compile(export_all).

start_link(Host, Port) -> merle:connect(Host, Port).

%% See ale_cache:cache/3.
cache(Key, Fun, Options) ->
    case merle:getkey(Key) of
        undefined ->
            error_logger:info_msg("Cache Miss: ~p", [Key]),
            Value = ale_cache:value(Fun, Options),
            merle:set(Key, Value),
            Value;

        Value ->
            error_logger:info_msg("Cache Hit: ~p", [Key]),
            Value
    end.
    