-module(ale_cache_memcached).

-compile(export_all).

start_link(ServersFile) ->
    % Use the merle library that has libketama feature
    % application:load(merle),
    % application:set_env(merle, file, ServersFile),
    % application:start(merle),
    % ignore.
    merle:connect().

r(Key, Fun, Options) ->
    case merle:getkey(Key) of
        undefined ->
            error_logger:info_msg("Cache Miss: ~p", [Key]),
            u(Key, Fun, Options);

        Value -> Value
    end.

u(Key, Fun, Options) ->
    Value = ale_cache:value(Fun, Options),
    merle:set(Key, Value),
    Value.
