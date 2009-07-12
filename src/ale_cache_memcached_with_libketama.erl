%%% http://github.com/cstar/merle

-module(ale_cache_memcached_with_libketama).

-compile(export_all).

start_link(ServersFile) ->
    application:load(merle),
    application:set_env(merle, file, ServersFile),
    application:start(merle),
    ignore.

r(Key, Fun, Options) -> ale_cache_memcached:r(Key, Fun, Options).
u(Key, Fun, Options) -> ale_cache_memcached:u(Key, Fun, Options).
