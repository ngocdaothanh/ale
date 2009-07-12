-module(ale_cache_etscached).

-compile(export_all).

start_link(Nodes, Mem) ->
    etscached:start_link(Nodes, Mem).

r(Key, Fun, Options) ->
    Fun2 = fun() -> ale_cache:value(Fun, Options) end,
    etscached:r(Key, Fun2).

u(Key, Fun, Options) ->
    Fun2 = fun() -> ale_cache:value(Fun, Options) end,
    etscached:u(Key, Fun2).
