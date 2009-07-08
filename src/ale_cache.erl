-module(ale_cache).

-compile(export_all).

-include("ale.hrl").

start_link(SC) ->
    case proplists:get_value("cache_server", SC#sconf.opaque) of
        undefined -> ignore;

        Config ->
            case string:tokens(Config, ":") of
                ["memcached", Host, Port] ->
                    Port2 = list_to_integer(Port),
                    merle:connect(Host, Port2);

                ["ets"] ->
                    ale_cache_server:start_link();

                _ -> ingore
            end
    end.

cache(Key, Fun) -> cache(Key, Fun, []).

cache(Key, Fun, Options) ->
    case whereis(ale_cache_server) of
        undefined ->
            case whereis(merle) of
                undefined -> Fun();

                _ ->
                    case merle:getkey(Key) of
                        undefined ->
                            error_logger:info_msg("Cache Miss: ~p", [Key]),
                            Value = Fun(),
                            merle:set(Key, Value),
                            Value;

                        Value ->
                            error_logger:info_msg("Cache Hit: ~p", [Key]),
                            Value
                    end
            end;

        _ -> ale_cache_server:cache(Key, Fun, Options)
    end.
