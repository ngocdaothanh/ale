%%% This module is the interface to various cache impelemtations.
%%%
%%% NOTE for implementations:
%%%
%%% The passed Fun may access the current process dictionary, thus it should be
%%% run right in the calling process, not in something like gen_server.
%%%
%%% error_logger does not support log level, thus "cache hit" message should
%%% not be logged, e.g. error_logger:info_msg("Cache Hit: ~p", [Key])
%%% to avoid IO bottle neck.
%%%
%%% Assumptions:
%%% * Memory only, even for page cache
%%% * At least 100MB should be used
%%% * LRU is a must, no TTL is needed

-module(ale_cache).

-compile(export_all).

-include("ale.hrl").

start_link(SC, Nodes) ->
    case proplists:get_value("cache", SC#sconf.opaque) of
        undefined -> ignore;

        Config ->
            case string:tokens(Config, ":") of
                ["cherly", SizeInMB] ->
                    ale_cache_cherly:start_link(list_to_integer(SizeInMB));

                ["memcached", Host, Port] ->
                    ale_cache_memcached:start_link(Host, list_to_integer(Port));

                ["memcached_with_libketama", ServersFile] ->
                    ale_cache_memcached_with_libketama:start_link(ServersFile);

                _ ->
                    ingore
            end
    end.

%-------------------------------------------------------------------------------

c(Key) ->
    Module = cache_module(),
    Module:r(Key).

c(Key, Fun) -> c(Key, Fun, []).
c(Key, Fun, Options) ->
    Options2 = case is_list(Options) of
        false -> [Options];
        true  -> Options
    end,

    ROrW = case proplists:get_value(w, Options2) of
        true -> w;
        _    -> r
    end,

    Module = cache_module(),
    Module:ROrW(Key, Fun, Options2).

%-------------------------------------------------------------------------------

%% Used by implementations to get the value to cache.
value(Fun, Options) ->
    case proplists:get_value(ehtmle, Options) of
        true ->
            Ehtmle = Fun(),
            yaws_api:ehtml_expander(Ehtmle);

        _ ->
            case proplists:get_value(ehtml, Options) of
                true ->
                    Ehtml = Fun(),
                    IoList = yaws_api:ehtml_expand(Ehtml),
                    list_to_binary(IoList);

                _ ->
                    case proplists:get_value(iolist, Options) of
                        true ->
                            IoList = Fun(),
                            list_to_binary(IoList);

                        _ -> Fun()
                    end
            end
    end.

%% Returns the cache server module being used.
cache_module() ->
    % case whereis(ale_cache_etscached) of
    %     undefined ->
    %         case whereis(merle) of
    %             undefined -> erlang:error("Cache server not running");
    % 
    %             _ -> ale_cache_memcached:cache(Key, Fun, Options)
    %         end;
    % 
    %     _ -> ale_cache_etscached:cache(Key, Fun, Options)
    % end.
    ale_cache_cherly.
