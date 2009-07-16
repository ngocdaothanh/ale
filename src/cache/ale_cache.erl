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

%% Returns {ok, Value} or not_found.
cache(Key) ->
    Module = cache_module(),
    Module:r(Key).

cache(Key, Fun) -> cache(Key, Fun, []).

%% Tries to read cache and returns value. If not found then runs Fun to get
%% value, write then returns it. Note that the return value form is different
%% from that of c/1.
%%
%% If you want to cache string or io list, for efficiency remember to set
%% Options to ask this function to convert it to binary before caching so that
%% serializing/deserializing is not performed every time the cache is read.
%%
%% Options:
%%   w                : always use Fun to write to the cache, e.g. overwrite any existing value
%%   ehtml            : the return value of Fun is EHTML and will be converted to HTML then to binary
%%   ehtmle           : the return value of Fun is EHTML and will be converted to HTML then to binary
%%   iolist           : the return value of Fun is io list and will be converted to binary
%%   {ttl, Seconds}   : time to live until cache is expired
%%   {slide, Boolean} : slide expiration if the cached object is read
cache(Key, Fun, Options) ->
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