%%% This module is the interface to various cache impelemtations.
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
                ["etscached", SizeInMB] ->
                    ale_cache_etscached:start_link(Nodes, list_to_integer(SizeInMB));

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

%% Cache implementations may use Mnesia. When applications use Mnesia and its
%% replication feature, they should call this function to know which RAM tables
%% are used by the cache server and replicate them as ram_copies properly.
%%
%% When Mnesia is used, tables should be ram_copies. Cache data is temporary and
%% it's OK if it is lost! and should be replicated to all nodes. What's the
%% point of using Mnesia if ram_copies and replication are not used!
mnesia_ram_tables() ->
    Module = cache_module(),
    case erlang:function_exported(Module, mnesia_ram_tables, 0) of
        true -> Module:mnesia_ram_tables();
        _    -> []
    end.

%-------------------------------------------------------------------------------

r(Key, Fun) -> r(Key, Fun, []).

%% If you want to cache string or io list, for efficiency remember to set
%% Options to ask this function to convert it to binary before caching so that
%% serializing/deserializing is not performed every time the cache is read.
%%
%% Options:
%%   write            : always use Fun to write to the cache, e.g. overwrite any existing value
%%   ehtml            : the return value of Fun is EHTML and will be converted to HTML then to binary
%%   iolist           : the return value of Fun is io list and will be converted to binary
%%   {ttl, Seconds}   : time to live until cache is expired
%%   {slide, Boolean} : slide expiration if the cached object is read
r(Key, Fun, Options) ->
    case cache_module() of
        undefined ->
            error_logger:error_msg("Cache server not running"),
            value(Fun, Options);

        Module    -> Module:r(Key, Fun, Options)
    end.

%-------------------------------------------------------------------------------

%% Used by implementations to get the value to cache.
value(Fun, Options) ->
    case proplists:get_value(ehtmle, Options) of
        true ->
            Tehtml = Fun(),
            yaws_api:ehtml_expander(Tehtml);

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
    %             undefined ->
    %                 error_logger:error_msg("Cache server not running"),
    %                 Fun();
    % 
    %             _ -> ale_cache_memcached:cache(Key, Fun, Options)
    %         end;
    % 
    %     _ -> ale_cache_etscached:cache(Key, Fun, Options)
    % end.
    ale_cache_etscached.
