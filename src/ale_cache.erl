-module(ale_cache).

-compile(export_all).

-include("ale.hrl").

ram_tables() -> [ale_cache_mnesia:table()].

start_link(SC, Nodes) ->
    case proplists:get_value("cache", SC#sconf.opaque) of
        undefined -> ignore;

        Server ->
            case string:tokens(Server, ":") of
                ["memcached", Host, Port] ->
                    Port2 = list_to_integer(Port),
                    ale_cache_memcached:start_link(Host, Port2);

                ["ets"] -> ale_cache_ets:start_link();

                ["mnesia"] ->
                    ale_cache_mnesia:start_link(Nodes);

                _ -> ingore
            end
    end.

cache(Key, Fun) -> cache(Key, Fun, []).

%% If you want to cache string or io list, for efficiency remember to set
%% Options to ask this function to convert it to binary before caching so that
%% serializing/deserializing is not performed every time the cache is read.
%%
%% Options:
%%   ehtml            : the return value of Fun is EHTML and will be converted to HTML then to binary
%%   iolist           : the return value of Fun is io list and will be converted to binary
%%   {ttl, Seconds}   : time to live until cache is expired
%%   {slide, Boolean} : slide expiration if the cached object is read
cache(Key, Fun, Options) ->
    % case whereis(ale_cache_server) of
    %     undefined ->
    %         case whereis(merle) of
    %             undefined ->
    %                 error_logger:error_msg("Cache server not running"),
    %                 Fun();
    % 
    %             _ -> ale_cache_memcached:cache(Key, Fun, Options)
    %         end;
    % 
    %     _ -> ale_cache_server:cache(Key, Fun, Options)
    % end.
    ale_cache_mnesia:cache(Key, Fun, Options).

value(Fun, Options) ->
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
    end.
