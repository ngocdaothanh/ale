-module(ale_cache_mnesia).

-compile(export_all).

-include("ale.hrl").

table() -> ?MODULE.

start_link(Nodes) ->
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, Nodes),
    Nodes2 = mnesia:system_info(running_db_nodes),

    mnesia:create_table(table(), [{ram_copies, Nodes2}]),  % Created only when the table does not already exist
    mnesia:add_table_copy(table(), node(), ram_copies),

    ignore.

cache(Key, Fun, Options) ->
    case catch mnesia:dirty_read(table(), Key) of
        [{?MODULE, Key, Value}] ->
            error_logger:info_msg("Cache Hit: ~p", [Key]),
            Value;

        _ ->
            error_logger:info_msg("Cache Miss: ~p", [Key]),
            Value = ale_cache:value(Fun, Options),
            mnesia:dirty_write({table(), Key, Value}),
            Value
    end.
