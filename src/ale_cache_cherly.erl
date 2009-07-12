-module(ale_cache_cherly).

-compile(export_all).

-define(MAGIC, "ale_cache_cherly").

start_link(SizeInMB) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, SizeInMB*1024*1024, []).

init(Size) ->
    {ok, C} = cherly:start(Size),
    {ok, C}.

r(Key, Fun, Options) ->
    case gen_server:call(?MODULE, {read, Key}, infinity) of
        {ok, Binary} ->
            Value = case Binary of
                <<?MAGIC, Rest/binary>> -> binary_to_term(Rest);
                _                                          -> Binary
            end,
            Value;

        not_found ->
            error_logger:info_msg("Cache Miss: ~p", [Key]),
            u(Key, Fun, Options)
    end.

u(Key, Fun, Options) ->
    Value = ale_cache:value(Fun, Options),
    Binary = case is_binary(Value) of
        false ->
            Binary0 = term_to_binary(Value),
            <<?MAGIC, Binary0/binary>>;

        true  -> Value
    end,
    gen_server:cast(?MODULE, {write, Key, Binary}),
    Value.

%-------------------------------------------------------------------------------

handle_call({read, Key}, _From, C) ->
    Result = cherly:get(C, Key),
    {reply, Result, C}.

handle_cast({write, Key, Binary}, C) ->
    cherly:put(C, Key, Binary),
    {noreply, C}.

handle_info(Info, C) ->
    {stop, {unexpected_message, Info}, C}.

terminate(_Reason, C) ->
    ets:delete(C).

code_change(_OldVsn, C, _Extra) -> {ok, C}.
