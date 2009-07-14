%%% See NOTE for implementations in ale_cache.

-module(ale_cache_cherly).

-compile(export_all).

-define(MAGIC, "ale_cache_cherly").

start_link(SizeInMB) ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, SizeInMB, []).

r(Key) ->
    case gen_server2:call(?MODULE, Key, infinity) of
        {ok, Binary} ->
            Value = case Binary of
                <<?MAGIC, Rest/binary>> -> binary_to_term(Rest);
                _                       -> Binary
            end,
            {ok, Value};

        not_found -> not_found
    end.

r(Key, Fun, Options) ->
    % This cannot be done in 1 call to the gen_server, see NOTE in ale_cache
    % about Fun
    case r(Key) of
        {ok, Binary} ->
            Value = case Binary of
                <<?MAGIC, Rest/binary>> -> binary_to_term(Rest);
                _                       -> Binary
            end,
            Value;

        not_found ->
            error_logger:info_msg("Cache Miss: ~p", [Key]),
            w(Key, Fun, Options)
    end.

w(Key, Fun, Options) ->
    Value = ale_cache:value(Fun, Options),
    gen_server2:cast(?MODULE, {Key, Value}),
    Value.

%-------------------------------------------------------------------------------

init(SizeInMB) ->
    {ok, C} = cherly:start(SizeInMB*1024*1024),
    {ok, C}.  % C is the state for the gen_server

handle_call(Key, _From, C) ->
    Result = cherly:get(C, Key),
    {reply, Result, C}.

handle_cast({Key, Value}, C) ->
    Binary = case is_binary(Value) of
        false ->
            Binary0 = term_to_binary(Value),
            <<?MAGIC, Binary0/binary>>;

        true  -> Value
    end,
    cherly:put(C, Key, Binary),
    {noreply, C}.

handle_info(Info, C) ->
    {stop, {unexpected_message, Info}, C}.

terminate(_Reason, C) -> cherly:stop(C).

code_change(_OldVsn, C, _Extra) -> {ok, C}.
