-module(ale_session_cache).

-compile(export_all).

session(Key, Value) ->
    SessionIdValue = case ale_session:session_id_value() of
        undefined -> ale_session:session_id_value(undefined);
        Value     -> Value
    end,

    Session2 = case ale_cache:cache(SessionIdValue) of
        undefined -> [{Key, Value}];
        Session   -> lists:keystore(Key, 1, Session, {Key, Value})
    end,

    ale_cache:cache(SessionIdValue, fun() -> Session2 end, w).

session(Key) ->
    case ale_session:session_id_value() of
        undefined -> undefined;

        SessionIdValue ->
            case ale_cache:cache(SessionIdValue) of
                undefined -> undefined;
                Session   -> proplists:get_value(Key, Session)
            end
    end.

clear_session() ->
    case ale_session:session_id_value() of
        undefined      -> undefined;
        SessionIdValue -> ale_cache:cache(SessionIdValue, fun() -> [] end, w)
    end.
