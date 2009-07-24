-module(ale_session_cache).

-compile(export_all).

session(Key, Value) ->
    SessionIdValue = case ale_session:session_id_value() of
        undefined       -> ale_session:session_id_value(undefined);
        SessionIdValue2 -> SessionIdValue2
    end,

    Session = case ale_cache:cache(SessionIdValue) of
        undefined ->
            case Value of
                undefined -> [];
                _         -> [{Key, Value}]
            end;

        Session2 ->
            case Value of
                undefined -> lists:keydelete(Key, 1, Session2);
                _         -> lists:keystore(Key, 1, Session2, {Key, Value})
            end
    end,

    ale_cache:cache(SessionIdValue, fun() -> Session end, w).

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
