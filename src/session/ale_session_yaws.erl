%%% Yaws has a built-in session server. It may be perfect when there is only one
%%% web server in the system.
%%%
%%% http://yaws.hyber.org/code.yaws?file=/session1.yaws
-module(ale_session_yaws).

-compile(export_all).

session(Key, Value) ->
    case ale_session:session_id_value() of
        undefined ->
            SessionIdValue = yaws_api:new_cookie_session([{Key, Value}]),
            ale_session:session_id_value(SessionIdValue);

        SessionIdValue ->
            case yaws_api:cookieval_to_opaque(SessionIdValue) of
                {ok, Session} ->
                    Session2 = lists:keystore(Key, 1, Session, {Key, Value}),
                    yaws_api:replace_cookie_session(SessionIdValue, Session2);

                _ ->
                    SessionIdValue2 = yaws_api:new_cookie_session([{Key, Value}]),
                    ale_session:session_id_value(SessionIdValue2)
            end
    end.

session(Key) ->
    case ale_session:session_id_value() of
        undefined -> undefined;

        SessionIdValue ->
            case yaws_api:cookieval_to_opaque(SessionIdValue) of
                {ok, Session} -> proplists:get_value(Key, Session);

                _ -> undefined
            end
    end.

clear_session() ->
    case ale_session:session_id_value() of
        undefined -> ok;

        SessionIdValue ->
            case yaws_api:cookieval_to_opaque(SessionIdValue) of
                {ok, Session} -> yaws_api:replace_cookie_session(SessionIdValue, []);
                _             -> ok
            end
    end.
