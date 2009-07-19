%%% For consistency, all session data (keys-values) of each user is collected
%%% into a proplists instead of being spread everywhere. When this proplists
%%% vanishes, all session data of this user vanishes.
-module(ale_session).

-compile(export_all).

-include("ale.hrl").

%-------------------------------------------------------------------------------

session(Key, Value) ->
    Module = session_module(),
    Module:session(Key, Value).

session(Key) ->
    Module = session_module(),
    Module:session(Key).

clear_session() ->
    Module = session_module(),
    Module:clear_session().

%-------------------------------------------------------------------------------
% For use by session implementation modules.

% FIXME: this should be configurable.
session_id_key() -> "__ale_session_id".

%% Getter.
session_id_value() ->
    case ale_pd:ale(session_id_value) of
        undefined ->
            Arg = ale_pd:arg(),
            Cookie = (Arg#arg.headers)#headers.cookie,
            SessionIdKey = session_id_key(),
            case yaws_api:find_cookie_val(SessionIdKey, Cookie) of
                []    -> undefined;
                Value -> Value
            end;

        Value -> Value
    end.

%% Setter. If Value is undefined, random value will be set.
session_id_value(Value) ->
    Value2 = case Value of
        undefined -> random_session_id_value();

        _ -> Value
    end,
    SessionIdKey = session_id_key(),
    ale_pd:yaws(header, {set_cookie, io_lib:format("~s=~s;", [SessionIdKey, Value2])}),
    ale_pd:ale(session_id_value, Value2).

%% Generates new session ID value.
random_session_id_value() ->
    % From yaws_sessions_server.erl
    N = random:uniform(16#ffffffffffffffff),  % 64 bits
    atom_to_list(node()) ++ [$- | integer_to_list(N)].

%-------------------------------------------------------------------------------

%% FIXME
session_module() ->
    ale_session_yaws.
