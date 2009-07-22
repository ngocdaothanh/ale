%%% For consistency, all session data (keys-values) of each user is collected
%%% into a proplists instead of being spread everywhere. When this proplists
%%% vanishes, all session data of this user vanishes.
-module(ale_session).

-compile(export_all).

-include("ale.hrl").

-define(FLASH_KEY, ale_session_flash).

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

flash(Value) -> session(?FLASH_KEY, Value).

flash() ->
    Flash = session(?FLASH_KEY),
    session(?FLASH_KEY, undefined),
    Flash.

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
        _         -> Value
    end,
    SessionIdKey = session_id_key(),
    {header, SetCookieRec} = yaws_api:setcookie(SessionIdKey, Value2, "/"),
    ale_pd:yaws(header, SetCookieRec),
    ale_pd:ale(session_id_value, Value2).

%% Generates new session ID value, which should be unique.
random_session_id_value() ->
    % node() should not be used because if the nodes are not connected, they may
    % have an identical name
    Random = random:uniform(16#ffffffffffffffff),  % 64 bits
    Now    = now(),
    Arg    = ale_pd:arg(),
    Salt   = proplists:get_value("session_secret", Arg#arg.opaque),
    ale_utils:md5_hex(erlang, term_to_binary({Random, Now, Salt})).

%-------------------------------------------------------------------------------

%% FIXME
session_module() ->
    ale_session_yaws.
