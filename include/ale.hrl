-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-define(D(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).

-define(T(S), ale_i18n:t(S)).                    % i18n
-define(TF(S, A), io:format(ale_i18n:t(S), A)).  % i18n with format

% Applications are run in ALE_ROOT/nodes/nx
-define(ALE_ROOT, "../..").
