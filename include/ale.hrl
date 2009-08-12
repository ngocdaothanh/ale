-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-define(D(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).

-define(T(S), ale_i18n:t(S)).                              % i18n
-define(TF(S, Data), io_lib:format(ale_i18n:t(S), Data)).  % i18n with format
-define(TFB(S, Binding), ale_i18n:tfb(S, Binding)).        % i18n with binding

% Applications are run in ALE_ROOT/nodes/nx
-define(ALE_ROOT, "../..").
