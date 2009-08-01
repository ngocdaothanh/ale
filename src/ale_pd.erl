%%% There is a process for each request.
%%%
%%% Yaws uses process dictionary. Some interesting things:
%%% * gc
%%% * sc
%%% * yaws_arg
%%%
%%% Ale also uses process dictionary in 3 namespaces:
%%% * ale_yaws: used to return things directly to Yaws (the namespace is not
%%%             just yaws to avoid potential conflict because Yaws also uses
%%%             the same process dictionary)
%%% * app:      used by the application
%%% * ale:      used by Ale framework


-module(ale_pd).

-compile(export_all).

-include("ale.hrl").

-define(KEY(Namespace, Key), {Namespace, Key}).

%-------------------------------------------------------------------------------
% Each application is associated with an SC (http://yaws.hyber.org/embed.yaws).
% We use SC's ets table to store consistent configurations throughout the
% application lifetime.

conf(SC, Namespace, Key) when is_record(SC, sconf) ->
    T = SC#sconf.ets,
    case ets:lookup(T, ?KEY(Namespace, Key)) of
        [{_NamespacedKey, Value}] -> Value;
        _                         -> undefined
    end;
conf(Namespace, Key, Value) ->
    SC = sc(),
    conf(SC, Namespace, Key, Value).

conf(Namespace, Key) ->
    SC = sc(),
    conf(SC, Namespace, Key).

conf(SC, Namespace, Key, Value) ->
    T = SC#sconf.ets,
    ets:insert(T, {?KEY(Namespace, Key), Value}).

%-------------------------------------------------------------------------------

%% If Key is header, Value will be accumulated to avoid overwriting the existings.
yaws(Key, Value) ->
    case Key of
        header ->
            case erlang:get(?KEY(ale_yaws, headers)) of
                undefined -> erlang:put(?KEY(ale_yaws, headers), [{header, Value}]);
                Headers   -> erlang:put(?KEY(ale_yaws, headers), Headers ++ [{header, Value}])
            end;

        _ -> erlang:put(?KEY(ale_yaws, Key), Value)
    end.

yaws(Key, Value1, Value2)         -> erlang:put(?KEY(ale_yaws, Key), {Value1, Value2}).
yaws(Key, Value1, Value2, Value3) -> erlang:put(?KEY(ale_yaws, Key), {Value1, Value2, Value3}).
yaws(Key)                         -> erlang:get(?KEY(ale_yaws, Key)).

app(Key, Value) -> erlang:put(?KEY(app, Key), Value).

%% Key script is special. For this key this function returns
%% {script, [{type, "text/javascript"}], accumulated scripts}.
%% For convenience, if there is no script it returns empty string.
app(Key) ->
    Value1 = erlang:get(?KEY(app, Key)),

    Value2 = case Key of
        % See app_add_head/1
        heads ->
            case Value1 of
                undefined -> "";
                IoList    -> IoList
            end;

        % See app_add_script/1
        scripts ->
            case Value1 of
                undefined -> "";
                IoList    -> {script, [{type, "text/javascript"}], IoList}
            end;

        _ -> Value1
    end,

    % Trick:
    % If
    % * the current action is cached without layout
    % * the layout is being rendered
    % then this function is being called (directly of indirectly) from the the
    % layout. In this case, Value should also be cached together with the view of
    % the action.
    case ale(variables_for_layout) of
        undefined -> ok;

        VFL ->
            % Avoid caching variables put by filters, only cache the variables
            % put by the action. Moreover, avoid adding {Key, Value} more than
            % one time.
            ActionKeys = ale_pd:ale(action_keys),
            case lists:member(Key, ActionKeys) andalso not proplists:is_defined(Key, VFL) of
                true  -> ale(variables_for_layout, [{Key, Value2} | VFL]);
                false -> ok
            end
    end,
    Value2.

%% Accumulates <head>. The heads will be dumped right before </body> in layout.
app_add_head(Head) ->
    % Avoid calling app(heads) because of the special cache handling right above
    case erlang:get(?KEY(app, heads)) of
        undefined -> app(heads, Head);
        IoList    -> app(heads, [IoList, Head])
    end.

%% Accumulates <script>. The scripts will be dumped right before </body> in layout.
app_add_script(Script) ->
    % Avoid calling app(scripts) because of the special cache handling right above
    case erlang:get(?KEY(app, scripts)) of
        undefined -> app(scripts, Script);
        IoList    -> app(scripts, [IoList, Script])
    end.

ale(Key, Value) -> erlang:put(?KEY(ale, Key), Value).
ale(Key)        -> erlang:get(?KEY(ale, Key)).

%-------------------------------------------------------------------------------

%% Returns all variables for one type.
%%%
%% get(ale_yaws) may be used to get the list to be sent to Yaws as the result of out/1.
%%
%% Type: ale_yaws | app | ale
get(Type) ->
    lists:foldr(
        fun
            ({{Type2, Key}, {Value1, Value2, Value3}}, Acc) when Type2 == Type ->
                [{Key, Value1, Value2, Value3} | Acc];

            ({{Type2, Key}, {Value1, Value2}}, Acc) when Type2 == Type ->
                [{Key, Value1, Value2} | Acc];

            ({{Type2, Key},  Value}, Acc) when Type2 == Type ->
                case (Type == ale_yaws) andalso (Key == headers) of
                    true  -> [Value | Acc];
                    false -> [{Key, Value} | Acc]
                end;

            (_, Acc) -> Acc
        end,
        [],
        erlang:get()
    ).

%% Returns all keys for one type.
%%
%% Type: ale_yaws | app | ale
keys(Type) ->
    lists:foldr(
        fun
            ({{Type2, Key}, {_Value1, _Value2, _Value3}}, Acc) when Type2 == Type -> [Key | Acc];
            ({{Type2, Key}, {_Value1, _Value2         }}, Acc) when Type2 == Type -> [Key | Acc];
            ({{Type2, Key},  _Value                    }, Acc) when Type2 == Type -> [Key | Acc];
            (_                                          , Acc)                    ->        Acc
        end,
        [],
        erlang:get()
    ).

%-------------------------------------------------------------------------------

gc()  -> erlang:get(gc).
sc()  -> erlang:get(sc).
arg() -> erlang:get(yaws_arg).

method(Method) -> ale(method, Method).
method()       -> ale(method).

path(Path) -> ale(path, Path).
path()     -> ale(path).

%% Makes absolute URL from Path. Host is as seen from the HTTP client.
schema_host_port(Path) ->
    SC = sc(),
    "http://" ++ SC#sconf.servername ++ lists:flatten(Path).

%% Returns remote IPv4 of the HTTP client in the form {a, b, c, d}.
ip() ->
    Arg = arg(),
    Sock = Arg#arg.clisock,
    Peer = case is_tuple(Sock) andalso (element(1, Sock) == sslsocket) of
        true -> ssl:peername(Sock);
        _    -> inet:peername(Sock)
    end, 
    {ok, {Ip, _Port}} = Peer,
    Ip.

%% Key: string().
params(Key, Value) -> erlang:put(?KEY(params, Key), Value).

%% To prevent list_to_atom attack, keys of params in the process dictionary are
%% strings, but for application development covenience, they can be accessed as
%% atoms.
%%
%% Key: atom()
params(Key)  ->
    KeyS = atom_to_list(Key),
    erlang:get(?KEY(params, KeyS)).

%-------------------------------------------------------------------------------

layout_module(Module) -> ale(layout_module, Module).
layout_module()       -> ale(layout_module).

view(Action) ->
    case Action of
        undefined -> view_module(undefined);

        _ ->
            Controller = ale_pd:params(controller),
            view(Controller, Action)
    end.

view(Controller, Action) ->
    ViewModule = list_to_atom("v_" ++ atom_to_list(Controller) ++ "_" ++ atom_to_list(Action)),
    ale_pd:ale(view_module, ViewModule).

view_module(Module) ->
    case Module of
        undefined -> erlang:erase(?KEY(ale, view_module));
        _         -> ale_pd:ale(view_module, Module)
    end.

view_module() -> ale(view_module).
