%% This module deals with setting and getting values from process dictionary.

-module(ale_pd).

-compile(export_all).

-define(KEY(Namespace, Key), {Namespace, Key}).

%-------------------------------------------------------------------------------
% There are 3 namespaces:
% * yaws: used to return things directly to Yaws
% * app:  used by the application
% * ale:  used by Ale framework

yaws(Key, Value)                  -> erlang:put(?KEY(yaws, Key), Value).
yaws(Key, Value1, Value2)         -> erlang:put(?KEY(yaws, Key), {Value1, Value2}).
yaws(Key, Value1, Value2, Value3) -> erlang:put(?KEY(yaws, Key), {Value1, Value2, Value3}).
yaws(Key)                         -> erlang:get(?KEY(yaws, Key)).

app(Key, Value) -> erlang:put(?KEY(app, Key), Value).

%% This function is special. It accumulates JavaScripts.
app_add_script(Script) ->
    % Avoid calling app(Key) because of the special cache handling
    case erlang:get(?KEY(app, script)) of
        undefined -> app(script, Script);
        IoList    -> app(script, [IoList, Script])
    end.

%% Key script is special. For this key this function returns
%% {script, [{type, "text/javascript"}], accumulated scripts}.
%% For convenience, if there is no script it returns empty string.
app(Key) ->
    Value1 = erlang:get(?KEY(app, Key)),

    % script is special
    Value2 = case Key of
        script ->
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

ale(Key, Value) -> erlang:put(?KEY(ale, Key), Value).
ale(Key)        -> erlang:get(?KEY(ale, Key)).

%-------------------------------------------------------------------------------

%% Returns all variables for one type.
%%%
%% get(yaws) may be used to get the list to be sent to Yaws as the result of out/1.
%%
%% Type: yaws | app | ale
get(Type) ->
    lists:foldr(
        fun
            ({{Type2, Key}, {Value1, Value2, Value3}}, Acc) when Type2 == Type ->
                [{Key, Value1, Value2, Value3} | Acc];

            ({{Type2, Key}, {Value1, Value2        }}, Acc) when Type2 == Type ->
                [{Key, Value1, Value2        } | Acc];

            ({{Type2, Key},  Value                  }, Acc) when Type2 == Type ->
                [{Key, Value}                  | Acc];

            (_                                       , Acc)                    ->
                                                 Acc
        end,
        [],
        erlang:get()
    ).

%% Returns all keys for one type.
%%
%% Type: yaws | app | ale
keys(Type) ->
    lists:foldr(
        fun
            ({{Type2, Key}, {_Value1, _Value2, _Value3}}, Acc) when Type2 == Type -> [Key | Acc];
            ({{Type2, Key}, {_Value1, _Value2         }}, Acc) when Type2 == Type -> [Key | Acc];
            ({{Type2, Key},  _Value                    }, Acc) when Type2 == Type -> [Key | Acc];
            (_                                         , Acc)                     ->        Acc
        end,
        [],
        erlang:get()
    ).

%-------------------------------------------------------------------------------

arg(Arg) -> ale(arg, Arg).
arg()    -> ale(arg).

method(Method) -> ale(method, Method).
method()       -> ale(method).

uri(Uri) -> ale(uri, Uri).
uri()    -> ale(uri).

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
        undefined -> erlang:erase(?KEY(ale, view_module));

        _ ->
            Controller = ale_pd:params(controller),
            view(Controller, Action)
    end.

view(Controller, Action) ->
    ViewModule = list_to_atom("v_" ++ atom_to_list(Controller) ++ "_" ++ atom_to_list(Action)),
    ale_pd:ale(view_module, ViewModule).

view_module(Module) ->
    ale_pd:ale(view_module, Module).

view_module() -> ale(view_module).
