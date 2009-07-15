%% This module deals with setting and getting values from process dictionary.

-module(ale_pd).

-compile(export_all).

-define(KEY(Namespace, Key), {Namespace, Key}).

yaws(Key, Value)                  -> erlang:put(?KEY(yaws, Key), Value).
yaws(Key, Value1, Value2)         -> erlang:put(?KEY(yaws, Key), {Value1, Value2}).
yaws(Key, Value1, Value2, Value3) -> erlang:put(?KEY(yaws, Key), {Value1, Value2, Value3}).
yaws(Key)                         -> erlang:get(?KEY(yaws, Key)).

app(Key, Value) -> erlang:put(?KEY(app, Key), Value).

app(Key) ->
    Value = erlang:get(?KEY(app, Key)),

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
                true  -> ale(variables_for_layout, [{Key, Value} | VFL]);
                false -> ok
            end
    end,
    Value.

%-------------------------------------------------------------------------------

arg()        -> ale(arg).
method()     -> ale(method).
uri()        -> ale(uri).
controller() -> ale(controller).
action()     -> ale(action).

layout(Value) -> ale(layout, Value).
layout()      -> ale(layout).

view(Value) -> ale(view, Value).
view()      -> ale(view).

content_for_layout() -> ale(content_for_layout).

%% Accumulates Script.
script(Script) ->
    case ale(script) of
        undefined -> ale(script, Script);
        IoList    -> ale(script, [IoList, Script])
    end.

%% Returns {script, [{type, "text/javascript"}], accumulated scripts}. If there
%% is no script returns "" (not undefined for convenience because this function
%% is used in layout).
script() ->
    case ale(script) of
        undefined -> "";
        IoList    -> {script, [{type, "text/javascript"}], IoList}
    end.

%-------------------------------------------------------------------------------

url_for(Controller, Action)       -> ale_routes:url_for(Controller, Action, []).
url_for(Controller, Action, Args) -> ale_routes:url_for(Controller, Action, Args).

%-------------------------------------------------------------------------------
% These functions are used internally by Ale.

ale(Key, Value) -> erlang:put(?KEY(ale, Key), Value).
ale(Key)        -> erlang:get(?KEY(ale, Key)).

arg(Arg)               -> ale(arg, Arg).
method(Method)         -> ale(method, Method).
uri(Uri)               -> ale(uri, Uri).
controller(Controller) -> ale(controller, Controller).
action(Action)         -> ale(action, Action).

content_for_layout(Value) -> ale(content_for_layout, Value).

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
