%% This module deals with setting and getting values from process dictionary.

-module(ale_pd).

-compile(export_all).

-define(KEY(Namespace, Key), {Namespace, Key}).

yaws(Key, Value)                  -> erlang:put(?KEY(yaws, Key), Value).
yaws(Key, Value1, Value2)         -> erlang:put(?KEY(yaws, Key), {Value1, Value2}).
yaws(Key, Value1, Value2, Value3) -> erlang:put(?KEY(yaws, Key), {Value1, Value2, Value3}).
yaws(Key)                         -> erlang:get(?KEY(yaws, Key)).

%% Returns the list to be sent to Yaws as the result of Yaws' out/1.
yaws() ->
    lists:foldr(
        fun
            ({{yaws, Key}, {Value1, Value2, Value3}}, Acc) -> [{Key, Value1, Value2, Value3} | Acc];
            ({{yaws, Key}, {Value1, Value2        }}, Acc) -> [{Key, Value1, Value2        } | Acc];
            ({{yaws, Key},  Value                  }, Acc) -> [{Key, Value}                  | Acc];
            (_                                      , Acc) ->                                  Acc
        end,
        [],
        erlang:get()
    ).

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
        VFL       -> ale(variables_for_layout, [{Key, Value} | VFL])
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
