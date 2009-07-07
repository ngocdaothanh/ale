-module(ale).

-compile(export_all).

-define(KEY(Namespace, Key), {Namespace, Key}).

sync() -> make:all([load]).

%-------------------------------------------------------------------------------

yaws(Key, Value) -> erlang:put(?KEY(yaws, Key), Value).
yaws(Key, Value1, Value2) -> erlang:put(?KEY(yaws, Key), {Value1, Value2}).
yaws(Key, Value1, Value2, Value3) -> erlang:put(?KEY(yaws, Key), {Value1, Value2, Value3}).
yaws(Key) -> erlang:get(?KEY(yaws, Key)).

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

%% Each request has its own processing process. If you want to share variables
%% across functions, use app/2 and app/1.
app(Key, Value) -> erlang:put(?KEY(app, Key), Value).
app(Key) -> erlang:get(?KEY(app, Key)).

%-------------------------------------------------------------------------------

layout(Value) -> ale(layout, Value).
layout()      -> ale(layout).

view(Value) -> ale(view, Value).
view()      -> ale(view).

content_for_layout(Value) -> ale(content_for_layout, Value).
content_for_layout()      -> ale:ale(content_for_layout).

%% Accumulates Script.
script(Script) ->
    case ale(script) of
        undefined -> ale(script, Script);
        IoList    -> ale(script, [IoList, Script])
    end.

%% Returns {script, [{type, "text/javascript"}], accumulated scripts} or "" if there is no script.
script() ->
    case ale(script) of
        undefined -> "";
        IoList    -> {script, [{type, "text/javascript"}], IoList}
    end.

%-------------------------------------------------------------------------------

user() ->
    undefined.

%-------------------------------------------------------------------------------

md5_hex(Data) ->
    Md5 = erlang:md5(Data),
    lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(Md5)]).

gravatar(Email, Size) ->
    GravatarId = case Email of
        undefined -> "";
        _         -> md5_hex(Email)
    end,
    Src = io_lib:format("http://www.gravatar.com/avatar.php?size=~p&gravatar_id=~s", [Size, GravatarId]),
    {img, [{src, Src}]}.

%-------------------------------------------------------------------------------

set_action_cache(Method, Uri, Value) ->
    Key = atom_to_list(Method) ++ ":" ++ Uri,
    merle:set(Key, Value).

get_action_cache(Method, Uri) ->
    Key = atom_to_list(Method) ++ ":" ++ Uri,
    merle:getkey(Key).

delete_action_cache(Method, Uri) ->
    Key = atom_to_list(Method) ++ ":" ++ Uri,
    merle:getkey(Key).

%-------------------------------------------------------------------------------
% Private functions

ale(Key, Value) -> erlang:put(?KEY(ale, Key), Value).
ale(Key) -> erlang:get(?KEY(ale, Key)).
