-module(ale).

-compile(export_all).

-define(KEY(Namespace, Key), {Namespace, Key}).

sync() -> make:all([load]).

%-------------------------------------------------------------------------------

%% Each request has its own processing process. If you want to share variables
%% across functions, you can put them to the process dictionary using put/3-5
%% and get/1, 2.
put(Namespace, Key, Value)                  -> put(?KEY(Namespace, Key), Value).
put(Namespace, Key, Value1, Value2)         -> put(?KEY(Namespace, Key), {Value1, Value2}).
put(Namespace, Key, Value1, Value2, Value3) -> put(?KEY(Namespace, Key), {Value1, Value2, Value3}).

get(Namespace, Key) -> erlang:get(?KEY(Namespace, Key)).

get(Namespace) ->
    lists:foldr(
        fun
            ({{Namespace2, Key}, {Value1, Value2, Value3}}, Acc)
            when Namespace2 == Namespace -> [{Key, Value1, Value2, Value3} | Acc];

            ({{Namespace2, Key}, {Value1, Value2        }}, Acc)
            when Namespace2 == Namespace -> [{Key, Value1, Value2        } | Acc];

            ({{Namespace2, Key},  Value                  }, Acc)
            when Namespace2 == Namespace -> [{Key, Value}                  | Acc];

            (_                                            , Acc)
                                         ->                                  Acc
        end,
        [],
        get()
    ).
