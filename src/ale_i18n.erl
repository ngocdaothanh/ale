-module(ale_i18n).

-compile(export_all).
  
t(Key) ->
    Key.

tfb(S, Binding) ->
    lists:foldl(
        fun({K, V}, Acc) ->
            K2 = [":", atom_to_list(K)],
            re:replace(Acc, K2, V, [global, {return, list}])
        end,
        t(S),
        Binding
    ).
