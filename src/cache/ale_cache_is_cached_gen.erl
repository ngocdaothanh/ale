%%% This module creates the is_cached function used in ale_yaws_mod:
%%% is_cached(Controller, page | action_with_layout | action_without_layout, Action) -> bool()

-module(ale_cache_is_cached_gen).

-compile(export_all).

gen() ->
    io:format("Collect caches...~n"),
    Forms = filelib:fold_files(".", "^c_.*\.beam$", true,
        fun(FileName, Acc) ->
            BaseName = filename:basename(FileName, ".beam"),
            io:format("  ~s...\n", [BaseName]),
            ControllerModule = list_to_atom(BaseName),
            case parse(ControllerModule) of
                undefined -> Acc;
                Form      -> [Form | Acc]
            end
        end,
        []
    ),

    Source = [
        "% This file is autogenerated by ale_cache_is_cached_gen. Do not edit\n% because your edit will be lost when this file is regenerated.\n"
        "-module(ale_is_cached).\n\n"

        "-export([is_cached/3]).\n",

        is_cached_gen(Forms),
        "\nis_cached(_, _, _) -> false.\n"
    ],
    file:write_file("ale_is_cached.erl", Source).

%-------------------------------------------------------------------------------

%% Returns {ControllerModule, [{Type, Action}]}
parse(ControllerModule) ->
    Attributes = ControllerModule:module_info(attributes),
    case proplists:get_value(caches, Attributes) of
        undefined -> undefined;
        Caches    -> {ControllerModule, parse(Caches, [])}
    end.

parse([], Acc) -> lists:reverse(lists:flatten(Acc));
parse([Type, Actions | Rest], Acc) ->
    TypeActionList = lists:foldr(
        fun(Action, Acc2) ->
            [{Type, Action} | Acc2]
        end,
        [],
        Actions
    ),
    parse(Rest, [TypeActionList | Acc]).

%-------------------------------------------------------------------------------

is_cached_gen(Forms) ->
    lists:foldl(
        fun({ControllerModule, TypeActionList}, Acc) ->
            [
                Acc, "\n",
                "% ", atom_to_list(ControllerModule), "\n",
                lists:foldl(
                    fun({Type, Action}, Acc2) ->
                        [
                            Acc2,
                            io_lib:format(
                                "is_cached(~p, ~p, ~p) -> true;\n",
                                [ControllerModule, Type, Action]
                            )
                        ]
                    end,
                    [],
                    TypeActionList
                )
            ]
        end,
        [],
        Forms
    ).