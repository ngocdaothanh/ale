%%% This module creates the routing function used in ale_yaws_mod:
%%% route_uri(Method, Uri) -> {Controller, Action, Args} | no_route

-module(ale_routes_gen).

-compile(export_all).

gen() ->
    io:format("Collect routes...~n"),
    Forms = filelib:fold_files(".", "^c_.*\.beam$", true,
        fun(FileName, Acc) ->
            BaseName = filename:basename(FileName, ".beam"),
            io:format("  ~s...\n", [BaseName]),
            Controller = list_to_atom(BaseName),
            [gen(Controller) | Acc]
        end,
        []
    ),

    Source = [
        "% This file is autogenerated by routes_gen. Do not edit because your edit will\n% be lost when this file is regenerated.\n"
        "-module(ale_routes).\n\n"

        "-export([route_uri/2, route_tokens/2]).\n\n"

        "route_uri(Method, Uri) -> Tokens = string:tokens(Uri, \"/\"), route_tokens(Method, Tokens).\n",

        Forms,

        "\nroute_tokens(_, _) -> no_route.\n"
    ],
    file:write_file("ale_routes.erl", Source).

gen(Controller) ->
    code:ensure_loaded(Controller),
    case erlang:function_exported(Controller, routes, 0) of
        true ->
            Routes = Controller:routes(),
            gen(Controller, Routes, [["\n% ", atom_to_list(Controller), "\n"]]);

        false -> ""
    end.

gen(_Controller, [], FormAcc) ->
    lists:reverse(FormAcc);
gen(Controller, [Method, Uri, Action | Rest], FormAcc) ->
    Tokens = string:tokens(Uri, "/"),
    {Tokens2, Vars} = lists:foldr(
        fun(Token, {TokenAcc, VarAcc}) ->
            FirstChar = hd(Token),
            case ($a =< FirstChar) andalso (FirstChar =< $z) of
                true  -> {[([$"] ++ Token ++ [$"]) | TokenAcc], VarAcc};
                false -> {[Token | TokenAcc], [Token | VarAcc]}
            end
        end,
        {[], []},
        Tokens
    ),

    Template = "route_tokens(~p, [~s]) -> "
        "{~p, ~p, [~s]};\n",
    Form = io_lib:format(Template, [Method, string:join(Tokens2, ", "), Controller, Action, string:join(Vars, ", ")]),
    gen(Controller, Rest, [Form | FormAcc]).
