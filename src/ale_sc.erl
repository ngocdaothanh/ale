-module(ale_sc).

-compile(export_all).

-include("ale.hrl").

%% Modifies the current SC to set docroot and xtra_docroots to the list of all
%% docroot directories in the application.
%%
%% Returns the modified SC.
set_docroots(SC) ->
    {ok, GC, [SCs]} = yaws_api:getconf(),  % The 3rd element is array of array

    % docroot is undefined at this moment (see yaws.conf)
    % SC2 = SC#sconf{xtra_docroots = public_dirs()} does not work because Yaws
    % does not check public_dirs if it sees that docroot is undefined
    SC2 = SC#sconf{docroot = ?ALE_ROOT ++ "/docroot", xtra_docroots = xtra_docroots()},

    % 10: position of servername in the sconf record, see yaws.hrl
    SCs2 = lists:keyreplace(SC#sconf.servername, 10, SCs, SC2),

    yaws_api:setconf(GC, [SCs2]),
    SC2.

xtra_docroots() ->
    xtra_docroots(filelib:wildcard(?ALE_ROOT ++ "/*"), []).

xtra_docroots(Dirs, Acc) ->
    lists:foldl(
        fun(Dir, Acc2) ->
            case filelib:is_dir(Dir) of
                false -> Acc2;

                true ->
                    Basename = filename:basename(Dir),
                    case Basename of
                        "docroot" -> [Dir | Acc];
                        _ -> xtra_docroots(filelib:wildcard(Dir ++ "/*"), Acc2)
                    end
            end
        end,
        Acc,
        Dirs
    ).
