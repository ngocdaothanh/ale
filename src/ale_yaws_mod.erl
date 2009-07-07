%%% start, out, and out404 are called by Yaws as configured in yaws.conf

-module(ale_yaws_mod).

-compile(export_all).

-include("ale.hrl").

-define(MAX_R, 30).
-define(MAX_T, 60).

%-------------------------------------------------------------------------------

%% Called after Yaws has loaded.
start(SC) ->
    SC2 = set_docroot(SC),
    start_children(SC2).

out(Arg) ->
    Uri = Arg#arg.server_path,
    case Uri of
        % Give Yaws a chance to server static file from "/static"
        % See http://groups.google.com/group/nitrogenweb/browse_thread/thread/c2ce70f696b77c9e
        % If Yaws cannot find a file at the Uri, out404 below will be called
        "/static" ++ _ -> ale:yaws(page, Arg#arg.server_path);

        _ ->
            % Errors are not cached
            Method = rest_method(Arg),
            case ale_routes:route_uri(Method, Uri) of
                no_route -> c_application:error_404(Arg, Uri);

                {Controller, Action, Args} ->
                    try
                        handle_request1(Method, Uri, Controller, Action, Arg, Args)
                    catch
                        Type : Reason ->
                            error_logger:error_report([
                                {type, Type}, {reason, Reason},
                                {trace, erlang:get_stacktrace()}
                            ]),

                            % Type and Reason are more convenient than those
                            % arguments of Yaws' errormod_crash
                            c_application:error_500(Type, Reason),
                            case ale:view() of
                                undefined -> ok;
                                View      -> ale:yaws(ehtml, View:render())
                            end
                    end
            end
    end,
    ale:yaws().

out404(Arg, _GC, _SC) ->
    Uri = Arg#arg.appmoddata,
    c_application:error_404(Uri),
    case ale:view() of
        undefined -> ok;
        View      -> ale:yaws(ehtml, View:render())
    end,
    ale:yaws().

%-------------------------------------------------------------------------------

%% Modifies the current SC to set docroot and xtra_docroots to the list of all
%% public directories in the application.
set_docroot(SC) ->
    {ok, GC, [SCs]} = yaws_api:getconf(),  % The 3rd element is array of array

    % docroot is undefined at this moment (see yaws.conf)
    % SC2 = SC#sconf{xtra_docroots = public_dirs()} does not work because Yaws
    % does not check public_dirs if it sees that docroot is undefined
    SC2 = SC#sconf{docroot = "docroot", xtra_docroots = xtra_docroots()},

    % 10: position of servername in the sconf record, see yaws.hrl
    SCs2 = lists:keyreplace(SC#sconf.servername, 10, SCs, SC2),

    yaws_api:setconf(GC, [SCs2]),
    SC2.

xtra_docroots() ->
    xtra_docroots(filelib:wildcard("./*"), []).

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

%-------------------------------------------------------------------------------

%% Starts the application supervisor and Merle. They are supervised by Ale, which
%% is in turn supervised by yaws_sup. We cannot let the 2 children supervised
%% directly by yaws_sup because yaws_sup is configured as "one_for_all", thus
%% Yaws is stopped if any of the children dies.
start_children(SC) ->
    ChildSpec = {
        ale, {supervisor, start_link, [?MODULE, SC]},
        permanent, infinity, supervisor, [ale_yaws_mod]
    },
    supervisor:start_child(yaws_sup, ChildSpec).

%% Called by start_children above.
init(SC) ->
    AppSpec = {
        ale_application, {c_application, start, [SC]},
        permanent, brutal_kill, worker, [c_application]
    },

    ChildSpecs = case proplists:get_value("memcached", SC#sconf.opaque) of
        undefined -> [AppSpec];

        HostPort ->
            [Host, Port] = string:tokens(HostPort, ":"),
            Port2 = list_to_integer(Port),
            MerleSpec = {
                merle, {merle, connect, [Host, Port2]},
                permanent, brutal_kill, worker, [merle]
            },
            [AppSpec, MerleSpec]
    end,

    {ok, {{one_for_one, ?MAX_R, ?MAX_T}, ChildSpecs}}.

%-------------------------------------------------------------------------------

%% 'GET'                       -> get
%% 'POST'                      -> post
%% 'POST' & _method = "put"    -> put
%% 'POST' & _method = "delete" -> delete
rest_method(Arg) ->
    Method = (Arg#arg.req)#http_request.method,
    case Method of
        'GET' -> get;

        'POST' ->
            case yaws_api:postvar(Arg, "_method") of
                {ok, "put"}    -> put;
                {ok, "delete"} -> delete;
                _              -> post
            end
    end.

handle_request1(Method, Uri, Controller, Action, Arg, Args) ->
    PageCached = page_cached(Controller, Action),
    case PageCached of
        true ->
            case merle:getkey(Uri) of
                undefined ->
                    error_logger:info_msg("Cache Miss: ~s", [Uri]),
                    ale:ale(page_cached, PageCached),
                    handle_request2(Method, Uri, Controller, Action, Arg, Args);

                Html ->
                    error_logger:info_msg("Cache Hit: ~s", [Uri]),
                    ale:yaws(html, Html)
            end;

        false ->
            ale:ale(page_cached, PageCached),
            handle_request2(Method, Uri, Controller, Action, Arg, Args)
    end.

handle_request2(Method, Uri, Controller, Action, Arg, Args) ->
    case run_before_filters(Controller, Action, Arg, Args) of
        true -> ok;

        false ->
            View = controller_to_view(Controller, Action),
            ale:view(View),

            apply(Controller, Action, [Arg | Args]),

            case ale:view() of
                undefined -> ok;    % Layout is not called if there is no view

                View2 ->
                    Content = View2:render(),
                    Content2 = case ale:layout() of
                        undefined -> Content;

                        Layout ->
                            ale:content_for_layout(Content),
                            Layout:render()
                    end,

                    Html = yaws_api:ehtml_expand(Content2),
                    case ale:ale(page_cached) of
                        true ->
                            error_logger:info_msg("Cache Write: ~s", [Uri]),
                            merle:set(Uri, Html);

                        false -> ok
                    end,
                    ale:yaws(html, Html)
            end
    end.

page_cached(Controller, Action) ->
    code:ensure_loaded(Controller),
    case erlang:function_exported(Controller, cached_pages, 0) of
        true  -> lists:member(Action, Controller:cached_pages());
        false -> false
    end.

%% Returns true if the action should be halted.
run_before_filters(Controller, Action, Arg, Args) ->
    lists:any(
        fun(Module) ->
            case erlang:function_exported(Module, before_filter, 4) of
                true  -> Module:before_filter(Controller, Action, Arg, Args);
                false -> false
            end
        end,
        [c_application, Controller]
    ).

%% Converts c_hello and show to v_hello_show
controller_to_view(Controller, Action) ->
    [$c, $_ | Base] = atom_to_list(Controller),
    list_to_atom("v_" ++ Base ++ "_" ++ atom_to_list(Action)).
