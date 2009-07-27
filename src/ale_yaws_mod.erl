%%% start, out, and out404 are called by Yaws as configured in yaws.conf

-module(ale_yaws_mod).

-compile(export_all).

-include("ale.hrl").

-define(MAX_R, 30).
-define(MAX_T, 60).

%-------------------------------------------------------------------------------

%% Called after Yaws has loaded.
start(SC) ->
    SC2 = ale_sc:set_docroots(SC),
    start_children(SC2).

% NOTE about caching:
% * 404 and 500 error pages are not cached
% * Pages are not cached to files on disk (then let Yaws handle the files). They
%   are cached in memory the same way as actions, fragments, and objects for 2
%   reasons: (1) memory caching as Yaws file caching are the same speed
%   (normally ~3000req/s), (2) functionalities like auto expiration 

% FIXME: currently cache only works if there is a view to render

out(Arg) ->
    Path = Arg#arg.server_path,  % Ale only supports mounting to /

    case Path of
        % Give Yaws a chance to server static file from "/static"
        % See http://groups.google.com/group/nitrogenweb/browse_thread/thread/c2ce70f696b77c9e
        % If Yaws cannot find a file at the Path, out404 below will be called
        %
        % NOTE: Yaws 1.84 crashes if we put({yaws, page}, Arg#arg.server_path)
        % and return page
        "/static" ++ _ -> {page, Path};

        _ ->
            Method = rest_method(Arg),
            case ale_routes:route_path(Method, Path) of
                no_route ->
                    c_application:error_404(),
                    case ale_pd:view_module() of
                        undefined  -> ignore;
                        ViewModule -> ale_pd:yaws(ehtml, ViewModule:render())
                    end;

                {ControllerModule, Action, Params} ->
                    try
                        Params2 = lists:map(
                            fun({Key, Value}) -> {Key, yaws_api:url_decode(Value)} end,
                            Params
                        ),
                        handle_request1(Arg, Method, Path, ControllerModule, Action, Params2)
                    catch
                        Type : Reason ->
                            error_logger:error_report([
                                {type,   Type},
                                {reason, Reason},
                                {trace,  erlang:get_stacktrace()}
                            ]),

                            % Type and Reason are more convenient than those
                            % arguments of Yaws' errormod_crash
                            c_application:error_500(Type, Reason),
                            case ale_pd:view_module() of
                                undefined  -> ignore;
                                ViewModule -> ale_pd:yaws(ehtml, ViewModule:render())
                            end
                    end
            end,
            ale_pd:get(yaws)
    end.

out404(_Arg, _GC, _SC) ->
    c_application:error_404(),
    case ale_pd:view_module() of
        undefined  -> ignore;
        ViewModule -> ale_pd:yaws(ehtml, ViewModule:render())
    end,
    ale_pd:get(yaws).

%-------------------------------------------------------------------------------

%% Starts the application supervisor and Merle. They are supervised by Ale, which
%% is in turn supervised by yaws_sup. We cannot let the 2 children supervised
%% directly by yaws_sup because yaws_sup is configured as "one_for_all", thus
%% Yaws is stopped if any of the children dies.
start_children(SC) ->
    application:start(log4erl),
    log4erl:conf("log4erl.conf"),

    ChildSpec = {
        ale, {supervisor, start_link, [?MODULE, SC]},
        permanent, infinity, supervisor, [ale_yaws_mod]
    },
    case supervisor:start_child(yaws_sup, ChildSpec) of
        {error, Reason} ->
            log4erl:error("Error starting Ale application:~n~p", [Reason]),
            {error, Reason};

        X -> X
    end.

%% Called by start_children above.
init(SC) ->
    Nodes = ale_sc:nodes(SC),
    AppSpec = {
        c_application, {c_application, start, [SC, Nodes]},
        permanent, brutal_kill, worker, [c_application]
    },
    CacheSpec = {
        ale_cache, {ale_cache, start_link, [SC, Nodes]},
        permanent, brutal_kill, worker, [ale_cache]
    },
    {ok, {{one_for_one, ?MAX_R, ?MAX_T}, [AppSpec, CacheSpec]}}.

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

handle_request1(Arg, Method, Path, ControllerModule, Action, Params) ->
    case ale_is_cached:is_cached(ControllerModule, page, Action) of
        true ->
            Html = ale_cache:cache(Path, fun() ->
                handle_request2(Arg, Method, Path, ControllerModule, Action, Params)
            end),
            ale_pd:yaws(html, Html);

        false -> handle_request2(Arg, Method, Path, ControllerModule, Action, Params)
    end.

%% Returns:
%% * HTML (converted to binary) if a view has been rendered (with or without layout)
%% * undefined if there was no view or the request has been halted by a before action filter
handle_request2(Arg, Method, Path, ControllerModule, Action, Params) ->
    % Set environment variables to the process dictionary here (not in
    % handle_request1 because they are not used there) because the application
    % may need these variables
    ale_pd:method(Method),
    ale_pd:path(Path),

    % Keys of params are always string to avoid list_to_atom attack, however
    % for convenience, application developer may use ale:params(Atom)

    ExtraParams = case Method of
        get -> yaws_api:parse_query(Arg);
        _   -> yaws_api:parse_post(Arg)
    end,
    lists:foreach(
        fun({Key, Value}) -> ale_pd:params(Key, Value) end,
        ExtraParams
    ),

    % Put params on URI later
    lists:foreach(
        fun({Key, Value}) -> ale_pd:params(Key, Value) end,
        Params
    ),

    % Lastly put controller and action params
    Controller = cm2c(ControllerModule),
    ale_pd:params("controller", Controller),
    ale_pd:params("action", Action),

    % Set default view before calling filter and action, the filter or action
    % may select another view
    ale_pd:view(Action),

    case run_before_action(ControllerModule, Action) of
        true -> undefined;  % Can't be page cached because halted by a before action filter

        false ->
            case ale_is_cached:is_cached(ControllerModule, action_with_layout, Action) of
                true ->
                    Html = ale_cache:cache(Path, fun() ->
                        handle_request3(Path, ControllerModule, Action)
                    end),
                    ale_pd:yaws(html, Html);    % Can't be page cached because action cached

                false -> handle_request3(Path, ControllerModule, Action)
            end
    end.

%% Returns HTML if there is a view to render, undefined otherwise.
handle_request3(Path, ControllerModule, Action) ->
    ActionCachedWithoutLayout = ale_is_cached:is_cached(ControllerModule, action_without_layout, Action),
    {ContentForLayout, FromCache} = case ActionCachedWithoutLayout of
        true ->
            % If taken from cache then it is binary or list:
            % * If list, it is:
            % [{content_for_layout, Binary} | Variables to be put to process dictionary for layout to use]
            % * If binary, it is content_for_layout
            case ale_cache:cache(Path) of
                not_found -> {handle_request4(ControllerModule, Action), false};

                {ok, BinaryOrList} ->
                    case is_binary(BinaryOrList) of
                        true -> {BinaryOrList, true};

                        false ->
                            CFL = proplists:get_value(content_for_layout, BinaryOrList),
                            lists:foreach(
                                fun({Key, Value}) ->
                                    case Key == content_for_layout of
                                        true  -> ok;
                                        false -> ale_pd:app(Key, Value)
                                    end
                                end,
                                BinaryOrList
                            ),
                            {CFL, true}
                    end
            end;

        false -> {handle_request4(ControllerModule, Action), undefined}
    end,

    case ContentForLayout of
        undefined -> undefined;

        _ ->  % Binary
            Html = case ale_pd:layout_module() of
                undefined ->
                    case ActionCachedWithoutLayout of
                        false -> ok;

                        % Cache ContentForLayout if is was not from cache
                        true ->
                            case FromCache of
                                true  -> ok;
                                false -> ale_cache:cache(Path, fun() -> ContentForLayout end)
                            end
                    end,
                    ContentForLayout;

                LayoutModule ->
                    ale_pd:app(content_for_layout, ContentForLayout),

                    % See above and ale_pd:app/1
                    Ehtml = case ActionCachedWithoutLayout of
                        false -> run_layout(LayoutModule);

                        true ->
                            % Cache ContentForLayout if is was not from cache
                            case FromCache of
                                true ->
                                    % Things have been put into the process dictionary above
                                    % we just need to render the layout
                                    run_layout(LayoutModule);

                                false ->
                                    % Variables for layout will be accumulated in ale_pd:app/2
                                    ale_pd:ale(variables_for_layout, []),
                                    Ehtml2 = run_layout(LayoutModule),

                                    % If the layout did not use any variable from the action or
                                    % its view, we only need to cache the content. Otherwise
                                    % we need to cache both the content and variables.
                                    VFL = ale_pd:ale(variables_for_layout),

                                    ThingToCache = case length(VFL) of
                                        0 -> ContentForLayout;
                                        _ -> [{content_for_layout, ContentForLayout} | VFL]
                                    end,
                                    ale_cache:cache(Path, fun() -> ThingToCache end, w),

                                    Ehtml2
                            end
                    end,

                    % Because the result may be cached, we convert EHTML to HTML (io list)
                    % binary for efficiency
                    Html2 = yaws_api:ehtml_expand(Ehtml),
                    list_to_binary(Html2)
            end,

            ale_pd:yaws(html, Html),
            Html
    end.

%% Returns HTML if there is a view to render, undefined otherwise.
handle_request4(ControllerModule, Action) ->
    ActionCachedWithoutLayout = ale_is_cached:is_cached(ControllerModule, action_without_layout, Action),

    % If the action is cached without layout, then variables introduced by this
    % action are remembered so that ale_pd:app/1 can cache them
    Keys1 = case ActionCachedWithoutLayout of
        false -> [];  % Just for suppressing compile time warning
        true  -> ale_pd:keys(app)
    end,

    ControllerModule:Action(),

    case ale_pd:view_module() of
        undefined -> undefined;  % Layout is not called if there is no view

        ViewModule ->
            Ehtml = ViewModule:render(),
            case ActionCachedWithoutLayout of
                false -> ok;

                true ->
                    Keys2 = ale_pd:keys(app),
                    ActionKeys = Keys2 -- Keys1,
                    ale_pd:ale(action_keys, ActionKeys)
            end,

            % Because the result may be cached, we convert EHTML to HTML (io list)
            % then to binary for efficiency
            Html = yaws_api:ehtml_expand(Ehtml),
            list_to_binary(Html)
    end.

%-------------------------------------------------------------------------------

%% Returns true if the action should be halted.
run_before_action(ControllerModule, Action) ->
    case erlang:function_exported(c_application, before_action, 2) andalso
        c_application:before_action(ControllerModule, Action) of
        true -> true;

        false ->
            erlang:function_exported(ControllerModule, before_action, 1) andalso
                ControllerModule:before_action(Action)
    end.

% FIXME
run_layout(LayoutModule) ->
    h_facebook:before_layout(),
    LayoutModule:render().

%% Converts c_hello to hello.
cm2c(ControllerModule) ->
    [$c, $_ | ControllerS] = atom_to_list(ControllerModule),
    list_to_atom(ControllerS).
