%%% start, out, and out404 are called by Yaws as configured in yaws.conf

-module(ale_yaws_mod).

-compile(export_all).

-include("ale.hrl").

-define(MAX_R, 30).
-define(MAX_T, 60).

%-------------------------------------------------------------------------------

%% Called after Yaws has loaded.
start(SC) ->
    SC2 = ale_sc:set_docroot(SC),
    start_children(SC2).

% NOTE about caching:
% * 404 and 500 error pages are not cached
% * Pages are not cached to files on disk (then let Yaws handle the files). They
%   are cached in memory the same way as actions, fragments, and objects for 2
%   reasons: (1) memory caching as Yaws file caching are the same speed
%   (normally ~3000req/s), (2) functionalities like auto expiration 

% FIXME: currently cache only works if there is a view to render

out(Arg) ->
    Uri = Arg#arg.server_path,  % Ale only supports mounting to /

    case Uri of
        % Give Yaws a chance to server static file from "/static"
        % See http://groups.google.com/group/nitrogenweb/browse_thread/thread/c2ce70f696b77c9e
        % If Yaws cannot find a file at the Uri, out404 below will be called
        %
        % NOTE: Yaws 1.84 crashes if we put({yaws, page}, Arg#arg.server_path)
        % and return page
        "/static" ++ _ -> {page, Uri};

        _ ->
            Method = rest_method(Arg),
            case ale_routes:route_uri(Method, Uri) of
                no_route ->
                    c_application:error_404(),
                    case ale_pd:view() of
                        undefined -> ignore;
                        View      -> ale_pd:yaws(ehtml, View:render())
                    end;

                % Long:  c_hello
                % Short: hello
                {LongController, Action, Args} ->
                    try
                        handle_request1(Arg, Method, Uri, LongController, Action, Args)
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
                            case ale_pd:view() of
                                undefined -> ignore;
                                View      -> ale_pd:yaws(ehtml, View:render())
                            end
                    end
            end,
            ale_pd:yaws()
    end.

out404(_Arg, _GC, _SC) ->
    c_application:error_404(),
    case ale_pd:view() of
        undefined -> ignore;
        View      -> ale_pd:yaws(ehtml, View:render())
    end,
    ale_pd:yaws().

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
    case supervisor:start_child(yaws_sup, ChildSpec) of
        {error, Reason} ->
            error_logger:error_msg("Error starting Ale application:~n~p~n", [Reason]),
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

handle_request1(Arg, Method, Uri, LongController, Action, Args) ->
    case page_cached(LongController, Action) of
        true ->
            Html = ale_cache:c(Uri, fun() ->
                handle_request2(Arg, Method, Uri, LongController, Action, Args)
            end),
            ale_pd:yaws(html, Html);

        false -> handle_request2(Arg, Method, Uri, LongController, Action, Args)
    end.

%% Returns HTML if the request is not halted by a before filter or action cached.
handle_request2(Arg, Method, Uri, LongController, Action, Args) ->
    % Set environment variables to the process dictionary here (not in
    % handle_request1 because they are not used there) because the application
    % may need these variables
    ale_pd:arg(Arg),
    ale_pd:method(Method),
    ale_pd:uri(Uri),
    [$c, $_ | ControllerS] = atom_to_list(LongController),
    Controller = list_to_atom(ControllerS),
    ale_pd:controller(Controller),
    ale_pd:action(Action),

    case run_before_filters(LongController, Action, Args) of
        true -> ignore;    % Can't be page cached because halted by a before filter

        false ->
            case action_cached_with_layout(LongController, Action) of
                true ->
                    Html = ale_cache:c(Uri, fun() ->
                        handle_request3(Uri, LongController, Action, Args)
                    end),
                    ale_pd:yaws(html, Html);    % Can't be page cached because action cached

                false -> handle_request3(Uri, LongController, Action, Args)
            end
    end.

%% Returns HTML if there is a view to render.
handle_request3(Uri, LongController, Action, Args) ->
    ActionCachedWithoutLayout = action_cached_without_layout(LongController, Action),
    {ContentForLayout, FromCache} = case ActionCachedWithoutLayout of
        true ->
            % If taken from cache then it is binary or list:
            % * If list, it is:
            % [{content_for_layout, Binary} | Variables to be put to process dictionary for layout to use]
            % * If binary, it is content_for_layout
            case ale_cache:c(Uri) of
                not_found -> {handle_request4(LongController, Action, Args), false};

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

        false ->
            % Binary
            {handle_request4(LongController, Action, Args), undefined}
    end,

    Html = case ale_pd:layout() of
        undefined ->
            case ActionCachedWithoutLayout of
                false -> ok;

                % Cache ContentForLayout if is was not from cache
                true ->
                    case FromCache of
                        true  -> ok;
                        false -> ale_cache:c(Uri, fun() -> ContentForLayout end)
                    end
            end,
            ContentForLayout;

        Layout ->
            ale_pd:content_for_layout(ContentForLayout),

            % See above and ale_pd:app/1
            Ehtml = case ActionCachedWithoutLayout of
                false -> Layout:render();

                true ->
                    % Cache ContentForLayout if is was not from cache
                    case FromCache of
                        true ->
                            % Things have been put into the process dictionary above
                            % we just need to render the layout
                            Layout:render();

                        false ->
                            % Variables for layout will be accumulated in ale_pd:app/2
                            ale_pd:ale(variables_for_layout, []),
                            Ehtml2 = Layout:render(),

                            % If the layout did not use any variable from the action or
                            % its view, we only need to cache the content. Otherwise
                            % we need to cache the content and variables.
                            VFL = ale_pd:ale(variables_for_layout),
                            ThingToCache = case length(VFL) of
                                0 -> ContentForLayout;
                                _ -> [{content_for_layout, ContentForLayout} | VFL]
                            end,
                            ale_cache:c(Uri, fun() -> ThingToCache end),

                            Ehtml2
                    end
            end,

            % Because the result may be cached, we convert to HTML then to
            % binary for efficiency
            Html2 = yaws_api:ehtml_expand(Ehtml),
            list_to_binary(Html2)
    end,

    ale_pd:yaws(html, Html),
    Html.

%% Returns HTML if there is a view to render.
handle_request4(LongController, Action, Args) ->
    View = controller_to_view(LongController, Action),
    ale_pd:view(View),

    apply(LongController, Action, Args),

    case ale_pd:view() of
        undefined -> ignore;    % Layout is not called if there is no view

        View2 ->
            Ehtml = View2:render(),

            % Because the result may be cached, we convert to HTML then to
            % binary for efficiency
            Html = yaws_api:ehtml_expand(Ehtml),
            list_to_binary(Html)
    end.

page_cached(LongController, Action) ->
    code:ensure_loaded(LongController),
    case erlang:function_exported(LongController, cached_pages, 0) of
        true  -> lists:member(Action, LongController:cached_pages());
        false -> false
    end.

action_cached_with_layout(LongController, Action) ->
    code:ensure_loaded(LongController),
    case erlang:function_exported(LongController, cached_actions_with_layout, 0) of
        true  -> lists:member(Action, LongController:cached_actions_with_layout());
        false -> false
    end.

action_cached_without_layout(LongController, Action) ->
    code:ensure_loaded(LongController),
    case erlang:function_exported(LongController, cached_actions_without_layout, 0) of
        true  -> lists:member(Action, LongController:cached_actions_without_layout());
        false -> false
    end.

%% Returns true if the action should be halted.
run_before_filters(LongController, Action, Args) ->
    case erlang:function_exported(c_application, before_filter, 3) andalso
        c_application:before_filter(ale_pd:controller(), Action, Args) of
        true -> true;

        false ->
            erlang:function_exported(LongController, before_filter, 2) andalso
                LongController:before_filter(Action, Args)
    end.

%% Converts c_hello and show to v_hello_show
controller_to_view(LongController, Action) ->
    [$c, $_ | Base] = atom_to_list(LongController),
    list_to_atom("v_" ++ Base ++ "_" ++ atom_to_list(Action)).
