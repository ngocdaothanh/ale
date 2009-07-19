%%% This module is intended of use by application developers.

-module(ale).

-compile(export_all).

%-------------------------------------------------------------------------------

sync() -> make:all([load]).

%-------------------------------------------------------------------------------

yaws(Key, Value)                  -> ale_pd:yaws(Key, Value).
yaws(Key, Value1, Value2)         -> ale_pd:yaws(Key, Value1, Value2).
yaws(Key, Value1, Value2, Value3) -> ale_pd:yaws(Key, Value1, Value2, Value3).
yaws(Key)                         -> ale_pd:yaws(Key).

%-------------------------------------------------------------------------------

app(Key, Value)        -> ale_pd:app(Key, Value).
app(Key)               -> ale_pd:app(Key).
app_add_head(Head)     -> ale_pd:app_add_head(Head).
app_add_script(Script) -> ale_pd:app_add_script(Script).

%-------------------------------------------------------------------------------

arg()       -> ale_pd:arg().
method()    -> ale_pd:method().
uri()       -> ale_pd:uri().
params(Key) -> ale_pd:params(Key).

layout_module(Module)    -> ale_pd:layout_module(Module).
view(Action)             -> ale_pd:view(Action).
view(Controller, Action) -> ale_pd:view(Controller, Action).
view_module(Module)      -> ale_pd:view_module(Module).

%-------------------------------------------------------------------------------

url_for(Controller, Action, Params)          -> ale_routes_gen:url_for(Controller, Action, Params).
url_for(Action, Params) when is_list(Params) -> ale_routes_gen:url_for(Action, Params);
url_for(Controller, Action)                  -> ale_routes_gen:url_for(Controller, Action).
url_for(Action)                              -> ale_routes_gen:url_for(Action).

%-------------------------------------------------------------------------------

cache(Key)               -> ale_cache:cache(Key).
cache(Key, Fun)          -> ale_cache:cache(Key, Fun).
cache(Key, Fun, Options) -> ale_cache:cache(Key, Fun, Options).

%-------------------------------------------------------------------------------

session(Key, Value) -> ale_session:session(Key, Value).
session(Key)        -> ale_session:session(Key).
clear_session()     -> ale_session:clear_session().

flash(Value) -> ale_session:flash(Value).
flash()      -> ale_session:flash().

%-------------------------------------------------------------------------------

ff(Basename, Data)    -> ale_utils:ff(Basename, Data).
md5_hex(Module, Data) -> ale_utils:md5_hex(Module, Data).
gravatar(Email, Size) -> ale_utils:gravatar(Email, Size).
