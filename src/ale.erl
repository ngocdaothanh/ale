%%% This module is intended of use by application developers.

-module(ale).

-compile(export_all).

%-------------------------------------------------------------------------------

sync() -> make:all([load]).

%-------------------------------------------------------------------------------

conf(SCOrNamespace, Key, Value) -> ale_pd:conf(SCOrNamespace, Key, Value).
conf(Namespace, Key)            -> ale_pd:conf(Namespace, Key).
conf(SC, Namespace, Key, Value) -> ale_pd:conf(SC, Namespace, Key, Value).


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

gc()        -> ale_pd:gc().
sc()        -> ale_pd:sc().
arg()       -> ale_pd:arg().

method()    -> ale_pd:method().
path()      -> ale_pd:path().
params(Key) -> ale_pd:params(Key).

schema_host_port(Path) -> ale_pd:schema_host_port(Path).
ip()                   -> ale_pd:ip().

layout_module(Module)    -> ale_pd:layout_module(Module).
view(Action)             -> ale_pd:view(Action).
view(Controller, Action) -> ale_pd:view(Controller, Action).
view_module(Module)      -> ale_pd:view_module(Module).

%-------------------------------------------------------------------------------

path(Controller, Action, Params)         -> ale_routes_gen:path(Controller, Action, Params).
path(ControllerOrAction, ActionOrParams) -> ale_routes_gen:path(ControllerOrAction, ActionOrParams).
path(Action)                             -> ale_routes_gen:path(Action).

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

ff(Basename)          -> ale_utils:ff(Basename).
ff(Basename, Data)    -> ale_utils:ff(Basename, Data).

md5_hex(Module, Data) -> ale_utils:md5_hex(Module, Data).

gravatar(Email)            -> ale_utils:gravatar(Email).
gravatar(Email, Size)      -> ale_utils:gravatar(Email, Size).
gravatar(Email, Size, Url) -> ale_utils:gravatar(Email, Size, Url).

mathcha()                       -> ale_utils:mathcha().
mathcha(Answer, EcryptedAnswer) -> ale_utils:mathcha(Answer, EcryptedAnswer).
