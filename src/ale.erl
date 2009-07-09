%% Functions in this module are the APIs of the Ale framework. Applications
%% would need to use these functions.

-module(ale).

-compile(export_all).

%-------------------------------------------------------------------------------

sync() -> make:all([load]).

%-------------------------------------------------------------------------------

yaws(Key, Value)                  -> ale_pd:yaws(Key, Value).
yaws(Key, Value1, Value2)         -> ale_pd:yaws(Key, Value1, Value2).
yaws(Key, Value1, Value2, Value3) -> ale_pd:yaws(Key, Value1, Value2, Value3).
yaws(Key)                         -> ale_pd:yaws(Key).

app(Key, Value) -> ale_pd:app(Key, Value).
app(Key)        -> ale_pd:app(Key).

%-------------------------------------------------------------------------------

arg()        -> ale_pd:arg().
controller() -> ale_pd:controller().
action()     -> ale_pd:action().

layout(Value) -> ale_pd:layout(Value).
layout()      -> ale_pd:layout().

view(Value) -> ale_pd:view(Value).
view()      -> ale_pd:view().

content_for_layout() -> ale_pd:content_for_layout().

script(Script) -> ale_pd:script(Script).
script()       -> ale_pd:script().

%-------------------------------------------------------------------------------

uri() -> ale_pd:uri().

url_for(Controller, Action)       -> ale_routes:url_for(Controller, Action, []).
url_for(Controller, Action, Args) -> ale_routes:url_for(Controller, Action, Args).

%-------------------------------------------------------------------------------

cache(Key, Fun)          -> ale_cache:cache(Key, Fun).
cache(Key, Fun, Options) -> ale_cache:cache(Key, Fun, Options).

%-------------------------------------------------------------------------------

user() -> ale_session:user().

%-------------------------------------------------------------------------------

md5_hex(Data) -> ale_utils:md5_hex(Data).
gravatar(Email, Size) -> ale_utils:gravatar(Email, Size).
