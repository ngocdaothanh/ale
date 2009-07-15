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

%% Each request has its own processing process. If you want to share variables
%% across functions, use app/2 and app/1.
app(Key, Value) -> ale_pd:app(Key, Value).
app(Key)        -> ale_pd:app(Key).

%-------------------------------------------------------------------------------

arg()        -> ale_pd:arg().
method()     -> ale_pd:method().
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

url_for(Controller, Action) -> ale_routes:url_for(Controller, Action, []).

%% Args must be a list. If the a list element is an integer or a float, Ale will
%% convert it to a string so that Args become a list of strings.
url_for(Controller, Action, Args) ->
    Args2 = lists:map(
        fun
            (Arg) when is_integer(Arg)-> integer_to_list(Arg);
            (Arg) when is_float(Arg)  -> float_to_list(Arg);
            (Arg) -> Arg
        end,
        Args
    ),
    ale_routes:url_for(Controller, Action, Args2).

%-------------------------------------------------------------------------------

%% Returns {ok, Value} or not_found.
cache(Key) -> ale_cache:c(Key).

cache(Key, Fun) -> cache(Key, Fun, []).

%% Tries to read cache and returns value. If not found then runs Fun to get
%% value, write then returns it. Note that the return value form is different
%% from that of c/1.
%%
%% If you want to cache string or io list, for efficiency remember to set
%% Options to ask this function to convert it to binary before caching so that
%% serializing/deserializing is not performed every time the cache is read.
%%
%% Options:
%%   w                : always use Fun to write to the cache, e.g. overwrite any existing value
%%   ehtml            : the return value of Fun is EHTML and will be converted to HTML then to binary
%%   ehtmle           : the return value of Fun is EHTML and will be converted to HTML then to binary
%%   iolist           : the return value of Fun is io list and will be converted to binary
%%   {ttl, Seconds}   : time to live until cache is expired
%%   {slide, Boolean} : slide expiration if the cached object is read
cache(Key, Fun, Options) -> ale_cache:c(Key, Fun, Options).

%-------------------------------------------------------------------------------

user() -> ale_session:user().

%-------------------------------------------------------------------------------

md5_hex(Data) -> ale_utils:md5_hex(Data).
gravatar(Email, Size) -> ale_utils:gravatar(Email, Size).
