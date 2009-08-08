-module(ale_utils).

-compile(export_all).

-include("ale.hrl").

-define(DEFAULT_GRAVATAR_SIZE, 80).

ff(Basename) -> ff(Basename, []).

%% Scans the project directory to find the first file with Basename, then reads
%% the file and apply io_lib:format/2. This function can be used to render
%% JavaScript template file.
ff(Basename, Data) ->
    Format = ale_cache:cache("ale_utils:ff/" ++ Basename, fun() ->
        case find_file(Basename, [?ALE_ROOT]) of
            undefined ->
                Reason = io_lib:format("File not found: ~s", Basename),
                erlang:error(Reason);

            File ->
                {ok, Binary} = file:read_file(File),
                Binary
        end
    end),
    io_lib:format(Format, Data).

%% Returns undefined if not found or the full file path.
find_file(_Basename, []) -> undefined;
find_file(Basename, [Dir | Rest]) ->
    Path = filename:join([Dir, Basename]),
    case filelib:is_regular(Path) of
        true -> Path;

        false ->
            Dirs = [D || D <- filelib:wildcard(Dir ++ "/*"), filelib:is_dir(D)],
            find_file(Basename, Dirs ++ Rest)
    end.

%-------------------------------------------------------------------------------

%% Module: erlang or crypto
%%
%% erlang is faster for short Data:
%% http://erlang.org/pipermail/erlang-questions/2009-January/041400.html
md5_hex(Module, Data) ->
    Md5 = Module:md5(Data),
    lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(Md5)]).

gravatar(Email) ->
    gravatar(Email, ?DEFAULT_GRAVATAR_SIZE).

gravatar(Email, Size) ->
    gravatar(Email, Size, "http://gravatar.com").

gravatar(Email, Size, Url) ->
    GravatarId = case Email of
        undefined -> "";
        _         -> md5_hex(erlang, Email)
    end,
    Src = io_lib:format("http://www.gravatar.com/avatar.php?size=~p&gravatar_id=~s", [Size, GravatarId]),
    {a, [{href, Url}], {img, [{src, Src}]}}.

%-------------------------------------------------------------------------------

%% Returns {Question, EcryptedAnswer}, both are io lists.
%%
%% Question: +, -, or x simple math question.
mathcha() ->
    random:seed(now()),
    case random:uniform(3) of
        1 ->
            O = " + ",
            A = random:uniform(100),
            B = random:uniform(100),
            C = A + B;

        2 ->
            O = " - ",
            A = random:uniform(100),
            B = random:uniform(100),
            C = A - B;

        3 ->
            O = " x ",
            A = random:uniform(30),
            B = random:uniform(30),
            C = A*B
    end,
    Question = [integer_to_list(A), O, integer_to_list(B),  " = ?"],
    Salt = mathcha_salt(),
    EcryptedAnswer = md5_hex(erlang, [Salt, integer_to_list(C)]),
    {Question, EcryptedAnswer}.

%% Returns bool().
mathcha(Answer, EncryptedAnswer) ->
    Salt = mathcha_salt(),
    EncryptedAnswer == md5_hex(erlang, [Salt, Answer]).

mathcha_salt() ->
    {H, _M, _S} = time(),
    H.
