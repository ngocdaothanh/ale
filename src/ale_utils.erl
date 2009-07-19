-module(ale_utils).

-compile(export_all).

%% Module: erlang or crypto
%%
%% erlang is faster for short Data:
%% http://erlang.org/pipermail/erlang-questions/2009-January/041400.html
md5_hex(Module, Data) ->
    Md5 = Module:md5(Data),
    lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(Md5)]).

gravatar(Email, Size) ->
    GravatarId = case Email of
        undefined -> "";
        _         -> md5_hex(erlang, Email)
    end,
    Src = io_lib:format("http://www.gravatar.com/avatar.php?size=~p&gravatar_id=~s", [Size, GravatarId]),
    {img, [{src, Src}]}.
