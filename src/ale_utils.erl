-module(ale_utils).

-compile(export_all).

md5_hex(Data) ->
    Md5 = erlang:md5(Data),
    lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(Md5)]).

gravatar(Email, Size) ->
    GravatarId = case Email of
        undefined -> "";
        _         -> md5_hex(Email)
    end,
    Src = io_lib:format("http://www.gravatar.com/avatar.php?size=~p&gravatar_id=~s", [Size, GravatarId]),
    {img, [{src, Src}]}.
