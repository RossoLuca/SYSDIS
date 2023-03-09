-module(logging).

-export([log/3, log/2, log/1]).


log(Id, Format, Data) ->
    FileName = "/dis_sys/drone_" ++ integer_to_list(Id),
    {ok, File} = file:open(FileName, [append]),
    Line = "Drone ~p -> " ++ Format ++ "~n",
    io:format(File, Line, [Id | Data]),
    DEV_MODE = list_to_atom(os:getenv("DEV_MODE", "false")),
    if DEV_MODE == true -> print_to_shell(Line, [Id | Data]); true -> ok end,
    file:close(File).


log(Format, Data) ->
    FileName = "/dis_sys/drone_hub",
    {ok, File} = file:open(FileName, [append]),
    Line = "DRONE HUB -> " ++ Format ++ "~n",
    io:format(File, Line, Data),
    DEV_MODE = list_to_atom(os:getenv("DEV_MODE", "false")),
    if DEV_MODE == true -> print_to_shell(Line, Data); true -> ok end,
    file:close(File).


log(Format) ->
    FileName = "/dis_sys/drone_hub",
    {ok, File} = file:open(FileName, [append]),
    Line = "DRONE HUB -> " ++ Format ++ "~n",
    io:format(File, Line, []),
    DEV_MODE = list_to_atom(os:getenv("DEV_MODE", "false")),
    if DEV_MODE == true -> print_to_shell(Line); true -> ok end,
    file:close(File).

print_to_shell(Format, Data) ->
    io:format(Format, Data).

print_to_shell(Format) ->
    io:format(Format).