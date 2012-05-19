-module(gettext_demo).

-export([hello_world/0, hello_world/1]).

-include("../include/gettext.hrl").

hello_world() ->
    hello_world("en").

hello_world(LC) ->
    io:format("Using ?TXT/2: ~p~n", [?TXT2("Hello World", LC)]),
    put(gettext_language, "sv"),
    io:format("Using ?TXT/1: ~p~n", [?TXT("Hello World")]).
