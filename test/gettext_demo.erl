-module(gettext_demo).

-export([hello_world/0, hello_world/1]).

%% configuration callbacks
-export([gettext_dir/0, gettext_def_lang/0]).

-include("../include/gettext.hrl").

-define(HELLO, "Hello World!").

hello_world() ->
    hello_world("en").

hello_world(LC) ->
    {ok, _} = gettext_server:start({?MODULE, []}),
    io:format("Using ?TXT/1 with default language: ~p~n", [?TXT(?HELLO)]),
    put(gettext_language, "sv"),
    io:format("Using ?TXT/1 with gettext_language set to \"sv\": ~p~n",
              [?TXT(?HELLO)]),
    io:format("Using ?TXT/2 with language=~p: ~p~n",
              [LC, ?TXT2(?HELLO, LC)]),
    io:format("Using ?TXT/2 with language=\"sv\": ~p~n",
              [?TXT2(?HELLO, "sv")]),
    put(gettext_language, "es"),
    io:format("Using ?TXT/1 with gettext_language set to \"es\": ~p~n",
              [?TXT(?HELLO)]),
    ok.


%% configuration callbacks

%% using code:priv_dir(gettext) might not work when building and testing
%% this application, depending on the user's setup, to for this test we just
%% use paths relative to the current directory

gettext_dir() -> ".".  % i.e., po files will be under ./lang/...

gettext_def_lang() -> "en".
