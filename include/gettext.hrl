-ifndef(_GETTEXT_HRL).
-define(_GETTEXT_HRL, true).

-compile({parse_transform,gettext_compile}).

-define(TXT(S), gettext:key2str(S)).

-define(TXT2(S, Lang), gettext:key2str(S, Lang)).

%%%
%%% This function is similar to ?FTXT but alows S (format string) to use
%%% args (A) in any order, any number of times or not at all. 
%%% This is needed for translators of po files to be able to write
%%% translations with a natural scentence structure. 
%%% 
-define(STXT(S, A), gettext_format:stxt(?TXT(S),A)).

-define(STXT2(S, A, Lang), gettetxt_format:stxt(?TXT(S, Lang),A)).


%%%
%%% IO-Format strings are handled by this macro
%%%
-define(FTXT(S, A), lists:flatten(io_lib:format(?TXT(S),A))).

-define(FTXT2(S, A, Lang), lists:flatten(io_lib:format(?TXT(S, Lang),A))).


%%%
%%% In case the string is used in a javascript context,
%%% we need to take care of quotes.
%%%
%%% I guess we must assume that quotes and backslashes
%%% always are represented as in good old ASCII...
%%%
%%% NB (11 May 2005): DO NOT USE SINGLE QUOTES IN JAVASCRIPT STRINGS !!!
%%%                   (some browsers doesn't handle escaped single quotes)
%%%
%%%
-define(JTXT(S), gettext:quotes(gettext:key2str(S))).

-define(JTXT2(S, Lang), gettext:quotes(gettext:key2str(S, Lang))).

-endif.
