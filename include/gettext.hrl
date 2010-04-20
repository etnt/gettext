%% Client header file for Erlang gettext
-ifndef(_GETTEXT_HRL).
-define(_GETTEXT_HRL, true).

-compile({parse_transform,gettext_compile}).

%% Note: Every macro expansion must contain an explicit call to
%% gettext:key2str(String), so that the parse transform can detect it.

-define(TXT(S), gettext:key2str(S)).

-define(TXT2(S, Lang), gettext:key2str(S, Lang)).

%%%
%%% This function is similar to ?FTXT but allows S (format string) to use
%%% args (A) in any order, any number of times or not at all. 
%%% This is needed for translators of po files to be able to write
%%% translations with a natural sentence structure. 
%%% 
-define(STXT(S, A), gettext_format:stxt(?TXT(S),A)).

-define(STXT2(S, A, Lang), gettext_format:stxt(?TXT2(S, Lang),A)).


%%%
%%% IO-Format strings are handled by this macro
%%%
-define(FTXT(S, A), lists:flatten(io_lib:format(?TXT(S),A))).

-define(FTXT2(S, A, Lang), lists:flatten(io_lib:format(?TXT2(S, Lang),A))).


%%%
%%% In case the string is used in a javascript context, we need to take
%%% care of quotes. It is assumed that we are working on an encoding
%%% compatible with ASCII/Latin-1/Unicode.
%%%
%%% NB (11 May 2005): DO NOT USE SINGLE QUOTES IN JAVASCRIPT STRINGS !!!
%%%                   (some browsers don't handle escaped single quotes)
%%%
%%%
-define(JTXT(S), gettext:quotes(?TXT(S))).

-define(JTXT2(S, Lang), gettext:quotes(?TXT2(S, Lang))).

-endif.
