%%%----------------------------------------------------------------------
%%% Created:  27 Oct 2003 by tobbe@bluetail.com
%%% Function: Tools for multi-lingual capabilities,
%%%           similar to GNU gettext.
%%%----------------------------------------------------------------------
-module(gettext).

-export([key2str/1, parse_po/1, lc2lang/1, quotes/1,
	 parse_po_bin/1, all_lang/0, key2str/2, all_lcs/0,
	 reload_custom_lang/1, unload_custom_lang/1,
	 recreate_db/0]).

-include("gettext.hrl").



reload_custom_lang(Lang) ->
    gettext_server:reload_custom_lang(Lang).

unload_custom_lang(Lang) ->
    gettext_server:unload_custom_lang(Lang).

all_lcs() ->
    gettext_server:all_lang().

recreate_db() ->
    gettext_server:recreate_db().


%%% --------------------------------------------------------------------
%%% This is the lookup routines.
%%% --------------------------------------------------------------------

%%% Hopefully, the surrounding code has done its job and
%%% put the language to be used in the process dictionary.
key2str(Key) -> 
    key2str(Key, get(gettext_language)).

key2str(Key, "a") -> 
    a_language(Key, "");
key2str(Key, Lang) -> 
    gettext_server:key2str(Key, Lang).

a_language([], Acc) ->
    lists:reverse(Acc);
a_language([$<|Tl] = Key, Acc) ->
    {RestKey, Html} = look_for_special(Key, [], $<),
    a_language(RestKey, Html++Acc);
a_language([$\s = Hd |Tl], Acc) ->
    a_language(Tl, [Hd|Acc]);
a_language([$~ = Hd |Tl], Acc) ->
    [Hd2|Tl2] = Tl, 
    a_language(Tl2, [Hd2, $~ |Acc]);
a_language([$$|Tl] = Key, Acc) ->
    {RestKey, DollarFormat} = look_for_special(Tl, [], $$),
    a_language(RestKey, DollarFormat++[$$|Acc]);
a_language([Hd|Tl] = Key, Acc) ->
    if
	Hd > 64 andalso Hd < 91 ->
	    a_language(Tl, [$a |Acc]);
	Hd > 96 andalso Hd < 123 ->
	    a_language(Tl, [$a |Acc]);
	Hd > 47 andalso Hd < 58 ->
	    a_language(Tl, [$b |Acc]);
	Hd > 191 ->
	    a_language(Tl, [$a |Acc]);
	true ->
	    a_language(Tl, [Hd |Acc])
    end.

look_for_special([], Acc, ToLookFor) ->
    {Acc, []};
look_for_special([Hd|Tl] = Key, Acc, ToLookFor) ->
    case Hd of
	ToLookFor ->
	    {Tl, [Hd|Acc]};
	_ ->
	    look_for_special(Tl, [Hd|Acc], ToLookFor)
    end.
    

%%% --------------------------------------------------------------------
%%% In case the string is used in a javascript context,
%%% we need to take care of quotes.
%%% --------------------------------------------------------------------

quotes([$'|T]) -> [$\\,$' | quotes(T)];
quotes([$"|T]) -> [$\\,$" | quotes(T)];
quotes([H|T])  -> [H      | quotes(T)];
quotes([])     -> [].


%%% --------------------------------------------------------------------
%%% Parse a PO-file
%%% --------------------------------------------------------------------

parse_po(Fname) ->
    {ok,Bin} = file:read_file(Fname),
    parse_po_bin(Bin).

parse_po_bin(Bin) ->
    parse_po_file(to_list(Bin)).

parse_po_file("msgid" ++ T) ->
    {Key, R0} = get_po_string(T),
    {Val, Rest} = get_msgstr(R0),
    [{Key,Val} | parse_po_file(Rest)];
parse_po_file([_ | T]) ->
    parse_po_file(T);
parse_po_file([]) ->
    [].

get_msgstr("msgstr" ++ T) ->
    get_po_string(T);
get_msgstr([_ | T]) ->
    get_msgstr(T).

%%%
%%% A PO-string has the same syntax as a C character string.
%%% For example:
%%%
%%%   msgstr ""
%%%     "Hello "
%%%
%%%     "\\World\n"
%%%
%%% Is parsed as: "Hello \World\n"
%%%
get_po_string([$\s|T]) -> get_po_string(T);
get_po_string([$\r|T]) -> get_po_string(T);
get_po_string([$\n|T]) -> get_po_string(T);
get_po_string([$\t|T]) -> get_po_string(T);
get_po_string([$"|T])  -> header_info(eat_string(T)).       %"make emacs happy

%%% only header-info has empty po-string !
header_info({"",R}) -> {?GETTEXT_HEADER_INFO, R};  
header_info(X)      -> X.

eat_string(S) ->
    eat_string(S,[]).

eat_string([$\\,$"|T], Acc)   -> eat_string(T, [$"|Acc]);   % unescape !
eat_string([$\\,$\\ |T], Acc) -> eat_string(T, [$\\|Acc]);  % unescape !
eat_string([$\\,$n |T], Acc)  -> eat_string(T, [$\n|Acc]);  % unescape !
eat_string([$"|T], Acc)       -> eat_more(T,Acc);           %"make emacs happy
eat_string([H|T], Acc)        -> eat_string(T, [H|Acc]).

eat_more([$\s|T], Acc) -> eat_more(T, Acc);
eat_more([$\n|T], Acc) -> eat_more(T, Acc);
eat_more([$\r|T], Acc) -> eat_more(T, Acc);
eat_more([$\t|T], Acc) -> eat_more(T, Acc);
eat_more([$"|T], Acc)  -> eat_string(T, Acc);               %"make emacs happy
eat_more(T, Acc)       -> {lists:reverse(Acc), T}.


to_list(A) when atom(A)    -> atom_to_list(A);
to_list(I) when integer(I) -> integer_to_list(I);
to_list(B) when binary(B)  -> binary_to_list(B);
to_list(L) when list(L)    -> L.


%%% --------------------------------------------------------------------
%%% Language Codes
%%% --------------------------------------------------------------------

lc2lang(LC) -> 
    case iso639:lc3lang(LC) of
	""   -> iso639:lc2lang(LC);  % backward compatible
	Lang -> Lang
    end.
    

all_lang() -> iso639:all3lang().

