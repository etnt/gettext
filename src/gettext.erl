%%%----------------------------------------------------------------------
%%% Created:  27 Oct 2003 by tobbe@bluetail.com
%%% Function: Tools for multi-lingual capabilities,
%%%           similar to GNU gettext.
%%%----------------------------------------------------------------------
-module(gettext).

-export([parse_po/1, lc2lang/1, quotes/1,
	 parse_po_bin/1, all_lang/0, 
	 key2str/1, key2str/2, key2str/3, 
	 all_lcs/0, all_lcs/1,
	 reload_custom_lang/1, reload_custom_lang/2,
	 unload_custom_lang/1, unload_custom_lang/2,
	 recreate_db/0, recreate_db/1,
	 gettext_dir/0, gettext_dir/1,
	 change_gettext_dir/1, change_gettext_dir/2,
	 default_lang/0, default_lang/1,
	 store_pofile/2, store_pofile/3, 
	 lang2cset/1, lang2cset/2]).

-include("gettext_internal.hrl").

-define(DEFAULT_SERVER, gettext_server).

%%% --------------------------------------------------------------------
%%% This is the lookup routine.
%%% --------------------------------------------------------------------
%%% Hopefully, the surrounding code has done its job and
%%% put the language to be used in the process dictionary.
key2str(Key) -> 
    key2str(Key, get(gettext_language)).

key2str(Key, "a") -> 
    a_language(Key, []);
key2str(Key, Lang) when is_list(Key) -> 
    key2str(?DEFAULT_SERVER, Key, Lang);
key2str(Server, Key) when is_atom(Server) ->
    key2str(Server, Key, get(gettext_language)).

key2str(Server, Key, Lang) ->
            gen_server:call(Server, {key2str, Key, Lang}, infinity).


%%% --------------------------------------------------------------------

reload_custom_lang(Lang) ->
    reload_custom_lang(?DEFAULT_SERVER, Lang).

reload_custom_lang(Server, Lang) ->
    gen_server:call(Server, {reload_custom_lang, Lang}, infinity).

unload_custom_lang(Lang) ->
    unload_custom_lang(?DEFAULT_SERVER, Lang).

unload_custom_lang(Server, Lang) ->
    gen_server:call(Server, {unload_custom_lang, Lang}, infinity).

all_lcs() ->
    all_lcs(?DEFAULT_SERVER).

all_lcs(Server) ->
    gen_server:call(Server, all_lcs, infinity).


recreate_db() ->
    recreate_db(?DEFAULT_SERVER). 
   
recreate_db(Server) ->
    gen_server:call(Server, recreate_db, infinity).

%%--------------------------------------------------------------------

gettext_dir() ->
    gettext_dir(?DEFAULT_SERVER).

gettext_dir(Server) ->
    gen_server:call(Server, gettext_dir, infinity).


change_gettext_dir(Dir) ->
    change_gettext_dir(?DEFAULT_SERVER, Dir).

change_gettext_dir(Server, Dir) ->
    gen_server:call(Server, {change_gettext_dir, Dir}, infinity).


default_lang() ->
    default_lang(?DEFAULT_SERVER).

default_lang(Server) ->
    gen_server:call(Server, default_lang, infinity).


%%--------------------------------------------------------------------

store_pofile(Lang, File) when is_binary(File) ->
    store_pofile(?DEFAULT_SERVER, Lang, File).

store_pofile(Server, Lang, File) when is_binary(File) ->
    gen_server:call(Server, {store_pofile, Lang, File}, infinity).

%%--------------------------------------------------------------------

lang2cset(Lang) ->
    lang2cset(?DEFAULT_SERVER, Lang).

lang2cset(Server, Lang) ->
    gen_server:call(Server, {lang2cset, Lang}, infinity).

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


to_list(A) when is_atom(A)    -> atom_to_list(A);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(B) when is_binary(B)  -> binary_to_list(B);
to_list(L) when is_list(L)    -> L.


%%% --------------------------------------------------------------------
%%% Language Codes
%%% --------------------------------------------------------------------

lc2lang(LC) -> 
    case iso639:lc3lang(LC) of
	""   -> iso639:lc2lang(LC);  % backward compatible
	Lang -> Lang
    end.
    

all_lang() -> iso639:all3lang().


%%% -------------------------------------------------------------------
%%% Function name: 
%%% a_language
%%% Function purpose:
%%% A special function to convert all strings in a po-file to a's Used
%%% to check what text has been textified and what hasn't been
%%% textified. Also converts numbers to b's instead.  The intended
%%% language ISO to be used is ISO/IEC 8859-1 (Latin-1)
%%% The check in the last clause: the intervals 
%%% 1. Hd > 64 andalso Hd < 91   = Uppercase Alpha numerical characters
%%% 2. Hd > 96 andalso Hd < 123  = Lowercase Alpha numerical characters
%%% 3. Hd > 191 andalso Hd < 256 = Special language characters (Ex. å)
%%% 3. Hd > 47 andalso Hd < 58   = Numbers
%%% sees to that only Alphanumerical characters is replaced, to keep 
%%% special characters, so that the context will remain to a higher 
%%% degree.
%%% -------------------------------------------------------------------
a_language([], Acc) ->
    lists:reverse(Acc);
a_language([$<|Tl] = Key, Acc) ->
    {RestKey, Html} = search_for(Key, [], $<),
    a_language(RestKey, Html++Acc);
a_language([$\s = Hd |Tl], Acc) ->
    a_language(Tl, [Hd|Acc]);
a_language([$~ = Hd |Tl], Acc) ->
    [Hd2|Tl2] = Tl, 
    a_language(Tl2, [Hd2, $~ |Acc]);
a_language([$$|Tl], Acc) ->
    {RestKey, DollarFormat} = search_for(Tl, [], $$),
    a_language(RestKey, DollarFormat++[$$|Acc]);
a_language([Hd|Tl], Acc) ->
    Char = if
	    (Hd > 64 andalso Hd < 91) orelse  % See function spec above (1.)
	    (Hd > 96 andalso Hd < 123) orelse % See function spec above (2.) 
	    (Hd > 191 andalso Hd < 256) ->    % See function spec above (3.)
		$a;
	    Hd > 47 andalso Hd < 58 ->
		$b;
	    true ->
		Hd
	end,
    a_language(Tl, [Char |Acc]).

%%% -------------------------------------------------------------------
%%% Function name: 
%%% search_for
%%% Function purpose:
%%% A function, used as a help-function to a_language. It's mostly 
%%% to be used to look for html so that links and html formatting 
%%% doesn't break when trying to converting strings to a's. The html are
%%% detected by matching for < and > and passes that text-chunk on to
%%% a_language and then a_language does it's magic and continues
%%% working on the rest of the string.
%%% --------------------------------------------------------------------
search_for([], Acc, _ToLookFor) ->
    {Acc, []};
search_for([Hd|Tl], Acc, ToLookFor) ->
    case Hd of
	ToLookFor ->
	    {Tl, [Hd|Acc]};
	_ ->
	    search_for(Tl, [Hd|Acc], ToLookFor)
    end.
