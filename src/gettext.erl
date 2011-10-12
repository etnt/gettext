%% -------------------------------------------------------------------------
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% @copyright 2003 Torbjörn Törnkvist
%% @author Torbjörn Törnkvist <tobbe@tornkvist.org>
%% @doc Erlang Gettext tools for multi-lingual capabilities

-module(gettext).

-export([parse_po/1, lc2lang/1,
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

-export([get_app_key/2
         , mk_polish_style_header/1
         , fixed_last_translator/0
         , fixed_revision_date/0
         , create_date/0 
         , charset/0
         , team/0
         , org_name/0
         , copyright/0
         , callback_mod/0
         , write_pretty/2
         , get_language_name/1
        ]).

-include("gettext_internal.hrl").

-define(DEFAULT_SERVER, gettext_server).

%%% --------------------------------------------------------------------
%%% This is the lookup routine.
%%% --------------------------------------------------------------------
%%% Hopefully, the surrounding code has done its job and
%%% put the language to be used in the process dictionary.

%% @doc Get the translation string for a key using the current language.
%% 
%% This is usually not called directly, but via the `?TXT(Key)' macro.

key2str(Key) -> 
    key2str(Key, get(gettext_language)).

%% @doc Get the translation string for a key and a specific language
%% and/or using a named gettext server. If no server is specified, the
%% default server is used. If no language is specified, the current
%% language is used.
%% 
%% This is usually not called directly, but via the `?TXT2(Key,Lang)'
%% macro.

key2str(Key, Lang) when is_list(Key) -> 
    key2str(?DEFAULT_SERVER, Key, Lang);
key2str(Server, Key) when is_atom(Server) ->
    key2str(Server, Key, get(gettext_language)).

%% @doc Get the translation string for a key and a specific language
%% using the named gettext server.

key2str(_Server, Key, "a") -> 
    a_language(Key);
key2str(Server, Key, Lang) ->
    gen_server:call(Server, {key2str, Key, Lang}, infinity).


%%% --------------------------------------------------------------------

%% @doc Reloads the PO-file for a language.

reload_custom_lang(Lang) ->
    reload_custom_lang(?DEFAULT_SERVER, Lang).

reload_custom_lang(Server, Lang) ->
    gen_server:call(Server, {reload_custom_lang, Lang}, infinity).


%% @doc Clears the data for a language from the server.

unload_custom_lang(Lang) ->
    unload_custom_lang(?DEFAULT_SERVER, Lang).

unload_custom_lang(Server, Lang) ->
    gen_server:call(Server, {unload_custom_lang, Lang}, infinity).


%% @doc Returns the list of languages used on the server

all_lcs() ->
    all_lcs(?DEFAULT_SERVER).

all_lcs(Server) ->
    gen_server:call(Server, all_lcs, infinity).


%% @doc Recreates the database file from scratch.

recreate_db() ->
    recreate_db(?DEFAULT_SERVER). 
   
recreate_db(Server) ->
    gen_server:call(Server, recreate_db, infinity).


%% @doc Returns the current data directory used by the server.

gettext_dir() ->
    gettext_dir(?DEFAULT_SERVER).

gettext_dir(Server) ->
    gen_server:call(Server, gettext_dir, infinity).


%% @doc Changes the data directory used by the server and re-reads all
%% server data from the new location.

change_gettext_dir(Dir) ->
    change_gettext_dir(?DEFAULT_SERVER, Dir).

change_gettext_dir(Server, Dir) ->
    gen_server:call(Server, {change_gettext_dir, Dir}, infinity).


%% @doc Returns the default language used by the server.

default_lang() ->
    default_lang(?DEFAULT_SERVER).

default_lang(Server) ->
    gen_server:call(Server, default_lang, infinity).


%% @doc Stores a binary as the new PO-file for a language.

store_pofile(Lang, Binary) when is_binary(Binary) ->
    store_pofile(?DEFAULT_SERVER, Lang, Binary).

store_pofile(Server, Lang, Binary) when is_binary(Binary) ->
    gen_server:call(Server, {store_pofile, Lang, Binary}, infinity).


%% @doc Returns the character set used for a language.

lang2cset(Lang) ->
    lang2cset(?DEFAULT_SERVER, Lang).

lang2cset(Server, Lang) ->
    gen_server:call(Server, {lang2cset, Lang}, infinity).


%% @doc Pretty print PO strings.

write_pretty(Str, Fd) ->
    gettext_compile:write_pretty(Str, Fd).

%%% --------------------------------------------------------------------
%%% Parse a PO-file
%%% --------------------------------------------------------------------

%% @doc Read and parse a PO-file.

parse_po(Fname) ->
    {ok,Bin} = file:read_file(Fname),
    parse_po_bin(Bin).

%% @doc Parse a PO-file given as a binary.

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
    case gettext_iso639:lc3lang(LC) of
	""   -> gettext_iso639:lc2lang(LC);  % backward compatible
	Lang -> Lang
    end.
    

all_lang() -> gettext_iso639:all3lang().


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
%%% 3. Hd > 191 andalso Hd < 256 = Special language characters (Ex. ï¿½)
%%% 3. Hd > 47 andalso Hd < 58   = Numbers
%%% sees to that only Alphanumerical characters is replaced, to keep 
%%% special characters, so that the context will remain to a higher 
%%% degree.
%%% -------------------------------------------------------------------
a_language(Key) ->
    a_language(Key, []).

a_language([], Acc) ->
    lists:reverse(Acc);
a_language([$<|_Tl] = Key, Acc) ->
    {RestKey, Html} = search_for(Key, [], $<),
    a_language(RestKey, Html++Acc);
a_language([$\s = Hd |Tl], Acc) ->
    a_language(Tl, [Hd|Acc]);
a_language([$~ = Hd |Tl], Acc) ->
    [Hd2|Tl2] = Tl, 
    a_language(Tl2, [Hd2, Hd |Acc]);
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

%% Get an application environment variable; fallback to a default value.
get_app_key(Key, Default) ->
    %% Crash if not-loadable...
    case application:load(gettext) of
        ok                               -> get_env(Key, Default);
        {error,{already_loaded,gettext}} -> get_env(Key, Default)
    end.

get_env(Key, Default) ->
    case application:get_env(gettext, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.


fixed_last_translator() ->
    get_app_key(fixed_last_translator, "Gettext-POlish system").

fixed_revision_date() ->
    get_app_key(fixed_revision_date, create_date()).

create_date() ->
    get_app_key(create_date, "2006-07-01 16:45+0200").

charset() ->
    get_app_key(charset, "iso-8859-1").
    
team() ->
    get_app_key(team, "Team <info@team.com>").
    
org_name() ->
    get_app_key(team, "Organization").
    
copyright() ->
    get_app_key(copyright, "YYYY Organization").
    
callback_mod() ->
    get_app_key(callback_mod, gettext_server).



mk_polish_style_header(LC) ->
    OrgName = org_name(),
    mk_polish_style_header(
      OrgName++" PO file for "++get_language_name(LC),
      copyright(),
      create_date(),
      fixed_revision_date(),
      fixed_last_translator(),
      team(),
      charset()
     ).

mk_polish_style_header(Header, CopyRight, CreateDate, RevDate, 
                       LastTranslator, Team, Charset) ->
    "# "++Header++"\n"
        "# Copyright (C) "++CopyRight++"\n"
        "#\n"
        "msgid \"\"\n"
        "msgstr \"\"\n"
        "\"Project-Id-Version: PACKAGE VERSION\\n\"\n"
        "\"POT-Creation-Date: "++CreateDate++"\\n\"\n"
        "\"PO-Revision-Date: "++RevDate++"\\n\"\n"
        "\"Last-Translator: "++LastTranslator++">\\n\"\n"
        "\"Language-Team: "++Team++"\\n\"\n"
        "\"MIME-Version: 1.0\\n\"\n"
        "\"Content-Type: text/plain; charset="++Charset++"\\n\"\n"
        "\"Content-Transfer-Encoding: 8bit\\n\"\n".

get_language_name(undefined) -> "";
get_language_name(LC)        -> gettext_iso639:lc2lang(LC).
