%% @private
%% @author Håkan Stenholm <hokan@kreditor.se>
%%
%% @doc Use this module to validate po files returned by translators. 
%%      This validator assumes the po file to be a valid (parsable without 
%%      errors by gettext/poEdit/GNU po file tools).
%%      Its use is to check for errors introduced by translators in the 
%%      msgstr part in the po file, each msgid-msgstr is checked for the 
%%      following:
%%
%%      * incorrect FTXT argument usage
%%      * incorrect STXT argument usage
%%      * incorrect whitespace at front/tail of text
%%      * missing translations
%%      * incorrect punctuation at the end of texts
%%      * changes in html structure, tag names and atributes used

-module(gettext_validate).

%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export ([
	  validate/1,
	  validate/2
	 ]).  

%% ----------------------------------------------------------------------------
-define(IS_CHAR(Char), 
	is_integer(Char), ((Char >= 0) andalso (Char =< 255))).

%%=============================================================================
%% External functions
%%=============================================================================

%% ----------------------------------------------------------------------------
%% @spec validate(PoFilePath::string(), IgnoreFilePath::""|string()) -> term() 
%% @doc  take a valid (parsable) po file and check that there are no 
%%       inconsistencies between the msgid and msgstr texts e.g. do ?FTXT 
%%       format strings contain the same set of "~..." parameters ...
%%
%%       The function prints a result listing.
%% 
%%       PoFilePath     - the po file check
%%       IgnoreFilePath - a optional file that can be used to specify 
%%                        {CheckType, MsgId, MsgStr} combinations that should
%%                        not be reported as a errors/warnings - there are cases
%%                        especialy for warnings that may be valid transaltions
%%                        that just look suspect (i.e. are errors 90% of the 
%%                        time).
%%                        Each (language) po file should usualy have one such 
%%                        ignore file.
%%
%%       Ignore formats:
%%       * {no_translation, MsgId, MsgStr}
%%       * {bad_ftxt, MsgId, MsgStr}
%%       * {bad_stxt, MsgId, MsgStr}
%%       * {bad_ws, MsgId, MsgStr}
%%       * {bad_punctuation, MsgId, MsgStr}
%%       * {bad_html, MsgId, MsggStr}
%% @end------------------------------------------------------------------------
validate(PoFilePath) ->
    validate(PoFilePath, "").

validate(PoFilePath, IgnoreFilePath) ->
    Terms = 
	case IgnoreFilePath of
	    "" -> [];
	    _  -> case file:consult(IgnoreFilePath) of
		      {ok, Terms0}     -> Terms0;
		      {error, Reason0} -> 
			  throw({no_ignore_file_or_malformed_file, Reason0})
		  end
	end,
    Ignores = ets:new(dummy, [set,private,{keypos,1}]),

    %% catch exceptions so that ets table is always cleaned up
    try
	%% fill ets table with ignore data for fast lookup
	lists:foreach(fun(E) -> 
			      case is_valid_ignore_entry(E) of
				  true  -> ets:insert(Ignores, {E});
				  false -> throw({invalid_ignore_file_entry, E})
			      end
		      end, Terms),
	
	%% get file and discard non-text meta data header
	[{header_info, _} | Trans] = 
	try gettext:parse_po(PoFilePath)
	catch _:_ -> throw(po_file_not_found_or_parsing_failed)
	end,
	
	{BadEntries, [BadSTXT, BadFTXT, NoTrans, BadWS, BadPunct, BadHtml]} =
	run_checks(Ignores, Trans),
	format_results(PoFilePath, BadEntries, 
		       [BadSTXT, BadFTXT, NoTrans, BadWS, BadPunct, BadHtml])
    catch _:Reason2 -> 
	    throw({validator_crashed, Reason2})
    after
	%% all checks done - discard ignore table
	ets:delete(Ignores)
    end,
    ok.

%% is_list/1 = is_string/1 check
is_valid_ignore_entry(E) -> 
    case E of
	{no_translation, MsgId, MsgStr} 
	when is_list(MsgId), is_list(MsgStr) -> true;
	{bad_ftxt, MsgId, MsgStr}
	when is_list(MsgId), is_list(MsgStr) -> true;
	{bad_stxt, MsgId, MsgStr}
	when is_list(MsgId), is_list(MsgStr) -> true;
	{bad_ws, MsgId, MsgStr}
	when is_list(MsgId), is_list(MsgStr) -> true;
	{bad_punctuation, MsgId, MsgStr}
	when is_list(MsgId), is_list(MsgStr) -> true;
	{bad_html, MsgId, MsgStr}
	when is_list(MsgId), is_list(MsgStr) -> true;
	_ ->
	    false
    end.


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
run_checks(Ignores, Trans) ->
    %% Each check runs independetly of all others - no data is retained between
    %% them this makes it easy to add more tests without breaking stuff.
    BadSTXT = lists:foldl(fun(TxtPair, Acc) -> 
				  look_for_bad_stxt(TxtPair, Ignores, Acc)
			  end, [], Trans),

    BadFTXT = lists:foldl(fun(TxtPair, Acc) -> 
				  look_for_bad_ftxt(TxtPair, Ignores, Acc)
			  end, [], Trans),

    NoTrans = lists:foldl(fun(TxtPair, Acc) -> 
				  look_for_no_translation(TxtPair, Ignores,
							  Acc)
			  end, [], Trans),

    BadWS = lists:foldl(fun(TxtPair, Acc) -> 
				look_for_bad_ws(TxtPair, Ignores, Acc) 
			end, [], Trans),

    BadPunct = lists:foldl(fun(TxtPair, Acc) -> 
				   look_for_bad_punctuation(
				     TxtPair, Ignores, Acc)
			   end, [], Trans),

    BadHtml = lists:foldl(fun(TxtPair, Acc) -> 
				  look_for_bad_html(
				    TxtPair, Ignores, Acc)
			  end, [], Trans),

    
    BadEntries = lists:flatten([BadSTXT, BadFTXT, NoTrans, BadWS, BadPunct, 
				BadHtml]),
    {BadEntries, [BadSTXT, BadFTXT, NoTrans, BadWS, BadPunct, BadHtml]}.


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
format_results(PoFilePath, BadEntries, 
	       [BadSTXT, BadFTXT, NoTrans, BadWS, BadPunct, BadHtml]) ->
    io:format("===========================================\n"
	      " Checking: ~s\n"
	      " Total - ~s\n"
	      "===========================================\n", 
	      [PoFilePath, format_count(BadEntries)]),

    io:format("-------------------------------------------\n"
	      "Untranslated (~s) \n"
	      "-------------------------------------------\n"
	      "~p\n", [format_count(NoTrans), NoTrans]),
    
    io:format("-------------------------------------------\n"
	      "BAD ?STXT format strings (~s) \n"
	      "-------------------------------------------\n"
	      "~p\n", [format_count(BadSTXT), BadSTXT]),

    io:format("-------------------------------------------\n"
	      "BAD ?FTXT format strings (~s) \n"
	      "-------------------------------------------\n"
	      "~p\n", [format_count(BadFTXT), BadFTXT]),

    io:format("-------------------------------------------\n"
	      "Incorrect whitespace usage (~s) \n"
	      "-------------------------------------------\n"
	      "~p\n", [format_count(BadWS), BadWS]),

    io:format("-------------------------------------------\n"
	      "Inconsistent punctuation (~s) \n"
	      "-------------------------------------------\n"
	      "~p\n", [format_count(BadPunct), BadPunct]),

    io:format("-------------------------------------------\n"
	      "Changes in html (~s) \n"
	      "-------------------------------------------\n"
	      "~p\n", [format_count(BadHtml), BadHtml]),
    ok.

%% return: io_list()
format_count(BadEntries) ->
    {ErrorCount, WarningCount} = count(BadEntries),
    io_lib:format("errors: ~p warnings: ~p", [ErrorCount, WarningCount]).

%% return: {ErrorCount::integer(), WarningCount::integer()}
count(BadEntries) ->
    F = fun(E, {Error, Warning}) ->
		case element(1, E) of
		    'ERROR'   -> {Error + 1, Warning};
		    'Warning' -> {Error, Warning + 1}
		end
	end,
    lists:foldl(F, {0,0}, BadEntries).
		
			

%% ----------------------------------------------------------------------------
%% each callback call may add a {ErrorType, ErrorText, .....} tuple in the 
%% lists:foldl/3 iteration. 


look_for_bad_stxt({OriginalFormatStr, TranslatedFormatStr}, 
		  Ignores, Acc) -> 
    %% XXX $ in non-STXT format strings isn't handle this may result 
    %%     in crashes !!! 

    %% check strings only
    Vars1 = gettext_format:get_vars_in_format_str(OriginalFormatStr),
    Vars2 = gettext_format:get_vars_in_format_str(TranslatedFormatStr),
    
    InterSect = lists_ext:intersection(Vars1, Vars2),
    %% some $...$ are unused
    OkButSuspect = lists_ext:subtract(Vars1, Vars2),
    %% translation contains unkown $...$
    BadVars      = lists_ext:subtract(Vars2, Vars1),
    
    case {length(InterSect), length(OkButSuspect), length(BadVars)} of
	{_,Suspect,Bad} when (Bad > 0) and (Suspect > 0) ->
	    do_ignore(Ignores,
		      {bad_stxt, OriginalFormatStr, TranslatedFormatStr},
		      {'ERROR', 
		       "Translation contains unknown var(s) "
		       "as well as unused var(s) (not using var(s) from msgid "
		       "may be ok).",
		       {bad_vars,         BadVars},
		       {unused_vars,      OkButSuspect},
		       {original,         OriginalFormatStr}, 
		       {translation,      TranslatedFormatStr}, 
		       {original_vars,    Vars1}, 
		       {translation_vars, Vars2}},
		      Acc);
	{_,_,Bad} when Bad > 0 ->
	    do_ignore(Ignores,
		      {bad_stxt, OriginalFormatStr, TranslatedFormatStr},
		      {'ERROR',
		       "Translation contains unknown var(s).",
		       {bad_vars,         BadVars},
		       {original,         OriginalFormatStr}, 
		       {translation,      TranslatedFormatStr}, 
		       {original_vars,    Vars1}, 
		       {translation_vars, Vars2}},
		      Acc);
	{_,Suspect,_} when Suspect > 0 ->
	    do_ignore(Ignores,
		      {bad_stxt, OriginalFormatStr, TranslatedFormatStr},
		      {'Warning',
		       "Translation dosen't use some var(s) this is ok but may "
		       "indicate a error.",
		       {unused_vars,      OkButSuspect},
		       {original,         OriginalFormatStr}, 
		       {translation,      TranslatedFormatStr}, 
		       {original_vars,    Vars1}, 
		       {translation_vars, Vars2}},
		      Acc);
	{Ok,_,_} when Ok == length(Vars1)  ->
	    Acc
    end.


look_for_bad_ftxt({OriginalFormatStr, TranslatedFormatStr}, Ignores, Acc) ->
    %% check strings only
    Tags1 = get_format_tags(OriginalFormatStr),
    Tags2 = get_format_tags(TranslatedFormatStr),
    case Tags1 == Tags2 of
	true -> Acc;
	false -> 
	    do_ignore(Ignores,
		      {bad_ftxt, OriginalFormatStr, TranslatedFormatStr},
		      {'ERROR',
		       "Format string missmatch.",
		       {original,         OriginalFormatStr}, 
		       {translation,      TranslatedFormatStr}, 
		       {original_tags,    Tags1}, 
		       {translation_tags, Tags2}},
		      Acc)
    end.


look_for_no_translation({OriginalFormatStr, TranslatedFormatStr}, 
			Ignores, Acc) ->
    case OriginalFormatStr == TranslatedFormatStr of
	false -> Acc;
	true  -> 
	    do_ignore(Ignores, 
		      {no_translation, OriginalFormatStr, TranslatedFormatStr}, 
		      {'Warning',
		       "Text appears to be untranslated.",
		       {text, OriginalFormatStr}}, 
		      Acc)
    end.


look_for_bad_ws({OriginalFormatStr, TranslatedFormatStr}, Ignores, Acc) ->
    FrontMatch = ws_match(OriginalFormatStr, TranslatedFormatStr),
    ORev = lists:reverse(OriginalFormatStr),
    TRev = lists:reverse(TranslatedFormatStr),
    TailMatch  = ws_match(ORev, TRev),

    Acc2 = case TailMatch of
	       false -> 
		   do_ignore(Ignores, 
			     {bad_ws, OriginalFormatStr, TranslatedFormatStr},
			     {'ERROR',
			      "Whitespaces differ at string tail.",
			      {original,    OriginalFormatStr}, 
			      {translation, TranslatedFormatStr}}, 
			     Acc);
	       true -> Acc
    end,
    Acc3 = case FrontMatch of
	       false -> 
		   do_ignore(Ignores,
			     {bad_ws, OriginalFormatStr, TranslatedFormatStr},
			     {'ERROR',
			      "Whitespaces differ at string front.",
			      {original,    OriginalFormatStr}, 
			      {translation, TranslatedFormatStr}},
			     Acc2);
	       true -> Acc2
    end,
    Acc3.


%% ----------------------------------------------------------------------------
%% return: {L1, L2} | 
%%         ws_missmatch (when initial whitespace differ)
text_with_no_ws_front([],[]) -> {[],[]};
text_with_no_ws_front([], _) -> ws_missmatch; % different no. of whitespaces
text_with_no_ws_front(_ ,[]) -> ws_missmatch; % different no. of whitespaces
text_with_no_ws_front([C1|R1] = L1, [C2|R2] = L2) ->
    W1 = is_ws(C1), 
    W2 = is_ws(C2),
    case {C1, C2, W1, W2} of
	{C, C, true,  true}  -> text_with_no_ws_front(R1,R2);
	{_, _, true,  false} -> ws_missmatch; % different no. of whitespaces
	{_, _, false, true}  -> ws_missmatch; % different no. of whitespaces
	{_, _, false, false} -> {L1, L2}
    end.

%% do whitespaces match - bool()
ws_match(R1,R2) -> 
    case text_with_no_ws_front(R1,R2) of
	{_,_}        -> true;
	ws_missmatch -> false
    end.

%%  is whitespace (control char or space/nbsp/NL/CR/...)
is_ws(C) when (C =< 32) orelse ((C >= 127) andalso (C =< 160)) -> true;
is_ws(_) -> false.


%% ----------------------------------------------------------------------------
%% Compare trailing punctuation between orginal and translation
%% Note: these tests may need to be relaxed or map between different sets of
%%       punctuation if unicode is supported or when languages with unusal
%%       punctuation rules is used in the po file 
look_for_bad_punctuation({OriginalFormatStr, TranslatedFormatStr}, 
			 Ignores, Acc) ->
    case text_with_no_ws_front(lists:reverse(OriginalFormatStr), 
			       lists:reverse(TranslatedFormatStr)) of

	%% look_for_bad_ws(...) will warn about whitespace missmatch
	%% so assume that user will find any punctuation bugs on revalidation
	ws_missmatch  -> Acc; 

	%% trailing white spaces are the same
	{[], []} -> Acc;
	{[], R2} -> bad_punct(R2, OriginalFormatStr, TranslatedFormatStr, 
			      Ignores, Acc);
	{R1, []} -> bad_punct(R1, OriginalFormatStr, TranslatedFormatStr, 
			      Ignores, Acc);
	{R1, R2} ->
	    C1 = hd(R1),
	    C2 = hd(R2),
	    P1 = is_punct(C1),
	    P2 = is_punct(C2),
	    GoodPunct = case {P1, P2} of
			    {true,  true}  -> C1 == C2;
			    {true,  false} -> false;
			    {false, true}  -> false;
			    {false, false} -> true
			end,
	    case GoodPunct of
		true  -> Acc;
		false ->
		    do_ignore(Ignores,
			      {bad_punctuation, OriginalFormatStr, 
			       TranslatedFormatStr},
			      {'Warning',
			       "Trailing punctuation is missmatched.",
			       {original,    OriginalFormatStr}, 
			       {translation, TranslatedFormatStr}},
			      Acc)
	    end
    end.

%% helper function 
bad_punct(R, OriginalFormatStr, TranslatedFormatStr, Ignores, Acc) ->
    P = is_punct(hd(R)),
    case P of
	true -> 
	    do_ignore(Ignores,
		      {bad_punctuation, OriginalFormatStr, TranslatedFormatStr},
		      {'Warning',
		       "Trailing punctuation is missmatched.",
		       {original,    OriginalFormatStr}, 
		       {translation, TranslatedFormatStr}},
		      Acc);
	false ->
	     Acc
    end.


%% return: bool()
%% Determine if C is a end of scentence/text marker
%% Note: using unicode will require adding new punctuation markers. 
%%       The set of punctuation markers is somewhat arbitrary, its main intent
%%       is to catch plausible errors, at the end of texts that may be joined
%%       with other texts.
is_punct(C) ->
    lists:member(C, ".,;!?:" ++ [161, %% upside down !
				 191  %% upside down ?
				]).


%% ----------------------------------------------------------------------------
look_for_bad_html({OriginalFormatStr, TranslatedFormatStr}, 
		  Ignores, Acc) ->
    %% convert to ehtml
    Oehtml = gettext_yaws_html:h2e(OriginalFormatStr),
    Tehtml = gettext_yaws_html:h2e(TranslatedFormatStr),
    LocalAcc = look_for_bad_tags(Oehtml, Tehtml, [], 
				 {OriginalFormatStr, TranslatedFormatStr}),

    %% check if any errors/warings should be ignored
    F = fun(E, Acc2) ->
		do_ignore(Ignores,
			  {bad_html, OriginalFormatStr, TranslatedFormatStr},
			  E,
			  Acc2)
	end,
    lists:foldl(F, [], LocalAcc) ++ Acc.



%% Note: only the first error of each branch is reported.
%% Note: see yaws.pdf (at http://yaws.hyber.org/) for Ehtml specification

%% -------------
%% process Body or similar list of tag / non-tag elements
%% Note: is_tuple(...) => is_ehtml_tag(...)

%% all [EHTML] content processed
look_for_bad_tags([], [], Acc, _) -> Acc;

%% [EHTML] - E is a tag
look_for_bad_tags([E|_], [], Acc, Info) when is_tuple(E) -> 
    {OriginalFormatStr, TranslatedFormatStr} = Info,
    [{'Warning', 
      "Tag is missing from the translation (this may be a error)",
      {original,    OriginalFormatStr}, 
      {translation, TranslatedFormatStr},
      {tag, E}} | Acc];

%% [EHTML] - E isn't a tag - ensure that this hold true for R
look_for_bad_tags([E|R], [], Acc, Info) -> 
    Acc2 = look_for_bad_tags(E, [], Acc, Info),
    look_for_bad_tags(R, [], Acc2, Info);

%% [EHTML] - E is a tag
look_for_bad_tags([], [E|_], Acc, Info) when is_tuple(E) -> 
    {OriginalFormatStr, TranslatedFormatStr} = Info,
    [{'Warning', 
      "Tag has been added to translation (this may be ok)",
      {original,    OriginalFormatStr}, 
      {translation, TranslatedFormatStr},
      {tag, E}} | Acc];

%% [EHTML] - E isn't a tag - ensure that this hold true for R
look_for_bad_tags([], [E|R], Acc, Info) -> 
    Acc2 = look_for_bad_tags([], E, Acc, Info),
    look_for_bad_tags([], R, Acc2, Info);

%% [EHTML] - both trees are non-empty - check first entry for error
look_for_bad_tags(Ehtml1, Ehtml2, Acc, Info) 
  when is_list(Ehtml1), is_list(Ehtml2) -> 
    Acc2 = look_for_bad_tags(hd(Ehtml1), hd(Ehtml2), Acc, Info),
    look_for_bad_tags(tl(Ehtml1), tl(Ehtml2), Acc2, Info);


%% -------------
%% {Tag, Attrs, Body} - tag and attrs match
look_for_bad_tags({Tag, Attrs, Body1}, {Tag, Attrs, Body2}, Acc, Info) 
  when is_atom(Tag), is_list(Attrs) -> 
    look_for_bad_tags(Body1, Body2, Acc, Info);
%% {Tag, Attrs, Body} - tag or attrs differ
look_for_bad_tags({Tag1, Attrs1, _}, {Tag2, Attrs2, _}, Acc, Info) 
  when is_atom(Tag1), is_list(Attrs1), is_atom(Tag2), is_list(Attrs2) -> 
    {OriginalFormatStr, TranslatedFormatStr} = Info,
    [{'Warning', 
      "Tag or tag attributes differ, even though the html structure "
      "appears to be the same.",
      {original,    OriginalFormatStr}, 
      {translation, TranslatedFormatStr},
      {original_tag, Tag1},
      {translation_tag, Tag2},
      {original_attrs, Attrs1},
      {translation_attrs, Attrs2}} | Acc];

%% {Tag, Attrs} - tag and attrs match
look_for_bad_tags({Tag, Attrs}, {Tag, Attrs}, Acc, _)
  when is_atom(Tag), is_list(Attrs) -> Acc;
%% {Tag, Attrs} - tag or attrs differ
look_for_bad_tags({Tag1, Attrs1}, {Tag2, Attrs2}, Acc, Info)
  when is_atom(Tag1), is_list(Attrs1), is_atom(Tag2), is_list(Attrs2) ->
    {OriginalFormatStr, TranslatedFormatStr} = Info,
    [{'Warning', 
      "Tag or tag attributes differ, even though the html structure "
      "appears to be the same.",
      {original,    OriginalFormatStr}, 
      {translation, TranslatedFormatStr},
      {original_tag, Tag1},
      {translation_tag, Tag2},
      {original_attrs, Attrs1},
      {translation_attrs, Attrs2}} | Acc];

%% {Tag} - tags match
look_for_bad_tags({Tag}, {Tag}, Acc, _) when is_atom(Tag) -> Acc;
%% {Tag} - tags differ
look_for_bad_tags({Tag1}, {Tag2}, Acc, Info) 
  when is_atom(Tag1), is_atom(Tag2) ->
    {OriginalFormatStr, TranslatedFormatStr} = Info,
    [{'Warning', 
      "Tags differ, even though the html structure appears to be the same.",
      {original,    OriginalFormatStr}, 
      {translation, TranslatedFormatStr},
      {original_tag, Tag1},
      {translation_tag, Tag2}} | Acc];


%% -------------
%% binary() - text no ehtml check needed
look_for_bad_tags(Bin1, Bin2, Acc, _) 
  when is_binary(Bin1), is_binary(Bin2) -> Acc;
look_for_bad_tags(Bin1, [], Acc, _) 
  when is_binary(Bin1) -> Acc;
look_for_bad_tags([], Bin2, Acc, _) 
  when is_binary(Bin2) -> Acc;

%% char() - text no ehtml check needed
look_for_bad_tags(Char1, Char2, Acc, _) 
  when ?IS_CHAR(Char1), ?IS_CHAR(Char2) -> Acc;
look_for_bad_tags(Char1, [], Acc, _) 
  when ?IS_CHAR(Char1) -> Acc;
look_for_bad_tags([], Char2, Acc, _) 
  when ?IS_CHAR(Char2) -> Acc;


%% -------------
%% ehtml trees differ
look_for_bad_tags(E1, E2, Acc, Info) ->
    {OriginalFormatStr, TranslatedFormatStr} = Info,
    [{'Warning', 
      "Html tag nesting structure differs.",
      {original,    OriginalFormatStr}, 
      {translation, TranslatedFormatStr},
      {original_tree, E1},
      {translation_tree, E2}} | Acc].


%% ----------------------------------------------------------------------------
%% Naive io format "~..." detector - should mostly work as most FTXT entries 
%% only use ~s and occasionaly ~p  
get_format_tags(IoFormatStr) ->
    get_format_tags(IoFormatStr, []).

get_format_tags([], Tags) -> lists:reverse(Tags);
get_format_tags([_C], Tags) -> lists:reverse(Tags);

get_format_tags([$~, C| R], Tags) -> get_format_tags(R, [C|Tags]);
get_format_tags([_C|R], Tags) -> get_format_tags(R, Tags).


%% ----------------------------------------------------------------------------
%% return: bool()
%% return true if Key is in Tab
is_ignore(Tab, {_Type, _OrgText, _TransText} = Key) ->
    case ets:lookup(Tab, Key) of
	[{Key}] -> true;
	[] -> false
    end.
	     
%% update Acc with Bad only if Key not in Ignores
do_ignore(Ignores, Key, Bad, Acc) ->
    case is_ignore(Ignores, Key) of
	false -> [Bad | Acc];
	true  -> Acc
    end.
