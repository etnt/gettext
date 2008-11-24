%% @private
%% @author Håkan Stenholm <hokan@kreditor.se>
%% @doc Support module for TXT functions.

-module(gettext_format).

%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export ([
	  stxt/2,
	  check_translations/0
	 ]).  

%%=============================================================================
%% External functions
%%=============================================================================

%% ----------------------------------------------------------------------------
%% @spec stxt(FormatStr::string(), 
%%            Args::[{Key::atom(),SubVal::string()}]) -> string()
%% @doc  This function is similar to ?FTXT but alows the FormatStr to use
%%       Args in any order, any number of times or not at all.
%%       This is needed for translators of po files to be able to write
%%       translations with a natural scentence structure. 
%%
%%       FormatStr: can contain $arg_name$ markers where arg_name must be one 
%%       of the Key entries in Args to substitute (when converted to a atom). 
%%       Key can be any atom not containing $.
%%
%%       FormatStr is processed as shown below (premature FormatStr end states
%%       and "missing char" edges aren't shown):
%%
%%```          $            !$             $           
%%     ->(0) --------> (2) --------> (3) -------> (4)
%%    | ^  \            \           ^  \           |  
%%    | \__/             \ $        \__/ !$        |
%%    |     !$            v                        |
%%    |                   (1)                      |
%%    |___________________/_______________________/
%%
%%    0  : start state
%%    1  : quoted $ found
%%    2/3: (start) accumulating tag name
%%    4  : look up tag in Args  
%%'''
%%      Note that Key/arg_name should be chosen to be helpfull to the translator
%%      who will usually only have acess to FormatStr shown as the MsgId in the
%%      po file.
%% @end------------------------------------------------------------------------
stxt([$$|R], Args) ->
    stxt2(R, Args);
stxt([C|R], Args) ->
    [C | stxt(R, Args)];
%% finished
stxt([], _) ->
    [].


stxt2([$$|R], Args) ->
    [$$ | stxt(R, Args)];
stxt2([C|R], Args) ->
    stxt3(R, Args, [C]);
%% unexpected end of FormatStr
stxt2([], _) ->
    throw("FormatStr ended with unfinished quote or substitution tag").

%% end of $....$ tag
stxt3([$$|R], Args, Acc) ->
    TagStr = lists:reverse(Acc),
    Tag = list_to_atom(TagStr),
    SubsStr = get_arg(Tag, Args),
    SubsStr ++ stxt(R, Args);
stxt3([C|R], Args, Acc) ->
    stxt3(R, Args, [C|Acc]);
%% unexpected end of FormatStr
stxt3([], _, _) ->
    throw("$...$ tag is missing ending $").


%% get the substitute value
get_arg(Key, Args) ->
    {value, {_, Val}} = lists:keysearch(Key, 1, Args),
    Val.


%% ----------------------------------------------------------------------------
%% Check gettext translation DB for malformed format strings
%% 
%% Note: gettext must be running for this check to work.
%% Note: some valid strings may be caught by the current code e.g. if they
%%       contain $ with a non-STXT/2 usage pattern
%% Note: FTXT/2 argument detection is naive, but should work well as long as ~s,
%%       ~p, ... and similar plain format directives (whitout options) are used
%%       in FTXT 
check_translations() ->
    
    BadSTXT = 
	ets:foldl(
	  %%    ignore meta data entries like 'header_info'
	  fun({{Info, _Lang}, _}, Acc) when is_atom(Info) -> 
		  Acc;

	     %% XXX add match for specific OriginalFormatStr-TranslatedFormatStr
	     %%     here if they use $ in a non-STXT conext

	     %% check strings only
	     ({{OriginalFormatStr, Lang}, TranslatedFormatStr}, Acc) -> 
		  Vars1 = get_vars_in_format_str(OriginalFormatStr),
		  Vars2 = get_vars_in_format_str(TranslatedFormatStr),

		  InterSect = lists_ext:intersection(Vars1, Vars2),
		  %% some $...$ are unused
		  OkButSuspect = lists_ext:subtract(Vars1, Vars2),
		  %% translation contains unkown $...$
		  BadVars      = lists_ext:subtract(Vars2, Vars1),

		  case {length(InterSect), length(OkButSuspect),
			length(BadVars)} of
		      {_,Suspect,Bad} when (Bad > 0) and (Suspect > 0) ->
			  [{"ERROR - translation contains unknown var(s) "
			    "as well as unused var(s) (this may be ok)",
			    {lang,             Lang},
			    {bad_vars,         BadVars},
			    {unused_vars,      OkButSuspect},
			    {original,         OriginalFormatStr}, 
			    {translation,      TranslatedFormatStr}, 
			    {original_vars,    Vars1}, 
			    {translation_vars, Vars2}} | Acc];
		      {_,_,Bad} when Bad > 0 ->
			  [{"ERROR - translation contains unknown var(s)",
			    {lang,             Lang},
			    {bad_vars,         BadVars},
			    {original,         OriginalFormatStr}, 
			    {translation,      TranslatedFormatStr}, 
			    {original_vars,    Vars1}, 
			    {translation_vars, Vars2}} | Acc];
		      {_,Suspect,_} when Suspect > 0 ->
			  [{"Warning - translation dosen't use some var(s)"
			    "this is ok but may indicate a error",
			    {lang,             Lang},
			    {unused_vars,      OkButSuspect},
			    {original,         OriginalFormatStr}, 
			    {translation,      TranslatedFormatStr}, 
			    {original_vars,    Vars1}, 
			    {translation_vars, Vars2}} | Acc];
		      {Ok,_,_} when Ok == length(Vars1)  ->
			  Acc
		  end
	  end,
	  [], gettext_db_ets),

    %% ------------------
    BadFTXT = 
	ets:foldl(
	  %%    ignore meta data entries like 'header_info'
	  fun({{Info, _Lang}, _}, Acc) when is_atom(Info) -> 
		  Acc;

	     %% check strings only
	     ({{OriginalFormatStr, Lang}, TranslatedFormatStr}, Acc) -> 
		  Tags1 = get_format_tags(OriginalFormatStr),
		  Tags2 = get_format_tags(TranslatedFormatStr),
		  case Tags1 == Tags2 of
		      true -> Acc;
		      false -> [{"ERROR - format string missmatch",
				 {lang,             Lang},
				 {original,         OriginalFormatStr}, 
				 {translation,      TranslatedFormatStr}, 
				 {original_tags,    Tags1}, 
				 {translation_tags, Tags2}} | Acc]
		  end
	  end,
	  [], gettext_db_ets),
    
    io:format("--------------------------\n"
	      "BAD ?STXT format strings  \n"
	      "--------------------------\n"
	      "~p\n", [BadSTXT]),

    io:format("--------------------------\n"
	      "BAD ?FTXT format strings  \n"
	      "--------------------------\n"
	      "~p\n", [BadFTXT]),
    ok.
    

%% ----------------------------------------------------------------------------
%% Extract the $....$ texts from a ?STXT format string
%% return: [string()] - the names of the $...$ tags, sorted and unique entries
%%         only
%% Note: this code must implement the same state machine as stxt(...) to work
%%       properly
get_vars_in_format_str(FormatStr) ->
    get_vars(FormatStr, []).

get_vars([$$|R], Vars) ->
    get_vars2(R, Vars);
get_vars([_C|R], Vars) ->
    get_vars(R, Vars);
%% finished
get_vars([], Vars) ->
    lists:usort(Vars).


get_vars2([$$|R], Vars) ->
    get_vars(R, Vars);
get_vars2([C|R], Vars) ->
    get_vars3(R, Vars, [C]);
%% unexpected end of FormatStr
get_vars2([], _) ->
    throw("FormatStr ended with unfinished quote or substitution tag").

%% end of $....$ tag
get_vars3([$$|R], Vars, Acc) ->
    VarStr = lists:reverse(Acc),
    get_vars(R, [VarStr | Vars]);
get_vars3([C|R], Vars, Acc) ->
    get_vars3(R, Vars, [C|Acc]);
%% unexpected end of FormatStr
get_vars3([], _, _) ->
    throw("$...$ tag is missing ending $").


%% ----------------------------------------------------------------------------
%% Naive io format "~..." detector - should mostly work as most FTXT entries 
%% only use ~s and occasionaly ~p  
get_format_tags(IoFormatStr) ->
    get_format_tags(IoFormatStr, []).

get_format_tags([], Tags) -> lists:reverse(Tags);
get_format_tags([_C], Tags) -> lists:reverse(Tags);

get_format_tags([$~, C| R], Tags) -> get_format_tags(R, [C|Tags]);
get_format_tags([_C|R], Tags) -> get_format_tags(R, Tags).
    
    

