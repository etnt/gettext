%% @private
%% @author Håkan Stenholm <hokan@kreditor.se>
%%
%% @doc Check for error like unkown $...$ parameters in msgstr and suspect
%%      but valid cases like not using a parameter from msgid in msgstr
%% ----------------------------------------------------------------------------

-module(gettext_validate_bad_stxt).

-export([ignore_entry_type/0,
	 heading/0,
	 check/3
	]).

%% ----------------------------------------------------------------------------
ignore_entry_type() ->
    bad_stxt.

heading() ->
    "BAD ?STXT format strings".
    

%% ----------------------------------------------------------------------------
check({OriginalFormatStr, TranslatedFormatStr}, 
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
	    gettext_validate:do_ignore(
	      Ignores,
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
	    gettext_validate:do_ignore(
	      Ignores,
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
	    gettext_validate:do_ignore(
	      Ignores,
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

