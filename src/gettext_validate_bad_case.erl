%% @private
%% @author Håkan Stenholm <hokan@kreditor.se>
%%
%% @doc Try to find texts that should/shouldn't start on upper case 
%%      depending on if the text is the start of a scentence or not - swedish
%%      text e.g. usualy only uses upper case at scentence start (and in names).
%% ----------------------------------------------------------------------------

-module(gettext_validate_bad_case).

-export([ignore_entry_type/0,
	 heading/0,
	 check/3
	]).

%% ----------------------------------------------------------------------------
ignore_entry_type() ->
    bad_case.

heading() ->
    "Inconsistent case at start".

%% ----------------------------------------------------------------------------
%% Note: this check assumes latin-1 text
%% Note: different languages have different rules for when to use upper case
%%       (in case supporting languages) also note that translations will not
%%       always start on the same word so they will sometime move a upper case
%%       word into a (original text) lower case postion
%%       

check({OriginalFormatStr, TranslatedFormatStr}, Ignores, Acc) ->
    %% only do case checks with non-empty strings
    case {OriginalFormatStr, TranslatedFormatStr} of
	{"", ""} -> Acc;
	{"",  _} -> Acc;
	{_ , ""} -> Acc;
	{_ ,  _} -> 
	    %% check if both strings start with the same case
	    case case_changed(has_case(hd(OriginalFormatStr)),
			      has_case(hd(TranslatedFormatStr))) of
		false -> Acc;
		true -> 
		    gettext_validate:do_ignore(
		      Ignores,
		      {bad_case, OriginalFormatStr, 
		       TranslatedFormatStr},
		      {'Warning',
		       "Text starts with different case.",
		       {original,    OriginalFormatStr}, 
		       {translation, TranslatedFormatStr}},
		      Acc)
	    end
    end.

%% return true if case changed i.e. went from upper <-> lower, changing
%% to/from case_less is not considerd to be a change. 
case_changed(Case1, Case2) ->
    case {Case1, Case2} of
	{upper, lower} -> true;
	{lower, upper} -> true;
	_ -> false
    end.
	

%% return: upper | lower | 
%%         case_less (one variant char e.g. numbers, punctuation ...)
%% note  : ß and y with double dots are technicaly a lower case only letters
%%         but can in this case checker context be considred to be case less
%%         as well
has_case(Char) ->
    [CU] = text:to_upper([Char]),
    [CL] = text:to_lower([Char]),
    case {CU == Char, CL == Char} of
	{true,  true}  -> case_less; 
	{true,  false} -> upper;
	{false, true}  -> lower
	%% {false, false}    should only happen if to_upper/to_lower is broken
    end.
