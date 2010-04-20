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
%% @private
%% @author Håkan Stenholm <hokan@kreditor.se>
%% @doc Try to find texts that should/shouldn't start on upper case
%% depending on if the text is the start of a sentence or not - e.g.,
%% Swedish text usually only uses upper case at sentence start (and in
%% names).

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
