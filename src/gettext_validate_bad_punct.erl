%% -*- coding: latin-1 -*-
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
%% @doc Compare trailing punctuation between msgid and msgstr.

-module(gettext_validate_bad_punct).

-export([ignore_entry_type/0,
	 heading/0,
	 check/3
	]).

%% ----------------------------------------------------------------------------
ignore_entry_type() ->
    bad_punctuation.

heading() ->
    "Inconsistent punctuation".

%% ----------------------------------------------------------------------------
%% Note: these tests may need to be relaxed or map between different sets of
%%       punctuation if unicode is supported or when languages with unusal
%%       punctuation rules are used in the po file
check({OriginalFormatStr, TranslatedFormatStr}, Ignores, Acc) ->
    case gettext_validate_bad_ws:text_with_no_ws_front(
	   lists:reverse(OriginalFormatStr),
	   lists:reverse(TranslatedFormatStr)) of

	%% look_for_bad_ws(...) will warn about whitespace mismatch
	%% so assume that user will find any punctuation bugs on revalidation
	ws_mismatch  -> Acc;

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
		    gettext_validate:do_ignore(
		      Ignores,
		      {bad_punctuation, OriginalFormatStr,
		       TranslatedFormatStr},
		      {'Warning',
		       "Trailing punctuation is mismatched.",
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
	    gettext_validate:do_ignore(
	      Ignores,
	      {bad_punctuation, OriginalFormatStr, TranslatedFormatStr},
	      {'Warning',
	       "Trailing punctuation is mismatched.",
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
