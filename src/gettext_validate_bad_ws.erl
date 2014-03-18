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
%% @doc Check for differences in trailing and initial whitespaces between
%% msgid and msgstr. This is important when several translated texts are
%% appended. Changes in whitespaces may also yield broken files when
%% generating things like CSV (comma separated values) files.

-module(gettext_validate_bad_ws).

-export([ignore_entry_type/0,
	 heading/0,
	 check/3,

	 %% used by gettext_validate_bad_punct.erl
	 text_with_no_ws_front/2
	]).

%% ----------------------------------------------------------------------------
ignore_entry_type() ->
    bad_ws.

heading() ->
    "Incorrect whitespace usage".

%% ----------------------------------------------------------------------------
check({OriginalFormatStr, TranslatedFormatStr}, Ignores, Acc) ->
    FrontMatch = ws_match(OriginalFormatStr, TranslatedFormatStr),
    ORev = lists:reverse(OriginalFormatStr),
    TRev = lists:reverse(TranslatedFormatStr),
    TailMatch  = ws_match(ORev, TRev),

    Acc2 = case TailMatch of
	       false ->
		   gettext_validate:do_ignore(
		     Ignores,
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
		   gettext_validate:do_ignore(
		     Ignores,
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
%%         ws_mismatch (when initial whitespace differ)
text_with_no_ws_front([],[]) -> {[],[]};
text_with_no_ws_front([], _) -> ws_mismatch; % different no. of whitespaces
text_with_no_ws_front(_ ,[]) -> ws_mismatch; % different no. of whitespaces
text_with_no_ws_front([C1|R1] = L1, [C2|R2] = L2) ->
    W1 = is_ws(C1),
    W2 = is_ws(C2),
    case {C1, C2, W1, W2} of
	{C, C, true,  true}  -> text_with_no_ws_front(R1,R2);
	{_, _, true,  false} -> ws_mismatch; % different no. of whitespaces
	{_, _, false, true}  -> ws_mismatch; % different no. of whitespaces
	{_, _, false, false} -> {L1, L2}
    end.

%% do whitespaces match - bool()
ws_match(R1,R2) ->
    case text_with_no_ws_front(R1,R2) of
	{_,_}        -> true;
	ws_mismatch -> false
    end.

%%  is whitespace (control char or space/nbsp/NL/CR/...)
is_ws(C) when (C =< 32) orelse ((C >= 127) andalso (C =< 160)) -> true;
is_ws(_) -> false.
