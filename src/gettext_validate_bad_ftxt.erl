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
%% @doc check that `io:format(...) args ~s,~p, ...' are the same in msgid
%% and msgstr.

-module(gettext_validate_bad_ftxt).

-export([ignore_entry_type/0,
	 heading/0,
	 check/3
	]).

%% ----------------------------------------------------------------------------
ignore_entry_type() ->
    bad_ftxt.

heading() ->
    "BAD ?FTXT format strings".

%% ----------------------------------------------------------------------------

check({OriginalFormatStr, TranslatedFormatStr}, Ignores, Acc) ->
    %% check strings only
    Tags1 = get_format_tags(OriginalFormatStr),
    Tags2 = get_format_tags(TranslatedFormatStr),
    case Tags1 == Tags2 of
	true -> Acc;
	false -> 
	    gettext_validate:do_ignore(
	      Ignores,
	      {bad_ftxt, OriginalFormatStr, TranslatedFormatStr},
	      {'ERROR',
	       "Format string missmatch.",
	       {original,         OriginalFormatStr}, 
	       {translation,      TranslatedFormatStr}, 
	       {original_tags,    Tags1}, 
	       {translation_tags, Tags2}},
	      Acc)
    end.


%% ----------------------------------------------------------------------------
%% Naive io format "~..." detector - should mostly work as most FTXT entries 
%% only use ~s and occasionaly ~p  
get_format_tags(IoFormatStr) ->
    get_format_tags(IoFormatStr, []).

get_format_tags([], Tags) -> lists:reverse(Tags);
get_format_tags([_C], Tags) -> lists:reverse(Tags);

get_format_tags([$~, C| R], Tags) -> get_format_tags(R, [C|Tags]);
get_format_tags([_C|R], Tags) -> get_format_tags(R, Tags).
