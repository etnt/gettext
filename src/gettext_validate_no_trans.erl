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
%% @doc Warn about texts that look untranslated (msgid = msgstr).

-module(gettext_validate_no_trans).

-export([ignore_entry_type/0,
	 heading/0,
	 check/3
	]).

%% ----------------------------------------------------------------------------
ignore_entry_type() ->
    no_translation.

heading() ->
    "Untranslated".

%% ----------------------------------------------------------------------------

check({OriginalFormatStr, TranslatedFormatStr}, Ignores, Acc) ->
    case OriginalFormatStr == TranslatedFormatStr of
	false -> Acc;
	true  -> 
	    gettext_validate:do_ignore(
	      Ignores, 
	      {no_translation, OriginalFormatStr, TranslatedFormatStr}, 
	      {'Warning',
	       "Text appears to be untranslated. Add it to .ignore "
	       "file if valid trans. but msgid = msgstr",
	       {text, OriginalFormatStr}}, 
	      Acc)
    end.
