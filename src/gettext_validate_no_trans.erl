%% @private
%% @author Håkan Stenholm <hokan@kreditor.se>
%%
%% @doc Warn about texts that look untranslated (msgid = msgstr).
%% ----------------------------------------------------------------------------

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
