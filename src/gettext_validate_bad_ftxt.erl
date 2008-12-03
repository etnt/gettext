%% @private
%% @author Håkan Stenholm <hokan@kreditor.se>
%%
%% @doc check that io:format(...) args ~s,~p, ... are the same in msgid and 
%%      msgstr
%% ----------------------------------------------------------------------------

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
