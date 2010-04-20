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
%% @doc Check for errors like unknown `$...$' parameters in msgstr and
%% suspect but valid cases like not using a parameter from msgid in msgstr.

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
    ThrowMsg = {formatter_crashed,
		OriginalFormatStr, TranslatedFormatStr},
    Vars1 = 
	try 
	    gettext_format:get_vars_in_format_str(OriginalFormatStr)
	catch _:_ ->
		throw(ThrowMsg)
	end,
    Vars2 = 
	try 
	    gettext_format:get_vars_in_format_str(TranslatedFormatStr)
	catch _:_ ->
		throw(ThrowMsg)
	end,
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

