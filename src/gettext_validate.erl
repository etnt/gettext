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
%% @author Håkan Stenholm <hokan@kreditor.se>
%% @doc Use this module to validate po files returned by translators. This
%% validator assumes the po file to be valid (parsable without errors by
%% gettext/poEdit/GNU po file tools). Its use is to check for errors
%% introduced by translators in the msgstr part in the po file; this is done
%% by supplying checker callback module names to the validate(...) function.
%%
%% A number of default checkers that should work on most texts can check
%% for:
%% <ul>
%% <li>incorrect FTXT argument usage</li>
%% <li>incorrect STXT argument usage</li>
%% <li>incorrect whitespace at front/tail of text</li>
%% <li>missing translations</li>
%% <li>incorrect punctuation at the end of texts</li>
%% <li>changes in HTML structure, tag names and attributes used</li>
%% <li>inconsistent usage of case at begining of text</li>
%% </ul>
%%
%% A checker must implement
%% ```ignore_entry_type() -> atom().'''
%% Returns the typename to use in the ignore file.
%%
%% ```heading() -> string().'''
%% Returns a short text used as heading when listing errors/warnings for
%% this checker.
%%
%% ```check({OriginalFormatStr::string(), TranslatedFormatStr::string()}, 
%%          Ignores, Acc::[acc()]) -> UpdatedAcc::[acc()]'''
%% Returns an updated accumulator `UpdatedAcc' after checking
%% `OriginalFormatStr' against `TranslatedFormatStr'. One or more entries
%% must be added to the head of `Acc' if problems are found. `Ignores'
%% contains an ETS table reference used by `gettext_validate:do_ignore(...)'
%% to check if the error/warning should be reported - this filtering should
%% be done after the check, so that the code crashes if the checker is
%% broken - ignore files could otherwise hide broken checks.
%%
%% Each acc() must be of the format:
%%```{alert_level(), 
%%    ErrorMsg::string(), 
%%    Pairs::{Name::atom(), Info::term()} ....}'''
%% where `alert_level()' is either `` 'ERROR' '', indicating an error that
%% may cause a crash (e.g., in STXT/FTXT) or generate invalid output, or ``
%% 'Warning' '', indicating a possible problem. `Pairs' contain check
%% specific error information - usually at least the msgid and msgstr, so
%% that they can be found in a po file and fixed.

-module(gettext_validate).


%% @type output() = [{{heading,          string()},
%%                    {ignore_type,      atom()}, 
%%		      {po_file_path,     string()},
%%		      {ignore_file_path, string()},
%% 		      CheckRes::[acc()]
%%                  }]


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export ([
	  %% the validator
	  validate/3,

	  %% use standard default checkers that should work on most texts
	  validate_q/1,
	  validate_q/2,
	  validate_qf/2,
	  validate_qf/3,

	  %% help functions for callback modules
	  do_ignore/4,

	  %% this may be useful when defining your own opts, e.g. when adding
	  %% new checks
	  default_opts/0
	 ]).  


%%=============================================================================
%% External functions
%%=============================================================================

%% ---------------------
%% validate "quick" - with default checkers

%% no ignores or opts
validate_q(PoFilePath) ->
    validate(PoFilePath, "", default_opts()).

%% no opts 
validate_q(PoFilePath, IgnoreFilePath) ->
    validate(PoFilePath, IgnoreFilePath, default_opts()).

%% ---------------------
%% validate "quick" to file - with default checkers

%% no ignores or opts
validate_qf(PoFilePath, SaveFile) ->
    validate(PoFilePath, "", default_opts() ++ [{to_file, SaveFile}]).

%% no opts 
validate_qf(PoFilePath, IgnoreFilePath, SaveFile) ->
    validate(PoFilePath, IgnoreFilePath, 
	     default_opts() ++ [{to_file, SaveFile}]).


%% ----------------------------------------------------------------------------
%% @spec validate(PoFilePath::string(), IgnoreFilePath::[]|string(),
%%                Opts::[{Key, Val}]) -> ok | output()
%% @doc  Takes a valid (parsable) po file and checks that there are no 
%%       inconsistencies between the msgid and msgstr texts. E.g.,
%%       checks that ?FTXT format strings contain the same set of "~..."
%%       parameters, etc.
%% 
%%       The function prints a result listing.
%% 
%%       `PoFilePath'     - the po file check
%%
%%       `IgnoreFilePath' - a optional file that can be used to specify 
%%                          {CheckType, MsgId, MsgStr} combinations that should
%%                          not be reported as a errors/warnings - false 
%%                          positives.
%%                          Each (language) po file should usualy have one such 
%%                          ignore file.
%%
%%       `Opts:'
%%``` {to_file, SaveLocation} - force output to file rather than shell
%%    {checkers, ModuleNames} - checkers to use, ModuleNames = [atom()]
%%    {end_user, Format}      - output format:
%%             human    - print readable text (default) to shell, file, ...
%%             computer - fuction returns output() to describe
%%                        the check results - this is useful for automated
%%                        po file validation
%%'''
%% @end------------------------------------------------------------------------
validate(PoFilePath, IgnoreFilePath, Opts) ->
    Terms = 
	case IgnoreFilePath of
	    "" -> [];
	    _  -> case file:consult(IgnoreFilePath) of
		      {ok, Terms0}     -> Terms0;
		      {error, Reason0} -> 
			  throw({no_ignore_file_or_malformed_file, Reason0})
		  end
	end,
    Ignores = ets:new(dummy, [set,private,{keypos,1}]),

    %% catch exceptions so that ets table is always cleaned up
    Res = 
	try
	    %% fill ets table with ignore data for fast lookup
	    lists:foreach(
	      fun(E) -> 
		      case is_valid_ignore_entry(E) of
			  true  -> ets:insert(Ignores, {E});
			  false -> throw({invalid_ignore_file_entry, E})
		      end
	      end, Terms),

	    %% get file and discard non-text meta data header
	    [{header_info, _} | Trans] = 
	    try gettext:parse_po(PoFilePath)
	    catch _:_ -> throw(po_file_not_found_or_parsing_failed)
	    end,

	    %% assert that there are no msgid duplicates
	    throw_on_duplicates(Trans),
	    
	    Callbacks = look_up(checkers, Opts),
	    CheckResults = run_checks(Ignores, Trans, Callbacks),
	    EndUser = case look_up(end_user, Opts) of
			  [] -> human;
			  EndUser0 -> EndUser0
		      end,
	    case EndUser of
		human ->
		    ResultText = format_results(PoFilePath, CheckResults, 
						Callbacks),
		    case look_up(to_file, Opts) of
			[] ->
			    io:format("~s~n", [ResultText]);
			SaveLocation ->
			    write_to_file(ResultText, SaveLocation)
		    end,
		    ok;
		computer ->
		    [{{heading, Checker:heading()},
		      {ignore_type, Checker:ignore_entry_type()}, 
		      {po_file_path, PoFilePath},
		      {ignore_file_path, IgnoreFilePath},
		      CheckRes} 
		     || {CheckRes, Checker} 
			    <- lists:zip(CheckResults, Callbacks)]
	    end
	
	catch _:Reason2 -> 
		throw({validator_crashed, Reason2})
	after
	    %% all checks done - discard ignore table
	    ets:delete(Ignores)
	end,
    Res.

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
throw_on_duplicates(MsgidMsgStrL) ->
    F = fun({MsgId1, _}, {MsgId2, _}) -> MsgId1 < MsgId2 end,
    SortedPairs = lists:sort(F, MsgidMsgStrL),
    case scan_for_duplicate(SortedPairs) of
	[] -> MsgidMsgStrL;
	Dups -> throw({po_file_contained_duplicates, Dups})
    end.
	     

scan_for_duplicate(SortedPairs) ->
    DupL = scan_for_duplicate(SortedPairs, []),
    lists:usort(DupL). %% only keep one copy of each duplicate

%% store all but last duplicate in Acc
scan_for_duplicate([{Id,_}=E, {Id,_} | R], Acc) ->
    scan_for_duplicate([E | R], [Id | Acc]);
scan_for_duplicate([_ | R], Acc) ->
    scan_for_duplicate(R, Acc);
scan_for_duplicate([], Acc) ->
    Acc.


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
%% Check that ignore file looks usable - we don't check the ignore type to 
%% allow for ignore files used with other sets of checker callback modules 
%% is_list/1 = is_string/1 check
%% get IgnoreTypes from ignore_types/1
is_valid_ignore_entry(E) ->
    case E of
	{Type, MsgId, MsgStr} when is_atom(Type), 
				   is_list(MsgId), is_list(MsgStr) ->
	    true;
	_ ->
	    false
    end.

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
write_to_file(ResultText, SaveLocation) ->
    ok = file:write_file(SaveLocation, ResultText).

%% return: Val | not_found
look_up(Key, Opts) -> 
    case lists:keysearch(Key, 1, Opts) of
	{value, {Key, Val}} -> Val;
	false -> []
    end.

default_opts() ->
    [{checkers, [gettext_validate_bad_stxt,
		 gettext_validate_no_trans,
		 gettext_validate_bad_case,
		 gettext_validate_bad_punct,
		 gettext_validate_bad_ftxt,  
		 gettext_validate_bad_html,
		 gettext_validate_bad_ws]}
    ].


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
run_checks(Ignores, Trans, Callbacks) ->
    %% Each check runs independetly of all others - no data is retained between
    %% them this makes it easy to add more tests without breaking stuff.

    %% get one entry (of [bad()]) for each check
    [lists:foldl(fun(TxtPair, Acc) -> 
			 Callback:check(TxtPair, Ignores, Acc)
		 end, [], Trans) || Callback <- Callbacks].


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% return: io_list() 
format_results(PoFilePath, BadL, Callbacks) 
  when length(BadL) == length(Callbacks) ->
    BadEntries = lists:flatten(BadL),
    
    [
     io_lib:format("=======================================================\n"
		   " Checking: ~s\n"
		   " Total - ~s\n"
		   "=======================================================\n", 
		   [PoFilePath, format_count(BadEntries)])
    ] ++ 

	[
	 io_lib:format(
	   "-------------------------------------------------------\n"
	   "~s (~s) \n"
	   "ignore type: ~p (use this in the ignore file)\n"
	   "-------------------------------------------------------\n"
	   "~p\n", [Callback:heading(), format_count(Bad), 
		    Callback:ignore_entry_type(),
		    Bad]) ||
	    {Callback, Bad} <- lists:zip(Callbacks, BadL)].


%% return: io_list()
format_count(BadEntries) ->
    {ErrorCount, WarningCount} = count(BadEntries),
    io_lib:format("errors: ~p warnings: ~p", [ErrorCount, WarningCount]).

%% return: {ErrorCount::integer(), WarningCount::integer()}
count(BadEntries) ->
    F = fun(E, {Error, Warning}) ->
		case element(1, E) of
		    'ERROR'   -> {Error + 1, Warning};
		    'Warning' -> {Error, Warning + 1}
		end
	end,
    lists:foldl(F, {0,0}, BadEntries).
		

%% ----------------------------------------------------------------------------
%% return: bool()
%% return true if Key is in Tab
is_ignore(Tab, {_Type, _OrgText, _TransText} = Key) ->
    case ets:lookup(Tab, Key) of
	[{Key}] -> true;
	[] -> false
    end.
	     
%% update Acc with Bad only if Key not in Ignores
%%
%% Ignores - the ets table to check for Key
%% Key - {CheckType, MsgId, MsgStr} a ignore file entry
%% Bad - a validate(...) error/warning
%% Acc - a validate(...) error/warning accumulator
do_ignore(Ignores, Key, Bad, Acc) ->
    case is_ignore(Ignores, Key) of
	false -> [Bad | Acc];
	true  -> Acc
    end.
