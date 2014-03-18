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
%% @doc Support module for TXT functions.

-module(gettext_format).

%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export ([
	  stxt/2,
	  get_vars_in_format_str/1
	 ]).

%%=============================================================================
%% External functions
%%=============================================================================

%% ----------------------------------------------------------------------------
%% @spec stxt(FormatStr::string(),
%%            Args::[{Key::atom(),SubVal::string()}]) -> string()
%% @doc  This function is similar to io_lib:format/2 but allows FormatStr to use
%%       Args in any order, any number of times or not at all.
%%       This is needed for translators of po files to be able to write
%%       translations with a natural scentence structure.
%%
%%       FormatStr: can contain $arg_name$ markers where arg_name must be one
%%       of the Key entries in Args to substitute (when converted to a atom).
%%       Key can be any atom not containing $.
%%
%%       FormatStr is processed as shown below (premature FormatStr end states
%%       and "missing char" edges aren't shown):
%%
%%```          $            !$             $
%%     ->(0) --------> (2) --------> (3) -------> (4)
%%    | ^  \            \           ^  \           |
%%    | \__/             \ $        \__/ !$        |
%%    |     !$            v                        |
%%    |                   (1)                      |
%%    |___________________/_______________________/
%%
%%    0  : start state
%%    1  : quoted $ found
%%    2/3: (start) accumulating tag name
%%    4  : look up tag in Args
%%'''
%%      Note that Key/arg_name should be chosen to be helpfull to the translator
%%      who will usually only have acess to FormatStr shown as the MsgId in the
%%      po file.
%% @end------------------------------------------------------------------------
stxt(Str, Args) ->
    try stxt0(Str, Args)
    catch throw:Reason -> throw({reason, Reason,
				 input_str, Str,
				 input_args, Args})
    end.

stxt0([$$|R], Args) ->
    stxt2(R, Args);
stxt0([C|R], Args) ->
    [C | stxt0(R, Args)];
%% finished
stxt0([], _) ->
    [].


stxt2([$$|R], Args) ->
    [$$ | stxt0(R, Args)];
stxt2([C|R], Args) ->
    stxt3(R, Args, [C]);
%% unexpected end of FormatStr
stxt2([], _) ->
    throw("FormatStr ended with unfinished quote or substitution tag").

%% end of $....$ tag
stxt3([$$|R], Args, Acc) ->
    TagStr = lists:reverse(Acc),
    Tag = list_to_atom(TagStr),
    SubsStr = get_arg(Tag, Args),
    SubsStr ++ stxt0(R, Args);
stxt3([C|R], Args, Acc) ->
    stxt3(R, Args, [C|Acc]);
%% unexpected end of FormatStr
stxt3([], _, _) ->
    throw("$...$ tag is missing ending $").


%% get the substitute value
get_arg(Key, Args) ->
    {value, {_, Val}} = lists:keysearch(Key, 1, Args),
    case is_list(Val) of
	true ->
	    Val;
	false ->
	    io_lib:format("~p", [Val])
    end.


%% ----------------------------------------------------------------------------
%% Extract the $....$ texts from a ?STXT format string
%% return: [string()] - the names of the $...$ tags, sorted and unique entries
%%         only
%% Note: this code must implement the same state machine as stxt(...) to work
%%       properly
get_vars_in_format_str(FormatStr) ->
    try get_vars(FormatStr, [])
    catch throw:Reason -> throw({reason, Reason,
				 input_str, FormatStr})
    end.

get_vars([$$|R], Vars) ->
    get_vars2(R, Vars);
get_vars([_C|R], Vars) ->
    get_vars(R, Vars);
%% finished
get_vars([], Vars) ->
    lists:usort(Vars).


get_vars2([$$|R], Vars) ->
    get_vars(R, Vars);
get_vars2([C|R], Vars) ->
    get_vars3(R, Vars, [C]);
%% unexpected end of FormatStr
get_vars2([], _) ->
    throw("FormatStr ended with unfinished quote or substitution tag").

%% end of $....$ tag
get_vars3([$$|R], Vars, Acc) ->
    VarStr = lists:reverse(Acc),
    get_vars(R, [VarStr | Vars]);
get_vars3([C|R], Vars, Acc) ->
    get_vars3(R, Vars, [C|Acc]);
%% unexpected end of FormatStr
get_vars3([], _, _) ->
    throw("$...$ tag is missing ending $").
