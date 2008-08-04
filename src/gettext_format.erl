%% module for TXT functions
%%-----------------------------------------------------------------------------

-module(gettext_format).

%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export ([
	  stxt/2
	 ]).  

%%=============================================================================
%% External functions
%%=============================================================================

%% ----------------------------------------------------------------------------
%% @spec stxt(FormatStr::string(), 
%%            Args::[{Key::atom(),SubVal::string()}]) -> string()
%% @doc  this function is similar to ?FTXT but alows the FormatStr to use
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
%%             $            !$             $           
%%     ->(0) --------> (2) --------> (3) -------> (4)
%%    | ^  \            \           ^  \           |  
%%    | \__/             \ $        \__/ !$        |
%%    |     !$            v                        |
%%    |                   (1)                      |
%%    |___________________/_______________________/
%%
%%    0  : start state
%%    1  : quote $ found
%%    2/3: (start) accumulating tag name
%%    4  : look up tag in Args  
%%
%%      Note that Key/arg_name should be chosen to be helpfull to the translator
%%      who will usually only have acess to FormatStr shown as the MsgId in the
%%      po file.
%% @end------------------------------------------------------------------------
stxt([$$|R], Args) ->
    stxt2(R, Args);
stxt([C|R], Args) ->
    [C | stxt(R, Args)];
%% finished
stxt([], _) ->
    [].


stxt2([$$|R], Args) ->
    [$$ | stxt(R, Args)];
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
    SubsStr ++ stxt(R, Args);
stxt3([C|R], Args, Acc) ->
    stxt3(R, Args, [C|Acc]);
%% unexpected end of FormatStr
stxt3([], _, _) ->
    throw("$...$ tag is missing ending $").


%% get the substitute value
get_arg(Key, Args) ->
    {value, {_, Val}} = lists:keysearch(Key, 1, Args),
    Val.
