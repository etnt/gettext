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
%% Created: 19 Jul 2007 by <kubanowak@gmail.com>
%% @author Jakub Nowak <kubanowak@gmail.com>
%% @doc Check and validate if any translated strings are missing.

-module(gettext_checker). 

-export([dialog/0,
	 dialog/1,
         help/0,
         get_first_key/0,
         next_non_translated/2,
         report_bug/3,
         check_bugs/0,
         display_errors/0
        ]).

-include("gettext_internal.hrl").


-define(TABLE_NAME, gettext_server_db).
-define(ROOT_DIR, filename:join([gettext_server:gettext_dir(), ?LANG_DIR])).
-define(ENDCOL, 72).
-define(PIVOT, 4).
-define(SEP, $\s).
-define(BUG_FILE, "bugs.dat").
-define(PROMPT, '> ').

help() ->
    io:format(
      "Module for finding and reparing dependency errors and warnings in~n"
      "the gettext_server_db.~n"
      "It recognizes the following situations:~n"
      "  * no key found in one of the languages~n"
      "  * key has the same value in both languages~n"
      "  * there is a space mismatch~n"
      "Use the dialog/0 or dialog/1 functions, e.g. "
      "gettext_checker:dialog(\"en\"),~nto start the checker~n~n"
      "The error finder is based on the two functions:~n"
      "  * next_non_translated(Key, Language) - to get the next error "
      "or warning~n"
      "    Returns [NextKey,Args,Status], where:~n"
      "      Status is one of the atoms:~n"
      "       - no_key~n" 
      "       - no_key_default~n"
      "       - same_value~n"
      "       - spaces_warning~n"
      "       - the_end~n"
      "      NextKey is a key from gettext_server_db in which the error "
      "occurred~n"
      "      (without the language information).~n"
      "      Args are additional arguments. Usually the empty list - "
      "only if~n"
      "      there is a space error it will contain:~n"
      "       - 'a', char_at_beginning, char_at_the_end~n"
      "       - 'b', char_at_beginning~n"
      "       - 'e', char_at_end~n"	      
      "  * get_first_key() - this function is used in order to"
      " return the first key~n"
      "    from the dets table.~n",
      []).


%% To be called from e.g a Makefile
display_errors() ->
    Lang = gettext:all_lcs(),
    F = fun(LC) ->
                FirstKey = get_first_key(),
                non_translated_stats(FirstKey, LC, 0, 0, 0)
        end,
    lists:foreach(F, Lang).

	      
	      
%% function implementing dialog with the user by giving a user possibility 
%% to pick up numbers with actions associated to them 
 
dialog() ->
    LC = gettext:default_lang(),
    io:format("Language set to: ~s~n", [gettext_iso639:lc2lang(LC)]),
    dialog(LC).

dialog(QueryLang) ->
  
    case check_lang(QueryLang) of 
	error ->
	    io:format("Wrong language! Specify a good one or exit(x)~n", []),
	    io:format("Possible languages: ~p~n", [all_langs()]),
		   
	    case get_string(?PROMPT) of 
		"x" -> 
		    true;
		NewLang ->
		    dialog(NewLang)
	    end;
	_Other ->
	    io:format("~n"
		      "1. check and change language~n"
		      "2. count errors and warnings~n"
		      "3. choose language~n"
		      "4. change space-errors automatically~n"
		      "5. check reported bugs~n"
		      "6. exit~n",
		      []),
	    case get_int(?PROMPT) of 
		{error, _Value} ->
		    io:format("wrong option ~n"),
		    dialog(QueryLang);
		1 ->
		    FirstKey = get_first_key(),		 
		    find_non_translated(FirstKey, QueryLang),
		    dialog(QueryLang);		
		2 ->
		    FirstKey = get_first_key(),
		    non_translated_stats(FirstKey, QueryLang,0,0,0),
		    dialog(QueryLang);
		3 ->
		    io:format("Possible languages: ~p~n", [all_langs()]),
		    NewLang = get_string(?PROMPT),
		    dialog(NewLang);
		4 ->
		    FirstKey = get_first_key(),
		    change_spaces(FirstKey, QueryLang),
		    dialog(QueryLang);
		5 ->
		    check_bugs(),	       
		    dialog(QueryLang),
		    io:format("check finished~n", []);
		6->
  		    true;
		_Num ->
		    io:format("wrong option ~n"),
		    dialog(QueryLang)
	    end
    end.

%%this function checks if the language we choose exists in the system, 
%%uses gettext_iso639 module
check_lang("a") ->
    ok;

check_lang(Lang) ->
    case gettext_iso639:lc2lang(Lang) of 
	"" ->
	    error;
	_Other ->
	    Fname = filename:join([?ROOT_DIR, ?CUSTOM_DIR, Lang, ?POFILE]),
	    case filelib:is_regular(Fname) of
		true  -> ok;
		false -> error
	    end	    
    end.

%% function thats is responsible for automatic space changing for all 
%% of the keys
change_spaces('$end_of_table',QueryLang) ->
    dets:sync(?TABLE_NAME), 
    save_to_po_file(QueryLang,custom),
    io:format("spaces checking is finished~n",[]);
%%
change_spaces(El,QueryLang) ->
     case next_non_translated(El,QueryLang) of 
	[NextKey,Key,Args,spaces_warning] ->
	     case Args of 
		 error ->
		     true;
		 _Other ->
		     NewValue = space_auto_change(Args,Key,QueryLang),
		     NewObj = {{Key,QueryLang},NewValue},
		     dets:insert(?TABLE_NAME,NewObj)		     
	     end,
	    change_spaces(NextKey,QueryLang);
	[NextKey,_,_,_Result ]->	    
	    change_spaces(NextKey,QueryLang)
    end.

%% function returning statistics about the number of errors and warnings    
%% (simple counting)
non_translated_stats('$end_of_table', QueryLang, E, W, S) ->
    io:format("~n------~n"
              "Results (~p):~n"
	      "Num.of key errors: ~p ~n"
	      "Same key warnings: ~p ~n"
	      "Spaces warnings: ~p~n",
	      [QueryLang, E, W, S]);
%%
non_translated_stats(El,QueryLang,E,W,S) ->
    case next_non_translated(El,QueryLang) of 
	[NextKey,_Key,_,no_key]->
	    non_translated_stats(NextKey,QueryLang,E+1,W,S);
	[NextKey,_Key,_,same_value] ->
	    non_translated_stats(NextKey,QueryLang,E,W+1,S);
	[NextKey,_Key,_,spaces_warning] ->
	    non_translated_stats(NextKey,QueryLang,E,W,S+1);
	[NextKey,_Key,_,no_key_default] ->
	    non_translated_stats(NextKey,QueryLang,E+1,W,S);
	[NextKey,_,_,_Result ]->	    
	    non_translated_stats(NextKey,QueryLang,E,W,S)
    end.

%% function responsible for getting all errors and warning and running
%% appropriate action for them
find_non_translated('$end_of_table',QueryLang) ->
    io:format("checking finished:)~n",[]),
    dets:sync(?TABLE_NAME), 
    save_to_po_file(QueryLang,custom),
    save_to_po_file(?DEFAULT_LANG,default);
%%
find_non_translated(El,QueryLang) ->
    case next_non_translated(El,QueryLang) of 
	[NextKey,Key,[],no_key]->	    
	    io:format("error: there is non translated string with key ~p "
		      "in ~p lang:~n",
		      [show_string(Key),QueryLang]),
	    Res = error_action(Key,QueryLang);
	[NextKey,Key,[],same_value] ->
	    io:format("warning: key ~p in lang ~p have the"
		      " same value:~n",[show_string(Key),QueryLang]),
	    Res = warning_action(Key,QueryLang);		
	[NextKey,Key,Args,spaces_warning] ->
	    io:format("warning: key ~p in lang ~p have space "
		      "mismatch~n",[show_string(Key),QueryLang]),
	    Res = spaces_action(Key,QueryLang, Args);
	[NextKey,Key,[],no_key_default] ->
	    io:format("error: there is non translated string with "
		      "key ~p in deafault lang:~n",
		      [show_string(Key)]),
	    Res = error_action_default(Key,QueryLang);
	[NextKey,_,_,the_end ]->	    
	    Res = ok
    end,
    if 
	Res == ret ->
	    find_non_translated('$end_of_table',QueryLang);
	true->
	    find_non_translated(NextKey,QueryLang)
    end.

%% function returning the first key in the whole table
%%
get_first_key() ->
    Key = dets:first(?TABLE_NAME),
    Key.

%% functionality for getting the new error/warning information from
%% gettext_server_db it returns a List - [NextKey,Key,Args,Status] - run
%% gettext_checker:help() for more informations
next_non_translated('$end_of_table',_QueryLang) ->
    ['$end_of_table',[],[],the_end];
%%
next_non_translated({Key, ?DEFAULT_LANG = Lang}, QueryLang) when is_atom(Key) ->
    NextKey = dets:next(?TABLE_NAME,{Key,Lang}),
    next_non_translated(NextKey, QueryLang);
%%
next_non_translated({Key, ?DEFAULT_LANG = Lang}, QueryLang) ->
    NextKey = dets:next(?TABLE_NAME, {Key, Lang}),
    Value = lookup(Key, Lang),	    
    case lookup(Key, QueryLang) of
        empty ->
            [NextKey, Key, [], no_key];
        Value ->
            [NextKey, Key, [], same_value];
        RetValue ->
            do_check_spaces(Key, NextKey, QueryLang, 
                            RetValue, Value) 				
    end;
%%
next_non_translated({Key, QueryLang}, QueryLang) ->
    NextKey = dets:next(?TABLE_NAME, {Key, QueryLang}),
    case lookup(Key, ?DEFAULT_LANG) of
        empty ->
            [NextKey, Key, [], no_key_default];
        _RetValue ->
            next_non_translated(NextKey, QueryLang)
    end;
%%
next_non_translated({Key, Lang}, QueryLang) ->
    NextKey = dets:next(?TABLE_NAME,{Key,Lang}),
    next_non_translated(NextKey, QueryLang).


do_check_spaces(Key, NextKey, QueryLang, RetValue, Value) ->
    case check_spaces(Value,RetValue) of  			     
        ok ->
            next_non_translated(NextKey,QueryLang);
        Args->%Args have info about where spaces mismatch
            [NextKey,Key,Args,
             spaces_warning]
    end.

%% Simple function that wraps the dets:lookup function
%%
lookup(Key,Lang) ->
    case dets:lookup(?TABLE_NAME, {Key, Lang}) of 
	[]          -> empty;  
	[{_,Str}|_] -> Str
    end.
%% function showing only first 40 chars of the string
%% 
show_string(String) ->
    Len = string:len(String),
    if 
	Len>40 ->
	    string:left(String,40)++"...";
	true ->
	    String
    end.

%% function that gets int from the user
%%
get_int(Prompt)->
    [_NewLine | TmpValue] =  lists:reverse(io:get_line(Prompt)),
    Value = lists:reverse(TmpValue),
    case string:to_integer(Value) of
	{error,_Reason}->
	    {error,Value};
	{Num,_Rest}->
	    Num
    end.

%% function that gets string from the user
get_string(Prompt) ->
    [_NewLine | TmpValue] =  lists:reverse(io:get_line(Prompt)),
    Value = lists:reverse(TmpValue),
    Value.
  
%% function responsible for finding spacing errors
%% Space error is only check in the beginning and the end of 
%% the string. it check if arguments are proper and if are
%% runs the check_spaces_real function  
check_spaces(S1,S2) when is_atom(S1) or is_atom(S2) ->
    error;
check_spaces(S1,S2)  ->
    Len1 = string:len(S1),
    Len2 = string:len(S2),
    if 
	(Len1<1) or (Len2<1) ->
	    error;	
	(S1==" ") or (S2==" ") ->
	    error;
	true ->
	    check_spaces_real(S1,S2)
    end.

%% B-begining
%% E-end
%% S-string  
check_spaces_real(S1,S2) ->
    B1 = hd(S1),
    B2 = hd(S2),

    [E1 | _Rest1] = lists:reverse(S1),
    [E2 | _Rest2] = lists:reverse(S2),
    if 
	(B1==$\s) or (B2==$\s) , (E1==$\s) or (E2==$\s) ->
	    if 
		not (B1==B2), not (E1==E2) ->
		    [a,B1,E1];
		    not (B1==B2) ->
		    [b,B1];
		not (E1==E2) ->
		    [e,E1];
		true ->
		    ok
	    end;
	(B1==$\s) or (B2==$\s) , not (B1==B2) ->
		[b,B1];
	(E1==$\s) or (E2==$\s) ,not (E1==E2) ->
		[e,E1];
	true ->
	    ok
    end.

%%!!!!!!!!!!!!!!!ACTIONS!!!!!!!!!!!!!!

get_action() ->
    case string:tokens(io:get_line(?PROMPT),"\s\n\t") of
        [Action|_] -> Action;
        _          -> ""
    end.
            
   
spaces_action(Key, QueryLang, error)->
    io:format("Skip(s)~n"
	      "Change manually(c)~n"
	      "Look at file references(l)~n"
	      "Return(r)~n"
	      ,[]),
    Action = get_action(),
    action(Action, Key, QueryLang, spaces, error);
%%
spaces_action(Key, QueryLang, Args)->
    io:format("Skip(s)~n"
	      "ChangeAutomaticly(a)~n"
	      "Change manually(c)~n"
	      "Look at file references(l)~n"
	      "Return(r)~n",[]
		      ),
    Action = get_action(),
    action(Action, Key, QueryLang, spaces, Args).

%% automaticly change the string so there is no space error
%%
space_auto_change([a | Arg], Key, QueryLang) ->
    Value = lookup(Key,QueryLang),
    [B1,E1] = Arg,
    if 
	(B1==$\s) , (E1==$\s) ->
	    NewValue=" " ++Value++" ";
	B1==$\s ->
	    [_End | TmpValue] = lists:reverse(Value),
	    NewValue = " " ++lists:reverse(TmpValue);
	E1==$\s ->
	    [_End | TmpValue]  = Value,
	    NewValue = TmpValue ++ " ";
	true ->
	    [_Begin | TmpValue1] =  Value,
	    [_End | TmpValue2] = lists:reverse(TmpValue1),
	    NewValue =  lists:reverse(TmpValue2)		    
    end,
    NewValue;
space_auto_change([b ,B1], Key, QueryLang) ->
    Value = lookup(Key,QueryLang),
    if 
	B1==$\s ->
	    NewValue=" " ++ Value;		
	true ->
	    
	    [_End| NewValue] = Value       	    
    end,
    NewValue;
space_auto_change([e ,E1], Key, QueryLang) ->
    Value = lookup(Key,QueryLang),
    if
	E1==$\s ->
	    NewValue = Value++ " ";
	true ->
	    [_End| TmpValue] = lists:reverse(Value),
	    NewValue =  lists:reverse(TmpValue)
    end,
    NewValue.

%% it is used when the two values for different languages are the same
%%    
warning_action (Key,QueryLang) ->
    io:format("Skip(s)~n"
	      "ChangeValue(c)~n"
	      "Look at file references(l)~n"
	      "Return(r)~n",[]
	     ),
    Action = get_action(),
    action(Action, Key, QueryLang, warning, undefined).

%% error action for default language - it is used when there is
%% no key in default language
%%
error_action_default(Key,QueryLang) ->
    io:format("Skip(s)~n"
	      "AddValue(a)~n"
	      "Delete(d)~n"
	      "Look at file references(l)~n"
	      "Return(r)~n",[]),
    Action = get_action(),
    action(Action, Key, QueryLang, error, default).

%% error action used when there is no key in specified language
%% (QueryLang)
error_action(Key,QueryLang) ->
    io:format("Skip(s)~n"
	      "AddValue(a)~n"
	      "Look at file references(l)~n"
	      "Return(r)~n",[]),
    Action = get_action(),
    action(Action, Key, QueryLang, error, no_key).


%%
%% Action matrix.
%%
action("s", _, _, _, _)   -> true;
action("r", _, _, _, _)   -> ret;
action("l", Key, _, _, _) -> print_comments(get_comments(Key));
%%
action("a", Key, QLang, error, no_key) ->
    Similar = find_similar(Key, QLang),
    print_possibilities(Similar, no_key),
    case get_int('New Value> ') of
        {error,NewValue}->
            NewObj = {{Key, QLang}, NewValue},
            dets:insert(?TABLE_NAME, NewObj); 
        Num->
            if Num =< length(Similar) ->
                    {ChosenKey, Val} = lists:nth(Num, Similar),
                    dets:delete(?TABLE_NAME, {ChosenKey, QLang}),
                    NewObj = {{Key, QLang}, Val},
                    dets:insert(?TABLE_NAME, NewObj); 
               true ->
                    io:format("wrong value~n"),
                    error_action(Key, QLang)			   
            end
    end;
action(_, Key, QLang, error, no_key) -> 
    error_action(Key, QLang);
%%
action("a", Key, _, error, default) ->
    NewValue = get_string('New Value> '),
    NewObj = {{Key, ?DEFAULT_LANG}, NewValue},
    dets:insert(?TABLE_NAME, NewObj);
action("d", Key, QLang, error, default) ->
    dets:delete(?TABLE_NAME, {Key, QLang}); 
action(_, Key, QLang, error, default) ->
    error_action_default(Key,QLang);
%%
action("c", Key, QLang, warning, _) ->
    NewValue = get_string(?PROMPT),
    NewObj = {{Key, QLang}, NewValue},
    dets:insert(?TABLE_NAME, NewObj);	
action(_, Key, QLang, warning, _) ->
    warning_action(Key, QLang);
%%
%%
action("c", Key, QLang, spaces, _) ->
    NewValue = get_string(?PROMPT),
    NewObj = {{Key, QLang}, NewValue},
    dets:insert(?TABLE_NAME, NewObj);	
action("a", Key, QLang, spaces, Args) ->
    NewValue = space_auto_change(Args, Key, QLang),
    NewObj = {{Key, QLang}, NewValue},
    dets:insert(?TABLE_NAME, NewObj);
action(_, Key, QLang, spaces, X) ->
    spaces_action(Key, QLang, X).


print_comments([]) ->
    true;
print_comments([ Comment | Rest]) ->
    io:format("~p~n",[Comment]),
    print_comments(Rest).

%% print possible string that can match the one we need
%% it uses find_similar function to look for keys that 
%% have similar look and print out their values 
%% (with the number we can choose) 
print_possibilities([],_Other) ->
    true;
print_possibilities(List,no_key) ->
    io:format("possible values:~n",[]),
    F = fun(X,Acc) ->
		{_Key,Val} = X,				
		Num = integer_to_list(Acc),
		String = Num++". "++show_string(Val)++"~n",
		io:format("~p~n",[String]),
		Acc+1
	end,
    lists:foldl(F,1,List);
print_possibilities(List,_Other) ->
    io:format("possible values:~n",[]),
    F = fun(X,Acc) ->
		{Key,Val} = X,
		
		Num = integer_to_list(Acc),
		String = Num++". Key:"++show_string(Key)++" Val:"++
		    show_string(Val)++"~n",
		io:format("~p~n",[String]),
		Acc+1
	end,
    lists:foldl(F,1,List).


%% function used to find similar strings it goes thought all 
%% dets table in check if the string is similar using the
%% compare function
%%	    
find_similar(Key,Lang)->
    Strings = dets:match(?TABLE_NAME, {{'$1',Lang},'$2'}),
    F = fun([SKey,SVal],Acc) -> 
	      case compare(SKey,Key) of 
		  true ->
		      case lookup(SKey,?DEFAULT_LANG) of 
			  empty ->
			      [{SKey,SVal} | Acc];
			  _Other ->
			      Acc
		      end;
		  false ->
		      Acc
	      end
      end,
    
    lists:foldl(F,[],Strings).

%% check if two string are similar - it is using the
%% dist function which returns Levenshtein distance 
%% between two strings

compare(S1,S2) when not is_list(S1) or not is_list(S2) ->
    false;

compare(S1,S2) ->
    Len1 = string:len(S1),
    
    LevenDis  = dist(S1,S2),
    if 
 	Len1>20 ->
 	    if 
		LevenDis>4 ->
		    false;
		true ->
		    true
	    end;
 	Len1>7 ->
	     if 
		 LevenDis>3 ->
		     false;
		 true ->
		     true
	     end;
	 true ->
	     if 
		 LevenDis>1 ->
		     false;
		 true ->
		     true
	     end
    end.
	
%% ----------------------------------------------------------------------------
%% Calculates the Levenshtein distance between two strings
dist(S1, S2) ->
    FirstRow = lists:seq(0, length(S2)),
    dist2(S1, S2, FirstRow, 1).

dist2([H1|T1], S2, PrevRow, C) ->
    NextRow = next_row(H1, S2, PrevRow, [C]),
    dist2(T1, S2, NextRow, C+1);
dist2([], _, LastRow, _) ->
    lists:last(LastRow).


next_row(Ch, [H2|T2], [P0|TP], CurRow) ->
    V = min(P0+ch_cost(Ch, H2), hd(TP)+1, hd(CurRow)+1),
    next_row(Ch, T2, TP, [V|CurRow]);
next_row(_Ch, [], _PrevRow, CurRow) ->
    lists:reverse(CurRow).

ch_cost(C, C) -> 0;
ch_cost(_, _) -> 1.

min(A,B,C) ->
    if A < B, A < C -> A;
       B < C -> B;
       true -> C
    end.

%% get comments for the specified key from the default language .po file
%%
get_comments(Key)  ->
    [_Header |FileCommentsQuery] = 
	parse_po_comment(filename:join([?ROOT_DIR, ?CUSTOM_DIR,
                                        gettext:default_lang(), ?POFILE])),
    case lists:keysearch(Key,1,FileCommentsQuery) of
	{value, {_,Res}} ->
	    Res;
	_ -> []
    end.
    

%% function that saves changes made in dets to appropriate po file 
%% Kind argument can have two values: custom and default depending on 
%% what the language is in the system. Function parses the .po file 
%% and gets all commets for the keys - then they are merged with 
%% the keys and values from dets database and all together putted
%% to the file
%% 
save_to_po_file(QueryLang,Kind) ->
    FilePath = prepare_file(QueryLang,Kind),
    [Header |FileCommentsQuery] = parse_po_comment(FilePath),
    ValQuery = dets:match_object(?TABLE_NAME,{{'_',QueryLang},'_'}),
    SFileCommentsQuery = lists:keysort(1,FileCommentsQuery),
    SValQuery = lists:keysort(1,convert_val_query(ValQuery,[])),
    WholeQuery = two_to_one_merge(SValQuery,SFileCommentsQuery,[]),
    
    {ok,Fd} = file:open(FilePath,[write]),
    write_header(Fd,Header),
    write_entries(WholeQuery,Fd),
    file:close(Fd).

%% gets full path to the gettext file
%%
prepare_file(QueryLang,default) ->
    Fname = filename:join([?ROOT_DIR,?DEFAULT_DIR,QueryLang,?POFILE]),
    filelib:ensure_dir(Fname),    
    Fname;

prepare_file(QueryLang,custom) ->
    Fname = filename:join([?ROOT_DIR,?CUSTOM_DIR,QueryLang,?POFILE]),
    filelib:ensure_dir(Fname),    
    Fname.


%% convert the resulst of dets:match function to more useful one 
%% it removes the Lang parametr
%%
convert_val_query([{{Key,_Lang},Val}| Rest],NewQuery) ->
    if is_list(Key) ->
	    convert_val_query(Rest,[{Key,Val} | NewQuery]);
       true ->
	    convert_val_query(Rest,NewQuery)
    end;

convert_val_query([],NewQuery) ->    NewQuery.

%% merges two list basing on the fact that they both have the 
%% same key values
%%
two_to_one_merge([{Key1,Val1} | Rest1],L2,Result) ->
    
    
    case lists:keysearch(Key1,1,L2) of
	{value,{_Key,Val2}}->
	    NewTouple = {Key1,Val1,Val2};
	false ->
	    NewTouple = {Key1,Val1,[]}
    end,
    
    two_to_one_merge(Rest1,L2,[NewTouple |Result]);

two_to_one_merge([],_L2,Result) ->
    lists:keysort(1,Result).


%% writes header to the file 
write_header(Fd,{header_info,Header}) ->
    file:write(Fd, "msgid \"\"\n"),
    file:write(Fd, "msgstr "),
    file:write(Fd,"\""++Header++"\"\n").
	
%% write whole dets database to the file
write_entries(L,Fd) ->
    F = fun({Key,Val1,Val2}) ->
		write_comments(Fd,Val2),
		file:write(Fd, "msgid "),
		write_pretty(Key,Fd),
		file:write(Fd, "msgstr "),
		write_pretty(Val1,Fd),
		io:format(Fd, "~n", [])
	end,
    lists:foreach(F, L).


write_comments(Fd,[Val | Rest]) ->
    io:format(Fd,"#~s~n",[Val]),
    write_comments(Fd,Rest);
write_comments(_Fd,[]) ->
    true.

write_pretty(Str,_Fd) when is_atom(Str) ->
    true;
write_pretty([], _) ->
    true;
write_pretty(Str, Fd) when length(Str) =< ?ENDCOL ->
    write_string(Str, Fd);
write_pretty(Str, Fd) ->
    {Line, Rest} = get_line(Str),
    write_string(Line, Fd),
    write_pretty(Rest, Fd).

write_string(Str, Fd) ->
    %file:write(Fd, "\""),
    file:write(Fd, io_lib:print(Str)),
    file:write(Fd, "\n").


%%% Split the string into substrings, 
%%% aligned around a specific column.
get_line(Str) ->
    get_line(Str, ?SEP, 1, ?ENDCOL, []).

%%% End of string reached.
get_line([], _Sep, _N, _End, Acc) ->
    {lists:reverse(Acc), []};
%%% Eat characters.
get_line([H|T], Sep, N, End, Acc) when N < End ->
    get_line(T, Sep, N+1, End, [H|Acc]);
%%% Ended with a Separator on the End boundary.
get_line([Sep|T], Sep, End, End, Acc) ->
    {lists:reverse([Sep|Acc]), T};
%%% At the end, try to find end of token within
%%% the given constraint, else backup one token.
get_line([H|T] = In, Sep, End, End, Acc) ->
    case find_end(T, Sep) of
	{true, Racc, Rest} ->
	    {lists:reverse(Racc ++ [H|Acc]), Rest};
	false ->
	    case reverse_tape(Acc, In) of
		{true, Bacc, Rest} ->
		    {lists:reverse(Bacc), Rest};
		{false,Str} ->
		    %%% Ugh...the word is longer than ENDCOL...
		    split_string(Str, ?ENDCOL)
	    end
    end.

find_end(Str, Sep) ->
    find_end(Str, Sep, 1, ?PIVOT, []).

find_end([Sep|T], Sep, N, Pivot, Acc) when N =< Pivot ->
    {true, [Sep|Acc], T};
find_end(_Str, _Sep, N, Pivot, _Acc) when N > Pivot ->
    false;
find_end([H|T], Sep, N, Pivot, Acc) ->
    find_end(T, Sep, N+1, Pivot, [H|Acc]);
find_end([], _Sep, _N, _Pivot, Acc) ->
    {true, Acc, []}.
    
reverse_tape(Acc, Str) ->
    reverse_tape(Acc, Str, ?SEP).

reverse_tape([Sep|_T] = In, Str, Sep) ->
    {true, In, Str};
reverse_tape([H|T], Str, Sep) ->
    reverse_tape(T, [H|Str], Sep);
reverse_tape([], Str, _Sep) ->
    {false, Str}.

split_string(Str, End) ->
    split_string(Str, End, 1, []).

split_string(Str, End, End, Acc) ->
    {lists:reverse(Acc), Str};
split_string([H|T], End, N, Acc) when N < End ->
    split_string(T, End, N+1, [H|Acc]);
split_string([], _End, _N, Acc) ->
    {lists:reverse(Acc), []}.

%% Parses PO file for finding all file information comments
%%
parse_po_comment(Fname) ->
    {ok,Bin} = file:read_file(Fname),
    parse_po_bin_comment(Bin).

parse_po_bin_comment(Bin) ->
    parse_po_file_comment(binary_to_list(Bin),[]).

parse_po_file_comment("msgid" ++ T,FileList) ->
    {Key, R0} = get_po_string(T),
    {Val, Rest} = get_msgstr(R0),
    if 
	Key==header_info ->
	   
	    [{Key,Val} | parse_po_file_comment(Rest,[])];
	true ->
	    RevList = lists:reverse(FileList),
	    [{Key,RevList} | parse_po_file_comment(Rest,[])]
    end;
parse_po_file_comment("#"++ T,FileList) ->
    {Val,RO} = get_po_comment(T,[]),
    parse_po_file_comment(RO,[Val | FileList]);
parse_po_file_comment([_ | T],FileList) ->
    parse_po_file_comment(T,FileList);
parse_po_file_comment([],_FileList) ->
    [].

get_msgstr("msgstr" ++ T) ->
    get_po_string(T);
get_msgstr([_ | T]) ->
    get_msgstr(T).

get_po_comment([$\n|T],Val) ->
    {lists:reverse(Val),T};
get_po_comment([Char|T],Val) ->
    get_po_comment(T,[Char | Val]).

get_po_string([$\s|T]) -> get_po_string(T);
get_po_string([$\r|T]) -> get_po_string(T);
get_po_string([$\n|T]) -> get_po_string(T);
get_po_string([$\t|T]) -> get_po_string(T);
get_po_string([$"|T])  -> header_info(eat_string(T)).       %" make emacs happy

%%% only header-info has empty po-string !
header_info({"",R}) -> {header_info, R};  
header_info(X)      -> X.

eat_string(S) ->
    eat_string(S,[]).

eat_string([$\\,$"|T], Acc)   -> eat_string(T, [$"|Acc]);   % unescape !
eat_string([$\\,$\\ |T], Acc) -> eat_string(T, [$\\|Acc]);  % unescape !
eat_string([$\\,$n |T], Acc)  -> eat_string(T, [$\n|Acc]);  % unescape !
eat_string([$"|T], Acc)       -> eat_more(T,Acc);           %" make emacs happy
eat_string([H|T], Acc)        -> eat_string(T, [H|Acc]).

eat_more([$\s|T], Acc) -> eat_more(T, Acc);
eat_more([$\n|T], Acc) -> eat_more(T, Acc);
eat_more([$\r|T], Acc) -> eat_more(T, Acc);
eat_more([$\t|T], Acc) -> eat_more(T, Acc);
eat_more([$"|T], Acc)  -> eat_string(T, Acc);               %" make emacs happy
eat_more(T, Acc)       -> {lists:reverse(Acc), T}.


%% this function saves new entry in dets 'bugs' table which is placed in
%% ROOT_DIR 
%%
report_bug(ActualValue,ProperValue,Lang) ->
    FileName = filename:join([?ROOT_DIR,?BUG_FILE]),
    case dets:open_file(bugs,[{file,FileName}]) of
	{ok,bugs} ->
	    case dets:insert(bugs,{ActualValue,{ProperValue,Lang}}) of
		ok ->
		    Res = {ok,"New bug reported"}; 
		{error,_} ->
		    Res = {error,"Problem with inserting message"}
	    end;
	{error,_} ->
	    
	    Res = {error,"Problem with inserting message"}
    end,
    dets:sync(bugs),
    dets:close(bugs),
    Res.

%% go thought all the reported bugs with the dets:traverse function and 
%% asks what to do witch them bu using the check_bug_action function
check_bugs() ->	       
    FileName = filename:join([?ROOT_DIR,?BUG_FILE]),
    F = fun({ActualValue,{ProperValue,Lang}}) ->
	      case check_bug_action(ActualValue,ProperValue,Lang) of 
		  ok ->
		      continue;
		  ret ->
		      ret
	      end
      end,
    case dets:open_file(bugs,[{file,FileName}]) of
	{ok,bugs} ->	    
	    dets:traverse(bugs,F);
	{error,_} ->
	    erlang:error({dets,open_file,FileName})
    end,
    dets:sync(?TABLE_NAME), 
    Save = fun(X)->
		save_to_po_file(X,custom)
	end,
    Langs = all_langs(),
    lists:foreach(Save,Langs),
    dets:sync(bugs),
    dets:close(bugs),
    true.

%% find similar entries in the gettext database it is looking 
%% by checking the similarity of the dets values (not keys)
find_similar_by_value(Val,Lang)->
    Strings = dets:match(?TABLE_NAME, {{'$1',Lang},'$2'}),
    F = fun([SKey,SVal],Acc) -> 
	      case compare(Val,SVal) of 
		  true ->		      
		      [{SKey,SVal} | Acc];	 
		  
		  false ->
		      Acc
	      end
      end,
    
    lists:foldl(F,[],Strings).

%% bug action -it receive an input from the user - and do 	
%% appropriate actions
check_bug_action(ActualValue,ProperValue,Lang) ->
    
    io:format("Lang:~p~n"
	      "ActualValue:~p~n"
	      "ProposedValue:~p~n"
	      ,[Lang,ActualValue,ProperValue]),
    
    io:format("Skip(s)~n"
	      "AddValue(a)~n"
	      "Ignore(i)~n"
	      "Return(r)~n",[]),
    Action = io:get_line(?PROMPT),
    case Action of 
	"s\n" ->
	    ok;
	"r\n" ->
	    ret;
	"a\n" ->
	    Similar = find_similar_by_value(ActualValue,Lang),
	    print_possibilities(Similar,with_key),
     
	    NewKey = get_new_key(Similar),
	    NewValue = get_new_value(Similar,ProperValue), 
	    dets:insert(?TABLE_NAME,{{NewKey,Lang},NewValue}),
	    dets:delete(bugs,ActualValue),
	    ok;
 	"i\n" ->
	    dets:delete(bugs,ActualValue),
	    ok;
	_Other ->
	    check_bug_action(ActualValue,ProperValue,Lang)
    end.
		
%% get new Key. if the number is specyfied it gets the value    
%% from function argument which should be a list of {NewKey,NewVal} entries
%% returns new key

get_new_key(Similar) ->
    
    case get_int('Key>') of
	{error,Input}->
	    Input;
	Num ->	    
	    if 
		Num =<length(Similar) ->
		    {NewKey,_Val} = lists:nth(Num,Similar),
		    NewKey;
		true ->
		    io:format("wrong value~n"),
		    get_new_key(Similar)
	    end
    end.

%% get new Key. if the number is specyfied it gets the value    
%% from function argument which should be a list of {NewKey,NewVal} entries
%% returns new value    
get_new_value(Similar,ProperVal) ->
    
    case get_int('Value>') of
	{error,""} ->
	    ProperVal;
	{error,Input}->
	    Input;
	Num ->	    
	    if 
		Num =<length(Similar) ->
		    {_NewKey,NewVal} = lists:nth(Num,Similar),
		    NewVal;
		true ->
		    io:format("wrong value~n"),
		    get_new_value(Similar,ProperVal)
	    end
    end.
   

%% This function returns all languages that are in the system, based on the 
%% content of the ../gettext/priv/lang directory   
all_langs() ->
    gettext:all_lang() -- [gettext:default_lang()].

%%    CustomPath = filename:join(?ROOT_DIR,?CUSTOM_DIR),
%%    {ok,AllCustom} = file:list_dir(CustomPath),    
%%    DirList = [ X || X<-AllCustom,
%%		     filelib:is_dir(filename:join(CustomPath,X))==true],
%%    LangList = [ X || X<-DirList,
%%		     filelib:is_regular(filename:join(CustomPath,X)++
%%					"/gettext.po")==ntrue],
%%    LangList. 

   

