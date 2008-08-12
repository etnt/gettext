-module(gettext_checking).

-export([text_util/0]).

-record(menu, {
	  intro_msg,
	  chosen,
	  parent_menu,
	  current_menu,
	  child_menu
	 }).

%%%-----------------------------------------------------------------------------
%%% Macro Name       : TOP_MSG
%%% Macro Explanation: Welcoming message for text_util/2
%%%-----------------------------------------------------------------------------
-define(TOP_MSG, 
	"==============================================================\n"
	" = = = = = = = = = T E X T    U T I L I T Y = = = = = = = = = \n"
	"==============================================================\n"
	" Welcome to the interactive gettext checker module,\n"
 	" let's begin checking for errors.\n"
	" Please choose the language you want to do a check on by\n"
	" typing in the corresponding number to the language you want.\n"
	"--------------------------------------------------------------").

%%%-----------------------------------------------------------------------------
%%% Macro Name       : OP_MSG
%%% Macro Explanation: Welcoming message for op_menu/2
%%% Macro connection : Connected with PO_FILE_MSG
%%%-----------------------------------------------------------------------------
-define(OP_MSG(Lang), 
       	"==============================================================\n"
	" = You have chosen "++Lang++" = \n").

%%%-----------------------------------------------------------------------------
%%% Macro Name       : PO_FILE_MSG
%%% Macro Explanation: Information message op_menu/2 (tells that a specific 
%%%                    po-file has been prepared for editing
%%% Macro connection : Connected with OP_MSG
%%%-----------------------------------------------------------------------------
-define(PO_FILE_MSG(File),  
	" The po-file\n"
	++File++"\n"
	" has been prepared for you to edit.\n"
	"==============================================================").

%%%-----------------------------------------------------------------------------
%%% Macro Name       : CONFIRM_MSG
%%% Macro Explanation: Confirmation message when you choose to quit
%%%-----------------------------------------------------------------------------
-define(CONFIRM_MSG, 
	"You have chosen to exit the Text Utility.\n"
	"Are you sure you want to do this?\n"
	"Enter y for yes or n for no: ").

%%%-----------------------------------------------------------------------------
%%% Macro Name       : ERROR_MSG
%%% Macro Explanation: Error message when bad input is entered
%%%-----------------------------------------------------------------------------
-define(ERROR_MSG,
	"==============================================================\n"
	" = =  Nonexisting option, please pick an existing option  = = \n"
	"==============================================================\n").

%%%-----------------------------------------------------------------------------
%%% Macro Name       : ERROR_MSG2
%%% Macro Explanation: Error message when a menu isn't found
%%%-----------------------------------------------------------------------------
-define(ERROR_MSG2,
       	"==============================================================\n"
	" = = = =  There is no such menu, please pick another  = = = = \n"
	"==============================================================\n").

%%%-----------------------------------------------------------------------------
%%% Macro Name       : LANG_OPTIONS
%%% Macro Explanation: Makes a list of all languages, sorts it and also converts
%%%                    it from two letter codes into its full language name.
%%%-----------------------------------------------------------------------------
-define(LANG_OPTIONS, 
	[iso639:lc2lang(X) || X <- lists:sort(gettext_server:all_lang())]).

%%%-----------------------------------------------------------------------------
%%% Macro Name       : LANGS
%%% Macro Explanation: Makes a list of all languages and sorts it (only two 
%%%                    letter codes is in the list).
%%%-----------------------------------------------------------------------------
-define(LANGS, lists:sort(gettext_server:all_lang())).

%%%-----------------------------------------------------------------------------
%%% Macro Name       : FIX_OPTIONS
%%% Macro Explanation: A list of the possible things you can fix with the
%%%                    Text utility
%%%-----------------------------------------------------------------------------
-define(FIX_OPTIONS, ["Fix space-errors",
		      "Fix msgid"]).

%%%-----------------------------------------------------------------------------
%%% Macro Name       : ALL_MENU_OPTIONS
%%% Macro Explanation: A tuple list with the different options for different 
%%%                    menus, the options is picked out with pick_menu/2
%%%-----------------------------------------------------------------------------
-define(ALL_MENU_OPTIONS, [{top_menu, ?LANG_OPTIONS},
			   {op_menu, ?FIX_OPTIONS}
			  ]).

%%%-----------------------------------------------------------------------------
%%% Macro Name       : ALL_FUNCTIONS
%%% Macro Explanation: A tuple list with the different menu functions in this 
%%%                    module (GUI menus and Text Utility Functions (things you
%%%                    can fix))
%%%-----------------------------------------------------------------------------
-define(ALL_FUNCTIONS, [{top_menu, fun top_menu/2},
			{op_menu, fun op_menu/2},
			{space_error, fun space_error/1},
			{msgid, fun msgid/1}
		       ]).

%%%-----------------------------------------------------------------------------
%%% Macro Name       : HIERARCHY
%%% Macro Explanation: A tuple list with the hierarchies for each menu.
%%% Macro notex      : Should be replaced by something else and much better.
%%%                    With this architecture each menu can only have one 
%%%                    sub-menu, which is not a good thing
%%%-----------------------------------------------------------------------------
-define(HIERARCHY, [{top_menu, {none, top_menu, op_menu}},
		    {op_menu, {top_menu, op_menu, none}},
		    {space_error, {op_menu, space_error, none}},
		    {msgid, {op_menu, msgid, none}}
		   ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Functions for different menus starts here 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%-----------------------------------------------------------------------------
%%% Function name    : text_util()
%%% Argument types   : n/a
%%% Function purpose : Initiate the Text Utility functionality
%%% Function info    : Initiates the record menu and passes on the atom top_menu
%%%                    and the entire record
%%%-----------------------------------------------------------------------------
text_util() ->
    Record = #menu{parent_menu = none,
		   current_menu = top_menu,
		   child_menu = op_menu
		  },
    top_menu(Record#menu.current_menu, Record).

%%%-----------------------------------------------------------------------------
%%% Function name    : text_util(Menu, Record)
%%% Argument types   : Menu is an atom, 
%%%                    Record is a record
%%% Function purpose : Provide the functionality of the top menu for the 
%%%                    Text Utility
%%% Function info    : ?TOP_MSG is a macro with a welcoming message for 
%%%                    text_util (defined in this module)
%%%-----------------------------------------------------------------------------
top_menu(Menu, Record) ->
    Msg = ?TOP_MSG,
    menu(Menu, Msg, Record).

%%%-----------------------------------------------------------------------------
%%% Function name    : op_menu(Menu, Record)
%%% Argument types   : Menu is an atom, 
%%%                    Record is a record
%%% Function purpose : Provide the functionality of the op menu (operation menu)
%%%                    for the Text Utility
%%% Function info    : ?OP_MSG is a macro with a welcoming message for 
%%%                    op_menu (defined in this module)
%%%-----------------------------------------------------------------------------
op_menu(Menu, Record) ->
    Msg = ?OP_MSG(Record#menu.intro_msg),
    Lang = lists:nth(Record#menu.chosen, ?LANGS),
    File = case Lang of
	       "sv" ->
		   filename:join([gettext_server:gettext_dir(), 
				  "lang", "default", "sv", "gettext.po"]);
	       _ ->
		   filename:join([gettext_server:gettext_dir(), 
				  "lang", "default", Lang, "gettext.po"])
	   end,
    FixedMsg = Msg++?PO_FILE_MSG(File),
    menu(Menu, FixedMsg, Record).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Functions for different menus ends here 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Creation of menus starts here 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%-----------------------------------------------------------------------------
%%% Function name    : menu(SelectedMenu, Msg, Record)
%%% Argument types   : SelectedMenu is an atom, 
%%%                    Msg is a string,
%%%                    Record is a record
%%% Function purpose : Make the menu system more general
%%% Function info    : SelectedMenu is an atom, to be used to pick out a menu
%%%                    with help from pick_menu/2 
%%%                    Msg is a string, which will be the intro message in for 
%%%                    that menu
%%%                    Record is an erlang record, used for navigating and set
%%%                    menu messages
%%%-----------------------------------------------------------------------------
menu(SelectedMenu, Msg, Record) ->
    Menu = pick_menu(?ALL_MENU_OPTIONS, SelectedMenu),
    draw_menu(Menu, [Msg], 1),
    choose_menu(Menu, Record).

%%%-----------------------------------------------------------------------------
%%% Function name    : draw_menu(Menu, Acc, Nr)
%%% Argument types   : Menu is a list
%%%                    Acc is a list
%%%                    Nr is a int
%%% Function purpose : Draw up a menu and number each choice from 1 to N
%%% Function info    : Menu is a list of options to be drawn
%%%                    Acc is an accumulator list, which has one element in it
%%%                    when first called, that element is the Intro message for
%%%                    that menu
%%%                    Nr is a counter, with the value 1 when first called, it's
%%%                    used to number all the menu choices rom 1 to N
%%%-----------------------------------------------------------------------------
draw_menu([], Acc, Nr) ->
    Previous = integer_to_list(Nr)++". Previous menu",
    Exit = integer_to_list(Nr+1)++". Exit",
    MenuItems = lists:reverse([Exit, Previous |Acc]),
    NrOfMenuItems = string:copies("~s~n", length(MenuItems)),
    io:format(NrOfMenuItems, MenuItems);
draw_menu([Hd|Tl], Acc, Nr) ->
    draw_menu(Tl, [integer_to_list(Nr)++". "++Hd|Acc], Nr+1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Creation of menus ends here 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Creation of possibility to make choices starts here 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%-----------------------------------------------------------------------------
%%% Function name    : choose_menu(Menu, Record)
%%% Argument types   : Menu is a list, 
%%%                    Record is a record
%%% Function purpose : Makes it possible to make a choice in a menu
%%% Function info    : Menu is a list of options used for error checking and
%%%                    making choices.
%%%                    Record is an erlang record, used for navigating and set
%%%                    menu messages
%%%-----------------------------------------------------------------------------
choose_menu(Menu, Record) -> 
    {Chosen, _Rest} = string:to_integer(io:get_line('> ')),
    if
	%% Simple bad input handling
	Chosen < 1 orelse 
	%% The reason for length(Menu)+2 is because you can then choose to exit,
	%% or go to a previous menu
	%% So the +1 is the previous option, and +2 is the exit option, so 
	%% therefore, the comparison has to be greater than length(Menu)+2
	Chosen > length(Menu)+2 orelse
	Chosen == error ->
	    io:format(?ERROR_MSG),
	    get_function(Record);
	Chosen > 0 andalso Chosen =< length(Menu) ->
	    Msg = lists:nth(Chosen, Menu),
	    UpdatedRecord = Record#menu{intro_msg = Msg, chosen = Chosen},
	    check_exist(Record#menu.child_menu, UpdatedRecord);
	%% The reason for length(Menu)+1 is that you can choose a previous menu.
	%% So the +1 is the previous option, so therefore, the comparison has to
	%% be equals to length(Menu)+1 if you want to enter a previous menu
	Chosen == length(Menu)+1 ->
	    check_exist(Record#menu.parent_menu, Record);
	%% The reason for length(Menu)+2 is because you can then choose to exit.
	%% So the +2 is the exit option, so therefore, the comparison has to
	%% be equals to length(Menu)+2 if you want to exit
	Chosen == length(Menu)+2 ->
	    terminate(Record)
    end.

%%%-----------------------------------------------------------------------------
%%% Function name    : terminate(Record)
%%% Argument types   : Record is a record
%%% Function purpose : Makes it possible to exit from any menu function above.
%%% Function info    : Record is an erlang record, used for navigating and set
%%%-----------------------------------------------------------------------------
terminate(Record) ->
    io:format(?CONFIRM_MSG),
    [Chosen2|_Rest2] = io:get_line(''),
    if
	Chosen2 == $Y orelse Chosen2 == $y ->
	    "The Text Utility has terminated";
	Chosen2 == $N orelse Chosen2 == $n ->
	    get_function(Record);
	%% Simple bad input handling
	true -> 
	    io:format(?ERROR_MSG),
	    get_function(Record)    
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Creation of possibility to make choices ends here 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc functions starts here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%-----------------------------------------------------------------------------
%%% Function name    : check_exist(ToTry, Record)
%%% Argument types   : ToTry is an atom, 
%%%                    Record is a record
%%% Function purpose : Check if the menu you're trying to access atually exists
%%% Function info    : The first clause matches the atom 'none' which means
%%%                    that the menu doesn't exist, so it just restarts at the
%%%                    same place.
%%%                    The second clause matches everything else, which will 
%%%                    be an existing menu. The record is updated and sent
%%%                    forward to get_function/1
%%%-----------------------------------------------------------------------------
check_exist(none, Record) ->
    io:format(?ERROR_MSG2),
    get_function(Record);
check_exist(ToTry, Record) ->
    {ParentMenu, CurrentMenu, ChildMenu} = 
	pick_menu(?HIERARCHY, ToTry),
    UpdatedRecord = Record#menu{
		      parent_menu = ParentMenu,
		      current_menu = CurrentMenu,
		      child_menu = ChildMenu},
    get_function(UpdatedRecord).

%%%-----------------------------------------------------------------------------
%%% Function name    : get_function(Record)
%%% Argument types   : Record is a record
%%% Function purpose : Get a function corresponding to the atom set in the 
%%%                    record field menu.current_menu
%%% Function info    : ?All_FUNCTIONS is a macro defined in this module which is
%%%                    tuple list in the form {atom, fun function_name/arity}
%%%-----------------------------------------------------------------------------
get_function(Record) ->
    Function = pick_menu(?ALL_FUNCTIONS, Record#menu.current_menu),
    Function(Record#menu.current_menu, Record).

%%%-----------------------------------------------------------------------------
%%% Function name    : pick_menu(List, Atom)
%%% Argument types   : List is a list,
%%%                    Atom is an atom
%%% Function purpose : Get an element in a list without knowing it's location 
%%% Function info    : the atom 'none' is there to make the previous menu and
%%%                    choice of any menu not to break if there is no such menu
%%%-----------------------------------------------------------------------------
pick_menu(_, none) ->
    none;
pick_menu([{Atom, Options} |_Tl], Atom) ->
    Options;
pick_menu([_Hd|Tl], Atom) ->
    pick_menu(Tl, Atom).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc functions ends here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Text Utility functions starts here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
space_error(PoFile) ->
    PoFile.

msgid(PoFile) ->
    PoFile.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Text Utility functions ends here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
