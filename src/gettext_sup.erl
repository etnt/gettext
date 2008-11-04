%%%----------------------------------------------------------------------
%%% File    : gettext_sup.erl
%%% Author  : tobbe@bluetail.com
%%% Purpose : Supervisor for the gettext handling
%%% Created : 28 Oct 2003 
%%%
%%% $Id$
%%%----------------------------------------------------------------------
-module(gettext_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0,start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    start_link([]).

start_link(Options) ->
    supervisor:start_link({local, gettext_sup}, gettext_sup, Options).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------
init([]) ->
    init([gettext_server]);  % just a default that should always work
init([CallBackMod]) ->
    GettextServer = {gettext_server,{gettext_server,start_link,[CallBackMod]},
	      permanent,5000,worker,[gettext_server]},
    {ok,{{one_for_one,3,10}, [GettextServer]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
