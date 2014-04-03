%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 14 Jun 2012 by magnus <magnus@feuerlabs.com>
%%%-------------------------------------------------------------------
-module(exogroom).

-behaviour(gen_server).

%% API
-export([start/0, start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(SERVER, ?MODULE).
-define(GROOM_SERVICES, services).

-record(service, {
	  name = undefined,
	  router_pid = undefined,
	  router_mod = undefined,
	  sideband_pid = undefined,
	  sideband_mod = undefined
	 }).
	  
-record(st, { 
	  services = []
	 }).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


start(Services) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Services, []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, St} |
%%                     {ok, St, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, App } = application:get_application(),
    %% { links, [
    %%   { simpe, sideband_sender_simple, [ { sieband_opt1, 1} ], 
    %%             groom_telnet, [{ room_opt1, 1 },
    %%             router_simple, [ { router_opt1, 1}] 
    %% ]}
    Services = application:get_env(App, ?GROOM_SERVICES, []),
    {ok, #st { services = setup_services(Services) }};

init(Services) ->
    {ok, #st { services = setup_services(Services) }}.


%%--------------------------------------------------------------------
%% @private
%% @docp
%% Handling call messages
%%
%% @spec handle_call({open, Device}, From, St) ->
%%                                   {reply, Reply, St} |
%%                                   {reply, Reply, St, Timeout} |
%%                                   {noreply, St} |
%%                                   {noreply, St, Timeout} |
%%                                   {stop, Reason, Reply, St} |
%%                                   {stop, Reason, St}
%% @end
%%--------------------------------------------------------------------
handle_call(Command, _From, St) ->
    {reply, {unknown_command, Command}, St}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, St) -> {noreply, St} |
%%                                  {noreply, St, Timeout} |
%%                                  {stop, Reason, St}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, St) ->
    {noreply, St}.


%% An incoming ssh / telnet / http client session was detected.
%% This will iniiate the marry-me protocol procedure.
handle_info({groom_client_connect, From, BrideAddr, Service, _Socket}, St) ->

    io:format("exogroom:handle_info(): groom_client connect from ~p: ~p -> ~p~n", 
	      [From, Service, BrideAddr]),

    case find_service(Service, St#st.services) of
	false ->
	    io:format("WARNING: Unknown service name: ~p~n", [Service]),
	    { noreply, St };

	#service { router_pid = RtPid,
		   router_mod = RtMod,
		   sideband_pid = SbPid,
		   sideband_mod = SbMod } ->
	    io:format("exogroom: Preparing bride conncetion from ~p~n", [ BrideAddr ]),
	    %% We want the incoming bride connection to be reported to groom client
	    {ok, WakeupData } = RtMod:prepare_bride_connection(RtPid, From, BrideAddr, infinity),

	    io:format("exogroom: Waking up bride ID ~p through sideband module ~p~n", 
		      [ BrideAddr,SbMod ]),

	    SbMod:wakeup(SbPid, BrideAddr, WakeupData),
	    { noreply, St}
    end;

handle_info(Info, St) ->
    io:format("exogroom:handle_info(): ~p ~p~n", [Info, St]),
    {noreply, St}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, St) -> void()
%% @end
%%-----------------------------------------\---------------------------
terminate(_Reason, _St) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Command process st when code is changed
%%
%% @spec code_change(OldVsn, St, Extra) -> {ok, NewSt}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


setup_services(SvcList) ->     
    setup_services(SvcList, []).


setup_services([], Acc) ->
    Acc;

setup_services([{ Name, 
		  GrMod, GrOpts, 
		  SidebandMod, SbOpts,
		  RouterMod, RtOpts} | T], Acc) ->

    io:format("exogroom:setup_services(~p): Groom: ~p GroomOpts: ~p~n", 
	      [ Name, GrMod, GrOpts ]),

    io:format("exogroom:setup_services(~p): Sideband: ~p SidebandOpts: ~p~n", 
	      [ Name,SidebandMod, SbOpts ]),
    
    io:format("exogroom:setup_services(~p): Router: ~p RouterOpts: ~p~n", 
	      [ Name, RouterMod, RtOpts ]),

    %% Fire up the groom.
    case GrMod:start(self(), Name, GrOpts) of
	{ ok, GrPid } ->

	    %% Setup sideband module
	    case SidebandMod:start(SbOpts) of 
		{ok, SbPid} ->
		    
		    %% Setup router module
		    case RouterMod:start(RtOpts) of
			{ ok, RtPid } ->
			    setup_services(T, [ #service { 
						   name = Name, 
						   sideband_pid = SbPid, 
						   sideband_mod = SidebandMod,
						   router_pid = RtPid,
						   router_mod = RouterMod
						  } | Acc ]);
			
			RtErr ->
			    io:format("setup_services(~p): Failed to setup router ~p: ~p~n", 
				      [ Name, RouterMod, RtErr ]),
			    SidebandMod:stop(SbPid),
			    GrMod:stop(GrPid),
			    setup_services(T,  Acc)

		    end;

		SbErr ->
		    io:format("setup_services(~p): Failed to setup sideband ~p: ~p~n", 
			      [ Name, SidebandMod, SbErr ]),
		    GrMod:stop(GrPid),
		    setup_services(T,  Acc)
	    end;
	GrErr ->	
	    io:format("setup_services(~p): Failed to setup groom ~p: ~p~n", 
		      [ Name, GrMod, GrErr ]),

	    setup_services(T,  Acc)
    end;

setup_services([WrongFormat | T], Acc) ->
    io:format("setup_services(~p): Wrong foirmat. Ignored.~n",
	      [ WrongFormat ]),
    io:format("                    Use { Name, GrMod, GrOpts, SbMod, SbOpts, RtMod, RtOpts }~n"),
    setup_services(T, Acc).


find_service(Name, SvcList) ->
    lists:keyfind(Name, #service.name, SvcList).
