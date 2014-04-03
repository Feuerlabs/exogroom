%%%-------------------------------------------------------------------
%%% @author magnus <magnus@t520.home>
%%% @copyright (C) 2014, magnus
%%% @doc
%%%
%%% @end
%%% Created : 11 Mar 2014 by magnus <magnus@t520.home>
%%%-------------------------------------------------------------------
-module(groom_telnet).

-behaviour(gen_server).
-behaviour(groom_plugin).

%% groom_plugin API
-export([start/3,
	 stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(PORT_DEFAULT, 23). 

-record(st, { }).

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
start(CBPid, CBArg, Opts) ->
    gen_server:start_link(?MODULE, {CBPid, CBArg, Opts}, []).

stop(Ref) ->
    gen_server:call(Ref, router_stop_server).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({CBPid, CBArg, Opts}) ->
    Port = util:get_opt(port, Opts, ?PORT_DEFAULT),
    case  gen_tcp:listen(Port, [binary, { active, true }]) of
	{ok, ListenSock} ->
	    spawn_link(fun() -> accept(ListenSock, CBPid, CBArg, Opts) end),
	    {ok, #st{}};
	Err ->
	    { stop, Err }
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(router_stop_server, _From, St) ->
    %% FIXME: Keep track of all client connection processes and 
    %%        send them a terminate message.
    {stop, ok, St };

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Accept/spawn/traffic procedures
%%%===================================================================

accept(ListenSock, CBPid, CBRef, Opts) ->
    { ok, ClientSock } = gen_tcp:accept(ListenSock),
    spawn_link(fun() -> accept(ListenSock, CBPid, CBRef, Opts) end),
    handle_client_traffic(ClientSock, CBPid, CBRef, Opts, undefined).

%% Read the target bride's hostname/IP as the first line
handle_client_traffic(ClientSock, CBPid, CBRef, Opts, undefined) ->
    gen_tcp:send(ClientSock, "Device IP or hostname: "),
    receive 
	%% The first line received contains the IP address of the bride.
	{ tcp, _S, Data } ->
	    %% Crude: Delete the cr/nl
	    {Target, _CrNl} = lists:split(size( Data ) - 2, binary_to_list(Data)),
	    
	    %% Report the connection to the exogroom process.
	    io:format("Groom telnet process ~p will send client connect to exogroom ~p~n",
		      [self(), CBPid]),
	    CBPid ! { groom_client_connect, self(), Target, CBRef, ClientSock },

	    %% Enter loop to handle traffic.
	    handle_client_traffic(ClientSock, CBPid, CBRef, Opts, Target);

	_ -> %% Error or closed.
	    gen_tcp:close(ClientSock),
	    exit(normal)
    end;

%% Once we've sent off the client connect all to the exogroom process,
%% we will start receiving messages from the router server process, which
%% was handed our pid by exogroom.erl.
%%
handle_client_traffic(ClientSock, CBPid, CBRef, Opts, Target) ->
    receive 
	%% bride has connected back to router server.
	%% This is the first message we will receive back as a telnet 
	%% connection process.
	{ router_client_connect, From, ClID } ->
	    io:format("groom_telnet(): Got connection from bride (through router): ~p~n", [ ClID ]),
	    %% Use the From pid, which is the router's connecection process for the 
	    %% given device, as the new callback pid.
	    handle_client_traffic(ClientSock, From, CBRef, Opts, Target);
	    
	{ router_client_data, _From, ClID,Data } ->
	    io:format("groom_telnet(): Got data from device (through router) ~p: ~p~n", [ ClID, Data]),
	    gen_tcp:send(ClientSock, Data),
	    handle_client_traffic(ClientSock, CBPid, CBRef, Opts, Target);

	{ router_client_timeout, _From, ClID } ->
	    io:format("groom_telnet(): Got timeout from bride ~p. Exiting~n", [ ClID]),
	    gen_tcp:shutdown(ClientSock, read_write),
	    gen_tcp:close(ClientSock),
	    exit(normal);

	{ router_client_disconnect, _From, ClID } ->
	    io:format("groom_telnet(): Router client disconnected ~p. Exiting~n", [ ClID]),
	    gen_tcp:shutdown(ClientSock, read_write),
	    gen_tcp:close(ClientSock),
	    exit(normal);

	{ tcp, _S, Data } ->
	    io:format("groom_telnet(): Got data from telnet client: ~p, Sending to ~p~n", 
		      [ Data, CBPid]),

	    CBPid ! { groom_client_data, self(), CBRef, ClientSock, Data },
	    handle_client_traffic(ClientSock, CBPid, CBRef, Opts, Target);

	{ tcp_closed, _S } ->
	    io:format("groom_telnet(): Got disconnect from client. Forwarding to router ~p. Exit session~n", 
		      [ CBPid]),

	    CBPid ! { groom_client_disconnect, self(), CBRef },
	    gen_tcp:close(ClientSock),
	    exit(normal);

	Other -> %% Error or closed.
	    io:format("groom_telnet(): Got unknown message ~p. Exit session~n", 
		      [Other]),
	    CBPid ! { groom_client_disconnect, self(), CBRef },
	    gen_tcp:shutdown(ClientSock, read_write),
	    gen_tcp:close(ClientSock),
	    exit(normal)
    end.
