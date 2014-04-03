%%%-------------------------------------------------------------------
%%% @author magnus <magnus@t520.home>
%%% @copyright (C) 2014, magnus
%%% @doc
%%%
%%% @end
%%% Created : 11 Mar 2014 by magnus <magnus@t520.home>
%%%-------------------------------------------------------------------
-module(router_server_simple).

-behaviour(gen_server).
-behaviour(router_server_plugin).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% router_server_plugin callback
-export([start/1, 
	 prepare_bride_connection/4,
	 send_client/3]).

-define(SERVER, ?MODULE). 
-define(KEY_TIMEOUT_DEFAULT, 5000). 
-define(PORT_DEFAULT, 4711). 
-define(IF_ADDR_DEFAULT, {127,0,0,1}). 
-define(ETS_TABLE, simple_router_clients). 
-define(ETS_TABLE_KEY, simple_router_client_key).  %% Secondary index is the client key

-record(client, {
	  pid = nil,
	  cb_pid = nil,
	  validated = false,
	  id = nil,
	  key = nil,
	  sock = nil
	 }).

-record(client_key, {
	  key = nil,
	  timer_ref = nil,
	  id = nil
	 }).

-record(st, {
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
start(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% Add a client key in preparation for an incoming connection
%% from the given client
%% When an incoming connection is received and authorized by the given key,
%% CBPid will get a { client_connect, CBArg } message sent to it.
%% If no client connection is provided within the given timeout (msec),
%% CBPid will get a { no_client_connect, CBArg }.
%%
prepare_bride_connection(Pid, CBPid, ClientID, Timeout) ->
    gen_server:call(Pid, {router_prepare_bride_connection, CBPid, ClientID, Timeout}).

send_client(Pid, ClientID, Data) ->
    gen_server:call(Pid, {router_send_client, ClientID, Data}).
    
%%===================================================================
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
init(Opts) ->
    %% NOT SECURE!
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),

    ets:new(?ETS_TABLE, [ named_table, set, public, { keypos, #client.id }]),
    ets:new(?ETS_TABLE_KEY, [named_table,  set, public, { keypos, #client_key.key }]),

    IFAddr  = util:get_opt(if_addr, Opts, ?IF_ADDR_DEFAULT),
    Port = util:get_opt(port, Opts, ?PORT_DEFAULT),

    %% Setup a prepare_bride_connection timeout thread that 
    %% keeps the ets table nice and clean.
    %%
    case gen_tcp:listen(Port, 
			[ binary, 
			  { active, true },
			  { ip, IFAddr}]) of
	{ok, ListenSock} ->
	    spawn_link(fun() -> accept(ListenSock, Opts) end),
	    {ok, #st{  }};
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

handle_call({router_send_client, ClientID, Data}, _From, St) ->
    case find_client(ClientID) of
	false ->
	    {reply, {error, client_not_found}, St};

	{ok, Client} -> 
	    gen_tcp:send(Client#client.sock, Data),
	    {reply, ok, St}

    end;
	    


handle_call( {router_prepare_bride_connection, 
	      CBPid, 
	      ClientID,
	      _Timeout}, _From, St) ->
    %% If the router server reserves a specific port or other resource
    %% For the given client, set Key to a tuple with the information
    %% needed for the router client to connect back and authenticate.
    %% In this simple example, we will just listen to one port
    %% and use a random number as an client authentication / identification.
    
    Key = integer_to_list(random:uniform(8999999999) + 1000000000), %% NOT SECURE IN ANY WAY.
    add_client_record(CBPid, ClientID, Key, ?KEY_TIMEOUT_DEFAULT, self()),
    { reply, {ok, Key }, St};

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
handle_info({ client_connect_timeout, ClientID, Key, CBPid },  St) ->              
    %% Check if client has already been validated
    case find_client(ClientID) of
	{ok, TmpCl} when TmpCl#client.validated =:= false -> %% Update with socket.
	    io:format("router_server_simple: Bride ~p timed out. Notifying groom ~p ~n", 
		      [ Key, CBPid ]),

	    CBPid ! { router_client_timeout, self(), ClientID },
	    delete_client_record(Key),
	    true;

	_ ->
	    true
    end,
    { noreply, St#st { }};

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
%%% Client ets functions
%%%===================================================================
add_client_record(CBPid, ClientID, Key, Timeout, TimeoutPid) ->
    io:format("Adding clientID (~p) CB(~p) key (~p)~n", [ClientID, CBPid, Key]),

    TRef = erlang:send_after(Timeout, TimeoutPid, 
			     { client_connect_timeout, ClientID, Key, CBPid }),

    ets:insert(?ETS_TABLE, #client { 
			      id = ClientID,
			      key = Key,
			      cb_pid = CBPid,
			      sock = nil,
			      validated = false
			     }),

    ets:insert(?ETS_TABLE_KEY,
	       #client_key { 
		  key = Key,
		  timer_ref = TRef,
		  id = ClientID
		 }),
    ok.
    
find_client(ClientID) ->
    case ets:lookup(?ETS_TABLE, ClientID) of

	[ Cl = #client { } ] -> 
	    {ok, Cl};

	_ -> 
	    not_found
    end.

validate_client(Key, Pid, Socket) ->
    %% Use secondary index table to retrieve key->client_ref relation
    io:format("Validating key (~p) on socket (~p)~n", [Key, Socket]),
    case ets:lookup(?ETS_TABLE_KEY, Key) of
	[ #client_key { id = ClientID, timer_ref = TimerRef } ] ->
	    case find_client(ClientID) of
		{ok, TmpCl} -> %% Update with socket.
		    %% Cancel timer
		    NewCl = TmpCl#client { sock = Socket, 
					   pid = Pid, 
					   validated = true },
		    ets:insert(?ETS_TABLE, NewCl),
		    erlang:cancel_timer(TimerRef),
		    {ok, NewCl};

		_ -> %% Mismatch between ETS_TABLE_KEY and ETS_TABLE
		    ets:delete(?ETS_TABLE_KEY, Key),
		    not_found
	    end;

	_ -> %% Key not found.
	    not_found
    end.


delete_client_record(Key) ->
    case ets:lookup(?ETS_TABLE_KEY, Key) of
	[ #client_key { id = ClientID, timer_ref = nil } ] ->
	    ets:delete(?ETS_TABLE_KEY, Key),
	    ets:delete(?ETS_TABLE, ClientID),
	    ok;

	[ #client_key { id = ClientID, timer_ref = TRef } ] ->
	    erlang:cancel_timer(TRef),
	    ets:delete(?ETS_TABLE_KEY, Key),
	    ets:delete(?ETS_TABLE, ClientID),
	    ok;

	_ -> not_found
    end.

		    

%%%===================================================================
%%% Accept/spawn/traffic procedures
%%%===================================================================

accept(ListenSock, Opts) ->
    { ok, ClientSock } = gen_tcp:accept(ListenSock),
    spawn_link(fun() -> accept(ListenSock, Opts) end),
    io:format("router_server_simple: Spawned client session.~n"),
    handle_client_key(ClientSock, Opts).


handle_client_key(ClientSock, Opts) ->
    io:format("router_server_simple: Receiving client traffic..~n"),
    receive 
	{ tcp, _S, << "key:", Key:10/bytes, _/binary >> } ->
	    io:format("router_server_simple: Session got key ~p ~n", [Key]),
	    handle_client_traffic(ClientSock, Opts, binary_to_list(Key));

	Other -> %% Illegal traffic
	    io:format("router_server_simple: ?Session got ~p ~n", [Other]),
	    exit(normal)

    after ?KEY_TIMEOUT_DEFAULT ->
	    io:format("router_server_simple: Timed out while waiting for router client key. Exit~n"),
	    exit(normal)
    end.


handle_client_traffic(ClientSock, _Opts, Key) ->
    Cl = case validate_client(Key, self(), ClientSock) of
	     { ok, TmpCl = #client {} } ->
		 io:format("router_server_simple(~p): Client Key validated. Notifying ~p ~n",
			  [self(), TmpCl#client.cb_pid]),
		 TmpCl#client.cb_pid ! { router_client_connect, self(), TmpCl#client.id },
		 TmpCl;
	     _ -> 	 
		 io:format("router_server_simple: Client Key failed ~n"),
		 delete_client_record(Key),
		 exit(normal)
	 end,

    %% We found a client, and the key is correct.
    handle_client_traffic_loop(Cl).


handle_client_traffic_loop(Cl) ->		     
    receive 
	%% Process data from the socket to the remote router client
	{ tcp, _S, Data } ->
	    io:format("router_server_simple: Session got ~p from router client. Sending to ~p ~n", 
		      [Data, Cl#client.cb_pid ]),

	    Cl#client.cb_pid ! { router_client_data, 
				 self(),
				 Cl#client.id, 
				 Data },
	    handle_client_traffic_loop(Cl);

	%% Process data from the groom process serving a single incoming
	%% telnet/http/ssh/whatever client.
	{ groom_client_data, From, _Ref, _GrSock, Data } ->
	    io:format("router_server_simple: Got ~p from groom process ~p. Fwd to router client ~n", 
		      [Data, From]),

	    gen_tcp:send(Cl#client.sock, Data),
	    handle_client_traffic_loop(Cl);

	%% Process data from the groom process serving a single incoming
	%% telnet/http/ssh/whatever client.
	{ groom_client_disconnect, From, _Ref } ->
	    io:format("router_server_simple: Got disconnect from groom process ~p. Terminating session.~n", 
		      [From]),

	    delete_client_record(Cl#client.id),
	    gen_tcp:shutdown(Cl#client.sock, read_write),
	    gen_tcp:close(Cl#client.sock),
	    exit(normal);

	{ tcp_closed, _S } ->

	    io:format("router_server_simple: Got disconnect from router client. Terminating session.~n"),
	    Cl#client.cb_pid ! { router_client_disconnect, self(), 
				 Cl#client.id },
	    
	    delete_client_record(Cl#client.key),
	    gen_tcp:close(Cl#client.sock),
	    exit(normal);

	Other -> %% Error or closed.
	    io:format("router_server_simple: ??Session got ~p ~n", [Other]),
	    Cl#client.cb_pid ! { router_client_disconnect, self(), 
				 Cl#client.id },
	    delete_client_record(Cl#client.key),
	    exit(normal)
    end.

