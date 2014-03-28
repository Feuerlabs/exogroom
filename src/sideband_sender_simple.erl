%%%-------------------------------------------------------------------
%%% @author magnus <magnus@t520.home>
%%% @copyright (C) 2014, magnus
%%% @doc
%%%
%%% @end
%%% Created : 11 Mar 2014 by magnus <magnus@t520.home>
%%%-------------------------------------------------------------------
-module(sideband_sender_simple).

-behaviour(gen_server).
-behaviour(sideband_sender_plugin).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% router_server_plugin callback
-export([start/1, 
	 wakeup/3,
	 stop/1]).

-define(SERVER, ?MODULE). 
-define(RECEIVER_PORT_DEFAULT, 5715).
-define(SENDER_PORT_DEFAULT, 5716).

-record(st, {
	  sock = nil,
	  receiver_port = nil
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
start(Options) ->
    gen_server:start_link(?MODULE, Options, []).

wakeup(Pid, Target, Data) ->
    gen_server:call(Pid, {sideband_wakeup, Target, Data}).
    
stop(Pid) ->
    gen_server:call(Pid, sideband_stop),
    ok.


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
init(Options) ->
    RcvPort = util:get_opt(receiver_port, Options, ?RECEIVER_PORT_DEFAULT),
    SndPort = util:get_opt(sender_port, Options, ?SENDER_PORT_DEFAULT),
    case gen_udp:open(SndPort,  [binary, { active, true }]) of
	{ ok, Socket } ->
	    {ok, #st{ sock = Socket, receiver_port = RcvPort }};

	{ error, Reason } ->
	    {stop, Reason }
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
handle_call({ sideband_wakeup, DestIP, Data}, _From, St) ->
    io:format("Will send ~p as a wakeup to ~p:~p~n", [ Data, DestIP, St#st.receiver_port ]),
    gen_udp:send(St#st.sock, DestIP, St#st.receiver_port, Data),
    { reply, ok, St };

handle_call(sideband_stop, _From, St) ->
    gen_udp:close(St#st.sock),
    { stop, ok, St };

handle_call( {udp, _Socket, _IP, _Port, _Data }, _From, St) ->
    { reply, ok, St };

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

