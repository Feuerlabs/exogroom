-module(router_server_plugin).

-export_type([options/0, data/0, error/0, ok/0, client_id/0, cb_pid/0, timeout/0]).

-type options()     :: [{atom(), any()}].
-type data()        :: any().
-type error()       :: {error, any()}.
-type ok()          :: {ok, data()}.
-type timeout()     :: integer().
-type cb_pid()       :: pid().
-type client_id()       :: any().


-callback start(options) ->
    { ok, pid() } | error().

-callback prepare_bride_connection(pid(), cb_pid(), client_id(), timeout()) ->
     { ok, key } | error().

-callback send_client(pid(), client_id(), data()) ->
    ok  | error().

%% Disconnect a client by having the groom send it { groom_client_disconnect, self(), Ref }
