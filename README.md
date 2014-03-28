

GROOM SIDE

application:start(sasl).

%% Start as gen server, not full app. Minor difference.

%% { simple, 
%%     groom_telnet, [ {port, 12345} ], 
%%     sideband_sender_simple, [], 
%%     router_server_simple, [ {if_addr, { 127,0,0,1 }}, { port, 4711 }]}]).

%% simple is the name of the service that ties groom, routing protocol, and
%% sideband sender together.
%% 
%% groom_telnet listens to telnet clients (telnet localhost 12345) and
%% forwards the traffic to the groom router (router_server_simple) to
%% be forwaded to the bride's router client.
%%
%% sideband_sender_simple sends a wakeup call to the bride, triggering
%% its bride router_client_simple server to connect back in to the 
%% groom router server. A key transmitted with the wakeup call is
%% used by the bride as a simple identification toward the groom router server.
%% 
%% router_server_simple listens to incoming bride connections trigered by
%% the wakeup call above. Any traffic received by the router server is
%% forwarded to groom_telnet to be forwarded to the telnet client.
%%

exogroom:start([{ simple, groom_telnet, [ {port, 12345} ], sideband_sender_simple, [], router_server_simple, [ {if_addr, { 127,0,0,1 }}, { port, 4711 }]}]).

%% Setup the bride.

%% Connect to groom_telnet and, as the first line sent, enter the IP address of the bride to conenct to

$ telnet localhost 12345
localhost   # The hostname of the bride (not ip, since I haven't 
            # fixed that yet) to send the wakeup msg to.

