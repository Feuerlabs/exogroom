-module(sideband_sender_plugin).

-export_type([options/0, target/0]).

-type options()     :: [{atom(), any()}].
-type target()    :: any().
-type error()       :: {error, any()}.
-type data()    :: any().

-callback start(options()) ->
    {ok, pid()} | error().

-callback wakeup(pid(), target(), data()) ->
    ok | error().

-callback stop(pid()) ->
    ok | error().
