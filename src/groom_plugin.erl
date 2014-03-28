-module(groom_plugin).

-export_type([options/0, data/0, error/0, ok/0, ref/0, callback_pid/0, callback_arg/0]).

-type options()     :: [{atom(), any()}].
-type data()        :: any().
-type error()       :: {error, any()}.
-type ok()          :: {ok, data()}.
-type ref()             :: pid().
-type callback_pid()    :: pid().
-type callback_arg()    :: any().

-callback start(callback_pid(), callback_arg(), options()) ->
    {ok, ref()} | error().

-callback stop(ref()) ->
    ok.


