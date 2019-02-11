-module(pid_manager_test).

-export([test/0]).

-define(SERVER, ?MODULE).

test() ->
    pid_manager:start({local, ?SERVER}),
    ok = pid_manager:register(?SERVER, 1, self()),
    ok = pid_manager:register(?SERVER, 2, self()),
    true = self() =:= pid_manager:get_pid(?SERVER, 1),
    pid_manager:unregister(?SERVER, 1),
    true = undefined =:= pid_manager:get_pid(?SERVER, 1),
    true = self() =:= pid_manager:get_pid(?SERVER, 2),
    Pid = spawn(fun() -> receive stop -> ok end end),
    pid_manager:register(?SERVER, 2, Pid),
    true = pid_manager:get_pid(?SERVER, 2) =:= Pid,
    Pid ! stop,
    timer:sleep(500),
    true = pid_manager:get_pid(?SERVER, 2) =:= undefined,
    ok.
