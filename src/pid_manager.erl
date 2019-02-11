-module(pid_manager).

-export([
    start/0,
    start/1,
    start_link/0,
    start_link/1,
    register/3,
    register/4,
    unregister/2,
    unregister/3,
    get_pid/2
]).

-define(SERVER_MODULE, pid_manager_server).

-type server_name() :: {local, atom()} | {global, atom()} | {via, atom(), term()}.
-type server_ref() ::  atom() | pid() | {global, atom()}.
-type option() :: sync.
-type options() :: [option()].

%% pid_manager: pid_manager library's entry point.
%% API

-spec start(server_name()) -> {ok, pid()} | {error, Reason::any()}.
start(Name) ->
    gen_server:start(Name, ?SERVER_MODULE, [], []).

-spec start() -> {ok, pid()} | {error, Reason::any()}.
start() ->
    gen_server:start(?SERVER_MODULE, [], []).

-spec start_link() -> {ok, pid()} | {error, Reason::any()}.
start_link() ->
    gen_server:start_link(?SERVER_MODULE, [], []).

-spec start_link(server_name()) -> {ok, pid()} | {error, Reason::any()}.
start_link(Name) ->
    gen_server:start_link(Name, ?SERVER_MODULE, [], []).


-spec register(server_ref(), any(), pid()) -> ok.
register(Ref, Id, Pid) when is_pid(Pid) ->
    register(Ref, Id, Pid, []).

-spec register(server_ref(), any(), pid(), options()) -> ok.
register(Ref, Id, Pid, Options) when is_pid(Pid) ->
    send_command(Ref, {register, Id, Pid}, Options).

-spec unregister(server_ref(), any()) -> ok.
unregister(Ref, Id) ->
    unregister(Ref, Id, []).

-spec unregister(server_ref(), any(), options()) -> ok.
unregister(Ref, Id, Options) ->
    send_command(Ref, {unregister, Id}, Options).

-spec get_pid(server_ref(), any()) -> pid() | undefined.
get_pid(Ref, Id) ->
    send_command(Ref, {get_pid, Id}, [sync]).

%% Internals

send_command(Ref, Command, Options)->
    case lists:member(sync, Options) of
        true ->
            gen_server:call(Ref, Command);
        false ->
            gen_server:cast(Ref, Command)
    end.

%% End of Module.
