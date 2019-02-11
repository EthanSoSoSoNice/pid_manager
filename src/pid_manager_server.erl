-module(pid_manager_server).

-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2]
).

-record(state, {
    id_map = map:map(), %% {id, {pid, mref}}
    mref_map = map:map() %% {mref, id}
}).


init([]) ->
    State = #state{
        id_map = #{},
        mref_map = #{}
    },
    {ok, State}.

handle_call({get_pid, Id}, _From, State) ->
    IdMap = State#state.id_map,
    case IdMap of
        #{ Id := {Pid, _}} ->
            {reply, Pid, State};
        _ ->
            {reply, undefined, State}
    end;
handle_call(_Msg, _From, State) ->
    {noreply, State}.


handle_cast({unregister, Id}, State) ->
    {noreply, unregister_pid(Id, State)};
handle_cast({register, Id, Pid}, State) ->
    {noreply, register_pid(Id, Pid, State)};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MRef, process, _Pid, _}, State) ->
    MRefTable = State#state.mref_map,
    case MRefTable of
        #{ MRef := Id } ->
            {noreply, unregister_pid(Id, State)};
        _ ->
            {noreply, State}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_Old, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%%%=============================
%%% Internal 
%%%=============================

unregister_pid(Id, State) ->
    try_demonitor_pid(Id, State).

register_pid(Id, Pid, State) ->
    State1 = try_demonitor_pid(Id, State),
    MRefMap = State1#state.mref_map,
    IdMap = State1#state.id_map,
    NewMRef = erlang:monitor(process, Pid),
    NewIdMap = IdMap#{ Id => {Pid, NewMRef} },
    NewMRefMap = MRefMap#{ NewMRef => Id },
    State1#state{ 
        mref_map = NewMRefMap,
        id_map = NewIdMap
    }.

try_demonitor_pid(Id, State) ->
    IdMap = State#state.id_map,
    MRefMap = State#state.mref_map,
    case maps:find(Id, IdMap) of
        error ->
            State;
        {ok, {_Pid, MRef}} ->
            erlang:demonitor(MRef),
            NewIdMap = maps:remove(Id, IdMap),
            NewMRefMap = maps:remove(MRef, MRefMap),
            State#state{
                id_map = NewIdMap,
                mref_map = NewMRefMap
            }
    end.