%%%-------------------------------------------------------------------
%%% @author golubkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(bs).

-behaviour(gen_fsm).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
    handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% public API
-export([start_link/1,stop/1,connect/3,ack/2]).
-export([unexpected/2]).

%% custom state names
-export([available/2,
        get_ack/2,
        full/3,
        full/2,
        poll/2,
        info/1]).

-define(SERVER, ?MODULE).

-define(MAX_CAPACITY, 2).


-record(bs_state, {name,mspid,msname,monitor,from,current_capacity=0,poll_db}).

-define(rec_info(T,R),lists:zip(record_info(fields,T),tl(tuple_to_list(R)))).

-import(db_ets, [db_init/0,db_close/1,db_delete/2,db_get/2,db_put/3,db_empty/1,db_query/2]).


%%%===================================================================
%%% API
%%%===================================================================

%%% PUBLIC API

%%{ok, Pid} | {error, Reason}
%%Start a base station process with the specified name. [Called by op/bsc]
start_link(BSName) ->
    io:format("~p starting ~n", [BSName]),
    gen_fsm:start_link({local, BSName}, ?MODULE, BSName, []).

%%bs:stop(BSPid) -> ok
%%Stop a specific base station. [Called by op/bsc]
stop(BSPid) ->
    %%gen_fsm:call(?MODULE, stop),
    %%TODO: how to correctly stop
    gen_fsm:stop(BSPid).
    %%gen_fsm:stop(?MODULE).

%%bs:connect(BSPid, MSPid, MSName)
%%Connect to a specific base station. [Called by a phone]
%%gen_fsm:sync_send_event(BSPid, {connect, MSPid, MSName}, 50000).
connect(BSPid, MSPid, MSName) ->
    gen_fsm:send_event(BSPid, {connect, MSPid, MSName}).

%%bs:ack(BSPid,MSName)
%%Acknowledge a connection to a specific base station. [Called by a phone].
ack(BSPid,MSName) ->
    io:format("~p acknowledgement 1 ~n", [MSName]),
    gen_fsm:send_event(BSPid, {acknowledgement,MSName}).

init(BSName) ->
    io:format("~p init ~n", [BSName]),
    {ok, PollList} = db_init(),
    {ok, available, #bs_state{name=BSName,poll_db=PollList},5000}.

info(BSPid) -> gen_fsm:sync_send_event(BSPid, info,50000).

%% The state machine
%%available({connect, MSPid, MSName}, State=#bs_state{}) ->
%%    {next_state, available, State}.

poll(MSName, PollDB) ->
    case db_get(MSName,PollDB) of
        undefined  -> case erlang:whereis(MSName) of
                          undefined ->
                                io:format("~p: phone is not started ~n", [MSName]),
                                db_put(MSName, 1, PollDB);
                          _Pid ->
                              db_put(MSName, 0, PollDB)
                      end;
        {ok, Attempt} -> case lists:last(Attempt) < 5 of
                             true -> io:format("~p: ~p: attempt ~n", [MSName,Attempt]),
                                 case erlang:whereis(MSName) of
                                     undefined ->
                                         io:format("~p: phone is not started second ~n", [MSName]),
                                         db_put(MSName, lists:last(Attempt) + 1, PollDB);
                                     _Pid ->
                                         db_put(MSName, 0, PollDB)
                                 end;
                             false -> io:format("~p: ~p: MS is lost ~n", [MSName, Attempt]),
                                 lr:lost(MSName),
                                 db_delete(MSName, PollDB)
                        end;
        Other -> io:format("~p ~p: unexpected msg ~n", [MSName,Other])
    end.




available({connect, MSPid, MSName}, State=#bs_state{}) ->
    io:format("~p connect ~n", [MSName]),
    case State#bs_state.current_capacity < ?MAX_CAPACITY of
        true -> phone:respond(connect,MSName),
            {next_state, get_ack, State#bs_state{mspid = MSPid, msname = MSName}, 5000};
        false -> phone:respond(reject,MSName),
            {next_state, available, State, 5000}
    end;
    %%Ref = monitor(process, MSPid),
%%    case phone:respond(connect,MSPid) of
%%        ok ->
%%            %ask_negotiate(MSPid, self()),
%%            notice(State, "asking MS ~p for a confirm", [MSPid]),
%%            Ref = monitor(process, MSPid),
%%            %%{next_state, get_ack, State#bs_state{mspid = MSPid, msname = MSName, monitor=Ref, from=From}};
%%            {reply, ok, authorized, Name};
%%        {error, Reason} ->
%%            %% TODO
%%            {reply, {error, Reason}, available, State}
%%    end;
%%available(timeout, State=#bs_state{}) ->
%%    io:format("~p timeout ~n", [State]),
%%    {next_state, available, State#bs_state{mspid = nopid, msname = noname},5000};
available(timeout, State=#bs_state{}) ->
    %%io:format("available timeout ~w ~n",[?rec_info(bs_state,State)]),
    case whereis(lr) of
        undefined -> io:format("~p: lr is not started ~n", [State#bs_state.name]),
            {next_state, available, State,5000};
        _Pid -> case lr:who_are_at(State#bs_state.name) of
                    none -> %%io:format("~p: who are at: none ~n", [State#bs_state.name]),
                        {next_state, available, State,5000};
                    {ok,PhoneList} -> io:format("~p: who are at ~p ~n", [State#bs_state.name,PhoneList]),
                        %%lists:map(fun(X)-> poll(X,State#bs_state.poll_db) end, PhoneList),
                        case length(PhoneList) < ?MAX_CAPACITY  of
                            true -> [poll(Elem,State#bs_state.poll_db) || Elem <- PhoneList],
                                io:format("poll_db: ~p ~n", [State#bs_state.poll_db]),
                                {next_state, available, State#bs_state{current_capacity = length(PhoneList)},5000};
                            false -> io:format("going to full state: ~p ~n", [State#bs_state.poll_db]),
                                {next_state, full, State#bs_state{current_capacity = length(PhoneList)},5000}
                        end
                end
    end;
available(info, State=#bs_state{}) ->
    io:format("Info: ~w ~n", [?rec_info(bs_state,State)]),
    {reply, ok, info, State, 5000};
available(Event, State=#bs_state{}) ->
    unexpected(Event, available),
    {next_state, available, State, 5000}.

%%unauthorized(_Event, _From, State) ->
%%    Reply = {error, invalid_message},
%%    {reply, Reply, unauthorized, State}.


get_ack({acknowledgement, MSName}, State = #bs_state{}) ->
    %%eb_server:deposit(State, Amount),
    io:format("~p ~p acknowledgement 2 ~n", [MSName,State#bs_state.name]),
    lr:located_at(MSName, State#bs_state.name),
    %%phone:respond(connect,MSName),
    {next_state, available, State, 5000};
get_ack(info, State=#bs_state{}) ->
    io:format("Info: ~w ~n", [?rec_info(bs_state,State)]),
    {reply, ok, get_ack, State, 5000};
get_ack(Event, State) ->
    io:format("~p unkonown state ~n", [Event]),
    unexpected(Event, acknowledgement),
    {next_state, acknowledgement, State, 5000}.


full(timeout, State=#bs_state{}) ->
    %%io:format("available timeout ~w ~n",[?rec_info(bs_state,State)]),
    case whereis(lr) of
        undefined -> io:format("~p: lr is not started ~n", [State#bs_state.name]),
            {next_state, full, State,5000};
        _Pid -> case lr:who_are_at(State#bs_state.name) of
                    none -> io:format("~p: noone in bs, going to available state: none ~n", [State#bs_state.name]),
                        {next_state, available, State,5000};
                    {ok,PhoneList} -> io:format("~p: who are at ~p ~n", [State#bs_state.name,PhoneList]),
                        case length(PhoneList) < ?MAX_CAPACITY  of
                            true -> [poll(Elem,State#bs_state.poll_db) || Elem <- PhoneList],
                                io:format("going to available state: ~p ~n", [State#bs_state.poll_db]),
                                {next_state, available, State#bs_state{current_capacity = length(PhoneList)},5000};
                            false ->  io:format("keeping in full state: ~p ~n", [State#bs_state.poll_db]),
                                [poll(Elem,State#bs_state.poll_db) || Elem <- PhoneList],
                                {next_state, full, State#bs_state{current_capacity = length(PhoneList)},5000}
                        end
                end
    end.


full(info, _From, State=#bs_state{}) ->
    io:format("Info: ~w ~n", [?rec_info(bs_state,State)]),
    {reply, ok, full, State, 5000};
full({connect, MSPid, MSName}, _From, State=#bs_state{}) ->
    notice(State, "full, rejecting phone MS ~p ~p", [MSPid, MSName]),
    phone:respond(reject,MSName),
    {next_state, full, State, 5000};
full(timeout, From, State=#bs_state{}) ->
    {next_state, get_ack, State#bs_state{mspid = nopid, msname = noname, from=From}, 5000};
full(Event, _From, State=#bs_state{}) ->
    unexpected(Event, available),
    {next_state, available, State, 5000}.


%%state_name(_Event, State = #bs_state{}) ->
%%    {next_state, state_name, State}.
%%
%%state_name(_Event, _From, State = #bs_state{}) ->
%%    Reply = ok,
%%    {reply, Reply, state_name, State}.






handle_event(_Event, StateName, State = #bs_state{}) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State = #bs_state{}) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State = #bs_state{}) ->
    {next_state, StateName, State}.

terminate(Reason, _StateName, _State = #bs_state{}) ->
    io:format("Terminated with Reason: ~p~n", [Reason]),
    ok.

code_change(_OldVsn, StateName, State = #bs_state{}, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Allows to log unexpected messages.
unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n",
        [self(), Msg, State]).

notice(#bs_state{name=N}, Str, Args) ->
    io:format("~s: "++Str++"~n", [N|Args]).