%%%-------------------------------------------------------------------
%%% @author golubkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(bs).

-behaviour(gen_fsm).

-import(db_ets, [db_init/0,db_close/1,db_delete/2,db_get/2,db_put/3,db_empty/1,db_query/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
    handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% public API
-export([start_link/1,stop/1,connect/3,ack/2]).
-export([unexpected/2]).

%% custom state names
-export([available/2,
        available/3,
        get_ack/2,
        get_ack/3,
        full/3,
        full/2,
        poll/2,
        info/1]).

%% Const values
-define(SERVER, ?MODULE).
-define(POLL_INTERVAL, 1000). %% One second
-define(MAX_CAPACITY, 3). %% BS can handle max 3 mobile phones
-define(rec_info(T,R),lists:zip(record_info(fields,T),tl(tuple_to_list(R)))).

-record(bs_state, {name,ms_pid,ms_name,current_capacity=0,timer_ref}).



%%%===================================================================
%%% PUBLIC API
%%%===================================================================

%%Start a base station process with the specified name. [Called by op/bsc]
start_link(BSName) ->
    %%io:format("~p starting ~n", [BSName]),
    gen_fsm:start_link({local, BSName}, ?MODULE, BSName, []).

%%Stop a specific base station. [Called by op/bsc]
stop(BSPid) ->
    gen_fsm:stop(BSPid).

%%Connect to a specific base station. [Called by a phone]
%%gen_fsm:sync_send_event(BSPid, {connect, MSPid, MSName}, 50000). -- sync call
connect(BSPid, MSPid, MSName) ->
    gen_fsm:send_event(BSPid, {connect, MSPid, MSName}).

%%Acknowledge a connection to a specific base station. [Called by a phone].
ack(BSPid,MSName) ->
    gen_fsm:send_event(BSPid, {acknowledgement,MSName}).

%% Function to check the state of BS State Machine
info(BSPid) -> gen_fsm:sync_send_event(BSPid, info,5000).

%%%===================================================================
%%% The state machine
%%%===================================================================

%% INIT
init(BSName) ->
    {ok, PollDB} = db_init(),
    case timer:send_interval(?POLL_INTERVAL, self(), {trigger, PollDB}) of
        {ok, TRef} -> {ok, available, #bs_state{name=BSName,timer_ref=TRef}};
        {error, _Reason} -> io:format("~p: error timer not started ~n", [BSName]),
            bs:stop(BSName)
    end.


%% AVAILABLE
available(info, _From, State=#bs_state{}) ->
    io:format("Info: State available: ~w ~n", [?rec_info(bs_state,State)]),
    {reply, ok, available, State}.

available({connect, MSPid, MSName}, State=#bs_state{}) ->
    %%io:format("~p connect ~n", [MSName]),
    case State#bs_state.current_capacity < ?MAX_CAPACITY of
        true -> phone:respond(connect,MSName),
            %%Ref = monitor(process, MSPid),
            {next_state, get_ack, State#bs_state{ms_pid = MSPid, ms_name = MSName}};
        false -> phone:respond(reject,MSName),
            {next_state, available, State}
    end;
available(timeout, State=#bs_state{}) ->
    io:format("available timeout ~w ~n",[?rec_info(bs_state,State)]),
    {next_state, available, State};
available(Event, State=#bs_state{}) ->
    unexpected(Event, available),
    {next_state, available, State}.


%% GET ACK
get_ack(info, _From, State=#bs_state{}) ->
    io:format("Info: State get_ack: ~w ~n", [?rec_info(bs_state,State)]),
    {reply, ok, get_ack, State}.

get_ack({acknowledgement, MSName}, State = #bs_state{}) ->
    lr:located_at(MSName, State#bs_state.name),
    {next_state, available, State};
get_ack(timeout, State=#bs_state{}) ->
    io:format("get_ack timeout ~w ~n",[?rec_info(bs_state,State)]),
    {next_state, available, State};
get_ack(Event, State) ->
    io:format("~p unknown state ~n", [Event]),
    unexpected(Event, acknowledgement),
    {next_state, acknowledgement, State}.

%% FULL
full(timeout, State=#bs_state{}) ->
    io:format("full timeout ~w ~n",[?rec_info(bs_state,State)]),
    {next_state, full, State}.

full(info, _From, State=#bs_state{}) ->
    io:format("Info: State full: ~w ~n", [?rec_info(bs_state,State)]),
    {reply, ok, full, State};
full({connect, _MSPid, MSName}, _From, State=#bs_state{}) ->
    phone:respond(reject,MSName),
    {next_state, full, State};
full(timeout, _From, State=#bs_state{}) ->
    {next_state, get_ack, State#bs_state{ms_pid = nopid, ms_name = noname}};
full(Event, _From, State=#bs_state{}) ->
    unexpected(Event, available),
    {next_state, available, State}.

handle_event(_Event, StateName, State = #bs_state{}) ->
    {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State = #bs_state{}) ->
    Reply = ok,
    {reply, Reply, StateName, State}.


poll(MSName, PollDB) ->
    case db_get(MSName,PollDB) of
        undefined  -> case erlang:whereis(MSName) of
                          undefined ->
                              db_put(MSName, 1, PollDB);
                          _Pid ->
                              db_put(MSName, 0, PollDB)
                      end;
        {ok, Attempt} -> case lists:last(Attempt) < 5 of
                             true -> %%io:format("~p: ~p: attempt ~n", [MSName,Attempt]),
                                 case erlang:whereis(MSName) of
                                     undefined ->
                                         %%io:format("~p: phone is not started ~n", [MSName]),
                                         db_put(MSName, lists:last(Attempt) + 1, PollDB);
                                     _Pid ->
                                         db_put(MSName, 0, PollDB)
                                 end;
                             false -> %%io:format("~p: ~p: MS is lost ~n", [MSName, Attempt]),
                                 lr:lost(MSName),
                                 db_delete(MSName, PollDB)
                         end;
        Other -> io:format("~p ~p: unexpected msg ~n", [MSName,Other])
    end.


handle_info({trigger, PollDB}, StateName, State = #bs_state{}) ->
    %%io:format("Info: ~p ~w ~n", [StateName,?rec_info(bs_state,State)]),
    case whereis(lr) of
        undefined -> io:format("~p: lr is not started ~n", [State#bs_state.name]),
            {next_state, StateName, State};
        _Pid -> case lr:who_are_at(State#bs_state.name) of
                    none ->
                        case StateName of
                            full -> %%io:format("~p: none in bs, going to available state: none ~n", [State#bs_state.name]),
                                {next_state, available, State};
                            _Other -> %%io:format("~p: no one in bs, keeping in current state ~n", [State#bs_state.name]),
                                {next_state, StateName, State}
                        end;
                    {ok,PhoneList} -> %%io:format("~p: who are at ~p ~n", [State#bs_state.name,PhoneList]),
                        case length(PhoneList) < ?MAX_CAPACITY  of
                            true -> [poll(Elem,PollDB) || Elem <- PhoneList],
                                case StateName of
                                    full -> %%io:format("going to available state: ~p ~n", [PollDB]),
                                        {next_state, available, State#bs_state{current_capacity = length(PhoneList)}};
                                    _Other -> %%io:format("~p: keeping in current state ~n", [State#bs_state.name]),
                                        {next_state, StateName,  State#bs_state{current_capacity = length(PhoneList)}}
                                end;
                            false ->
                                case StateName of
                                    full -> %%io:format("keeping in full state: ~p ~n", [PollDB]),
                                        [poll(Elem,PollDB) || Elem <- PhoneList],
                                        {next_state, full, State#bs_state{current_capacity = length(PhoneList)}};
                                    _Other -> %%io:format("going to full state: ~p ~n", [PollDB]),
                                        [poll(Elem,PollDB) || Elem <- PhoneList],
                                        {next_state, full,  State#bs_state{current_capacity = length(PhoneList)}}
                                end
                        end
                end
    end;
%%    send_after(?POLL_INTERVAL, self(), {trigger, Count}),
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