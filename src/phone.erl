%%%-------------------------------------------------------------------
%%% @author golubkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. апр. 2021 13:27
%%%-------------------------------------------------------------------
-module(phone).
-author("golubkin").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

%%-compile(export_all).
%% added API
-export([respond/2,stop/1,location/2,connect/1,info/1]).

-define(SERVER, ?MODULE).

-record(phone_state, {ms_pid,ms_name,ms_state=on,status=disconnected,bs_name=undefined}).

-define(rec_info(T,R),lists:zip(record_info(fields,T),tl(tuple_to_list(R)))).


%%%===================================================================
%%% API
%%%===================================================================
start_link(MSName) ->
    %%io:format("~p starting ~n", [MSName]),
    gen_server:start_link({local, MSName}, ?MODULE, MSName, []).

respond(Request, MSPid) -> gen_server:call(MSPid, {respond, Request}).
location(BSName, MSPid) -> gen_server:call(MSPid, {location,BSName}).
connect(MSPid) -> gen_server:call(MSPid, connect).
info(MSPid) -> gen_server:call(MSPid, info).


stop(MSPid) ->
    gen_server:call(MSPid, stop),
    gen_server:stop(MSPid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(MSName) -> {ok,#phone_state{ms_pid=self(),ms_name=MSName}}.

%%%===================================================================
%%% handle_call
%%%===================================================================

handle_call({respond,connect}, _From, State=#phone_state{}) ->
    bs:ack(State#phone_state.bs_name,State#phone_state.ms_name),
    %%io:format("~p: Ack ~n", [State#phone_state.bs_name]),
    {reply, ok, State#phone_state{status = connected}};
handle_call({respond,reject}, _From, State=#phone_state{}) ->
    %%io:format("not connected, rejected ~n"),
    {reply, reject, State#phone_state{status = disconnected}};
handle_call({respond,busy}, _From, State=#phone_state{}) ->
    %%io:format("not connected, bs is busy ~n"),
    {reply, busy, State#phone_state{status = disconnected}};

handle_call({location,BSName}, _From, State=#phone_state{}) ->
    Reply = ok,
    {reply, Reply, State#phone_state{bs_name = BSName}};

handle_call(connect, _From, State = #phone_state{}) ->
    case State#phone_state.bs_name of
        undefined -> {reply, {error, undefined_bs_name}, State};
        BSName -> case erlang:whereis(BSName) of
                      undefined ->
                          %%io:format("~p: BS is not started ~n", [BSName]),
                          {reply, {error, bs_is_not_started, BSName}, State};
                      _Pid ->
                          %%io:format("~p: Connecting ~n", [BSName]),
                          bs:connect(BSName, State#phone_state.ms_pid, State#phone_state.ms_name),
                          {reply, ok, State}
                  end
    end;

handle_call(info, _From, State=#phone_state{}) ->
    io:format("Info: ~w ~n",
        [?rec_info(phone_state,State)]),
    {reply, ok, State};

handle_call(stop, _From, State=#phone_state{}) ->
    Reply = ok,
    {reply, Reply, State#phone_state{ms_state=off}};

handle_call(Request, _From, State=#phone_state{}) ->
    Reply = {error,unknown_request,Request},
    {reply, Reply, State}.

%%%===================================================================
%%%
%%%===================================================================

handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
