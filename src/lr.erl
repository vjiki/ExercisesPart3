%%%-------------------------------------------------------------------
%%% @author golubkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. апр. 2021 18:28
%%%-------------------------------------------------------------------
-module(lr).
-author("golubkin").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

%%-compile(export_all).
%% added API
-export([located_at/2,where_is/1,lost/1,stop/0]).
-export([who_are_at/1,empty/0]).
-export([new_backend/1]).

-define(SERVER, ?MODULE).

%%-record(lr_state, {db=$1,dbMod=$2}).

-import(db_ets, [db_init/0,db_close/1,db_delete/2,db_get/2,db_put/3,db_empty/1,db_query/2]).


%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

located_at(MS, BS) -> gen_server:call(?MODULE, {located_at, MS, BS}).
lost(MS) -> gen_server:call(?MODULE, {lost, MS}).
where_is(MS) -> gen_server:call(?MODULE, {where_is, MS}).
who_are_at(BS) -> gen_server:call(?MODULE, {who_are_at, BS}).
empty() -> gen_server:call(?MODULE, empty).


stop() -> %terminate(normal, endState),
    gen_server:call(?MODULE, stop),
    gen_server:stop(?MODULE).

new_backend(NewDbMod) ->
    %%      Instruction how to
    %%      1. Change code of LR
    %%      2. compile:file(lr).
    %%      3. sys:suspend(lr).
    %%      4. call new_backend()
    %%      5. sys:resume(?MODULE).
    %%sys:change_code(?MODULE, NewDbMod, db_list, []),
    io:format("going to change code to: ~p ~n", [NewDbMod]),
    sys:change_code(?MODULE, ?MODULE, [], []),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) -> db_init().

%%%===================================================================
%%% handle_call
%%%===================================================================

handle_call({where_is,MS}, _From, DbInfo) ->
    Reply = case db_get(MS,DbInfo) of
                undefined  -> lost;
                {ok,BS} -> {ok,BS}
            end,
    {reply, Reply, DbInfo};

handle_call({located_at,MS,BS}, _From, DbInfo) ->
    {ok, NewDbInfo} = db_put(MS, BS, DbInfo),
    Reply = ok,
    {reply, Reply, NewDbInfo};

handle_call({lost,MS}, _From, DbInfo) ->
    {ok,NewDbInfo} = db_delete(MS,DbInfo),
    Reply = ok,
    {reply, Reply, NewDbInfo};

handle_call({who_are_at,BS}, _From, DbInfo) ->
    Reply = case db_query(BS,DbInfo) of
                undefined -> none;
                {ok,MSList} -> {ok,MSList}
            end,
    {reply, Reply, DbInfo};

handle_call(empty, _From, DbInfo) ->
    {ok,NewDbInfo} = db_empty(DbInfo),
    Reply = ok,
    {reply, Reply, NewDbInfo};

handle_call(stop, _From, DbInfo) ->
    db_close(DbInfo),
    {reply, ok, []}.

%%%===================================================================
%%%
%%%===================================================================

handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, _State, _Extra) ->
    %%db_empty(State),
    {ok, NewState} = db_init(),
    io:format("Changed code!~n"),
    {ok, NewState}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


