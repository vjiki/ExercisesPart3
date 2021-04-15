%%%-------------------------------------------------------------------
%%% @author golubkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(lrm).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(lrm_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #lrm_state{}}.

handle_call(_Request, _From, State = #lrm_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #lrm_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #lrm_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #lrm_state{}) ->
    ok.

code_change(_OldVsn, State = #lrm_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
