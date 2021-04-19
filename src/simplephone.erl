%%%-------------------------------------------------------------------
%%% @author golubkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. апр. 2021 15:23
%%%-------------------------------------------------------------------
-module(simplephone).
-author("golubkin").

%% API
-export([respond/2]).

-import(bs,[connect/3,ack/2]).


respond(connect,_PhonePid) ->
    erlang:error(not_implemented);
respond(reject,_PhonePid) ->
    erlang:error(not_implemented);
respond(busy,_PhonePid) ->
    erlang:error(not_implemented).
