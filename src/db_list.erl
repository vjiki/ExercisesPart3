%%%-------------------------------------------------------------------
%%% @author golubkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. апр. 2021 12:45
%%%-------------------------------------------------------------------
-module(db_list).
-author("golubkin").

-include_lib("db_record.hrl").

%% API
-export([db_init/0,db_close/1]).
-export([db_delete/2,db_get/2,db_put/3]).
-export([db_query/2,db_empty/1]).

db_init() -> {ok, []}.

%% or {error,Reason}
db_close(_DbInfo) ->
    %%io:format("~p closing Db ~n", [_DbInfo]),
    ok.

db_put(Key, Data, DbInfo) ->
    %%io:format("db_list: ~p ~n", [DbInfo]),
    {ok, DelDbInfo} = db_delete(Key, DbInfo),
    NewDbInfo = lists:append(DelDbInfo, [#element{key=Key, data=Data}]),
    {ok,NewDbInfo}.

db_get(Key, DbInfo) ->
    case lists:keyfind(Key, #element.key, DbInfo) of
        #element{data = Data} -> {ok, Data};
        false ->  undefined;
        _Other -> io:format("other ~p  ~n", [_Other])
    end.

db_delete(Key, DbInfo) ->
    NewDbInfo = lists:keydelete(Key,#element.key,DbInfo),
    {ok, NewDbInfo}.


db_query(Data, Dblnfo) ->
    KeyList = [Key || #element{key = Key, data = DataDbInfo} <- Dblnfo, DataDbInfo == Data],
    case KeyList == [] of
        false -> {ok, KeyList};
        true -> undefined
    end.

db_empty(_Dblnfo) -> {ok, []}.
