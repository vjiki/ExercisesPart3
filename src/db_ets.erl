%%%-------------------------------------------------------------------
%%% @author golubkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. апр. 2021 14:11
%%%-------------------------------------------------------------------
-module(db_ets).
-author("golubkin").
-include_lib("db_record.hrl").

%% API
-export([db_init/0,db_close/1]).
-export([db_delete/2,db_get/2,db_put/3]).
-export([db_query/2,db_empty/1]).


db_init() -> {ok, ets:new(my_ets, [set,protected,{keypos,#element.key}])}.

%% or {error,Reason}
db_close(_DbInfo) ->
    %%io:format("~p closing Db ~n", [DbInfo]),
    %%ets:delete_all_objects(DbInfo),
    ok.

db_put(Key, Data, DbInfo) -> ets:insert(DbInfo,  #element{key=Key, data=Data}),
    %%io:format("db_ets: ~p ~n", [DbInfo]),
    {ok, DbInfo}.

db_get(Key, DbInfo) ->
    Data = lists:append(ets:match(DbInfo, #element{key=Key, data='$2'})),
    case Data == [] of
        true -> undefined;
        false -> {ok, Data}
    end.

db_delete(Key, DbInfo) ->
    ets:delete(DbInfo, Key),
    {ok, DbInfo}.

db_query(Data, Dblnfo) ->
    KeyList = lists:append(ets:match(Dblnfo, #element{key='$1', data = Data})),
    case KeyList == [] of
        false -> {ok, KeyList};
        true -> undefined
    end.

db_empty(_Dblnfo) -> {ok, ets:new(my_ets, [set,protected,{keypos,#element.key}])}.