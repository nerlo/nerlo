%% @doc This is the Erlang side of the graph database.
%% 
%% @author Ingo Schramm

-module(nerlo_gdb).

% public interface
-export([start/0, stop/0, has_db/0, create_node/0, delete_node/1]).

-author("Ingo Schramm").

-include("global.hrl").
-include("ej.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(HANDLER, {handler,graphdb}).
-define(NODE(Id), {node,Id}).

% @doc Start the database.
start() ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,init}]) of
        {ok,[{result,true}]} -> ok;
        _Any                 -> error
    end.

stop() ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,stop}]) of
        {ok,[{result,ok}]} -> ok;
        _Any               -> error
    end.

% @doc Test whether a graph database is running.
has_db() ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,has_db}]) of
        {ok, Data} -> 
            case lists:keyfind(result,1,Data) of
                false          -> false;
                {result,Value} -> Value
            end;
        Error -> false
    end.
        
create_node() ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,create_node}]) of
        {ok, Data} -> 
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_id};
                {result,Value} -> ?NODE(Value)
            end;
        Error -> Error
    end.

delete_node(?NODE(Id)) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,delete_node},{id,Id}]) of
        {ok, Data} -> 
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_id};
                {result,Value} -> Value
            end;
        Error -> Error
    end.    




