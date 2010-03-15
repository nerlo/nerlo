%% @doc This is the Erlang side of the graph database.
%% 
%% @author Ingo Schramm

-module(nerlo_gdb).

% public interface
-export([start/0, has_db/0]).

-author("Ingo Schramm").

-include("global.hrl").
-include("ej.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(HANDLER, {handler,graphdb}).

% @doc Start the database.
start() ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,init}]) of
        {ok,[{result,true}]} -> ok;
        _Any                 -> error
    end.

% @doc Test whether a graph database is running.
has_db() ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,has_db}]) of
        {ok, Data} -> 
            case lists:keyfind(result,1,Data) of
                false -> false;
                {result,Value} -> Value
            end;
        _Any -> error
    end.
        

