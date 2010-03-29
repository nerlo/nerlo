%% @doc This is the Erlang side of the graph database.
%% 
%% @author Ingo Schramm

-module(neo4j).

% public interface
-export([start/0
        ,stop/0
        ,has_db/0
        ,add_vertex/0
        ,del_vertex/1
        ,add_undirected_edge/2
        ,add_undirected_edge/3
        ,add_directed_edge/2
        ,add_directed_edge/3
        ,del_edge/1
        ,vertex_get_edges/1
        ,vertex_set_property/3
        ,vertex_del_property/2
        ,vertex_get_property/2
        ,vertex_get_properties/1
        ,edge_get_adjacent_vertices/1
        ,edge_is_directed/1
        ,edge_set_property/3
        ,edge_del_property/2
        ,edge_get_property/2
        ,edge_get_properties/1
        ,traverse/6
        ]).

-author("Ingo Schramm").

-include("global.hrl").
-include("ej.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(HANDLER, {handler,graphdb}).
-define(VERTEX(Id), {vertex,Id}).
-define(EDGE(Id,Type,A,B,Dir), {edge,Id,Type,A,B,Dir}).

% @doc Start the database.
start() ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,init}]) of
        {ok,[{result,true}]} -> ok;
        _Any                 -> error
    end.

% @doc Stop the database.
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
        _Error -> false
    end.

% @doc Add a vertex.
add_vertex() ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,add_vertex}]) of
        {ok, Data} -> 
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_id};
                {result,Value} -> ?VERTEX(Value)
            end;
        Error -> Error
    end.

% @doc Delete a vertex.
del_vertex(?VERTEX(Id)) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,del_vertex},{id,Id}]) of
        {ok, Data} -> 
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_id};
                {result,Value} -> Value
            end;
        Error -> Error
    end.

% @doc Get the list of edges of a given vertex.
vertex_get_edges(?VERTEX(Id)) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,vertex_get_edges},{id,Id}]) of
        {ok, Data} -> 
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_content};
                {result,Value} -> Value
            end;
        Error -> Error
    end.

% vertex_map_edges(?VERTEX(Id))
% iterate somehow ...

vertex_set_property(?VERTEX(Id), Key, Val) ->
    not_implemented.

vertex_del_property(?VERTEX(Id), Key) ->
    not_implemented.

vertex_get_property(?VERTEX(Id), Key) ->
    not_implemented.

vertex_get_properties(?VERTEX(Id)) ->
    not_implemented.

% @doc Add an undirected edge.
% This will actually add two edges, one in each direction.
add_undirected_edge(Va=?VERTEX(_A), Vb=?VERTEX(_B), Type) ->
    private_add_edge(Va, Vb, Type, false).

% @doc Add an undirected edge with type EDGE.
% This will actually add two edges, one in each direction.
add_undirected_edge(Va=?VERTEX(_A), Vb=?VERTEX(_B)) ->
    private_add_edge(Va, Vb, 'EDGE', false).

% @doc Add a directed edge.
add_directed_edge(Va=?VERTEX(_A), Vb=?VERTEX(_B), Type) ->
    private_add_edge(Va, Vb, Type, true).

% @doc Add a directed edge with type EDGE.
add_directed_edge(Va=?VERTEX(_A), Vb=?VERTEX(_B)) ->
    private_add_edge(Va, Vb, 'EDGE', true).

private_add_edge(?VERTEX(A), ?VERTEX(B), Type, Dir) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,add_edge},{a,A},{b,B},{type,Type},{dir,Dir}]) of
        {ok, Data} -> 
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_id};
                {result,Value} -> ?EDGE(Value,Type,A,B,Dir)
            end;
        Error -> Error
    end.

% @doc Delete an edge.
del_edge(?EDGE(Id,_Type,_A,_B,_Dir)) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,del_edge},{id,Id}]) of
        {ok, Data} -> 
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_id};
                {result,Value} -> Value
            end;
        Error -> Error
    end.

edge_get_adjacent_vertices(?EDGE(_Id,_Type,A,B,_Dir)) ->
    {?VERTEX(A),?VERTEX(B)}.

edge_is_directed(?EDGE(_Id,_Type,_A,_B,Dir)) ->
    Dir.

edge_set_property(?VERTEX(Id), Key, Val) ->
    not_implemented.

edge_del_property(?VERTEX(Id), Key) ->
    not_implemented.

edge_get_property(?VERTEX(Id), Key) ->
    not_implemented.

edge_get_properties(?VERTEX(Id)) ->
    not_implemented.

traverse(?VERTEX(Id), Order, Stop, Return, RelType, Dir) ->
    not_implemented.


