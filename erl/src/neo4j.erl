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
        ,add_edge/2
        ,add_edge/3
        ,del_edge/1
        ,vertex_get_edges/1
        ,vertex_set_property/3
        ,vertex_del_property/2
        ,vertex_get_property/2
        ,vertex_get_properties/1
        ,edge_get_adjacent_vertices/1
        ,edge_get_start_vertex/1
        ,edge_get_end_vertex/1
        ,edge_get_type/1
        ,edge_set_property/3
        ,edge_del_property/2
        ,edge_get_property/2
        ,edge_get_properties/1
        ,traverse/6
        ,order/0
        ,size/0
        ]).

-author("Ingo Schramm").

-include("global.hrl").
-include("ej.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(HANDLER, {handler,graphdb}).
-define(VERTEX(Id), {vertex,Id}).
-define(EDGE(Id,Type,A,B), {edge,Id,Type,A,B}).

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

% @doc Set a property at a vertex.
vertex_set_property(?VERTEX(Id), Key, Val) ->
    private_set_property(vertex, Id, Key, Val).

vertex_del_property(?VERTEX(Id), Key) ->
    private_del_property(vertex, Id, Key).

vertex_get_property(?VERTEX(Id), Key) ->
    private_get_property(vertex, Id, Key).

vertex_get_properties(?VERTEX(Id)) ->
    private_get_properties(vertex, Id).

% @doc Add an undirected edge.
% This will actually add two edges, one in each direction.
add_edge(Va=?VERTEX(_A), Vb=?VERTEX(_B), Type) ->
    private_add_edge(Va, Vb, Type).

% @doc Add an undirected edge with type EDGE.
% This will actually add two edges, one in each direction.
add_edge(Va=?VERTEX(_A), Vb=?VERTEX(_B)) ->
    private_add_edge(Va, Vb, 'EDGE').

% @doc Delete an edge.
del_edge(?EDGE(Id,_Type,_A,_B)) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,del_edge},{id,Id}]) of
        {ok, Data} -> 
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_id};
                {result,Value} -> Value
            end;
        Error -> Error
    end.

% @doc Get the adjacent vertices connected by an edge.
edge_get_adjacent_vertices(?EDGE(_Id,_Type,A,B)) ->
    {?VERTEX(A),?VERTEX(B)}.

edge_get_start_vertex(Edge) ->
    {Start,_End} = edge_get_adjacent_vertices(Edge),
    Start.

edge_get_end_vertex(Edge) ->
    {_Start,End} = edge_get_adjacent_vertices(Edge),
    End.

edge_get_type(?EDGE(_Id,Type,_A,_B)) ->
    Type.

edge_set_property(?EDGE(Id,_Type,_A,_B), Key, Val) ->
    private_set_property(edge, Id, Key, Val).

edge_del_property(?EDGE(Id,_Type,_A,_B), Key) ->
    private_del_property(edge, Id, Key).

edge_get_property(?EDGE(Id,_Type,_A,_B), Key) ->
    private_get_property(edge, Id, Key).

edge_get_properties(?EDGE(Id,_Type,_A,_B)) ->
    private_get_properties(edge, Id).

traverse(?VERTEX(Id), Order, Stop, Return, RelType, Dir) ->
    not_implemented.

% @doc Determine the order of the graph, the number of vertices.
order() ->
    not_implemented.

% @doc Determine the size of the graph, the number of edges.
size() ->
    not_implemented.


%% ----- PRIVATE ------

private_add_edge(?VERTEX(A), ?VERTEX(B), Type) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,add_edge},{a,A},{b,B},{type,Type}]) of
        {ok, Data} -> 
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_id};
                {result,Value} -> ?EDGE(Value,Type,A,B)
            end;
        Error -> Error
    end.

private_set_property(Type, Id, Key, Val) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER
                                ,{call,set_property}
                                ,{type,Type}
                                ,{id,Id}
                                ,{key,Key}
                                ,{value,Val}]) of
        {ok, _} -> ok;
        Error   -> Error
    end.

private_del_property(Type, Id, Key) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER
                                ,{call,del_property}
                                ,{type,Type}
                                ,{id,Id}
                                ,{key,Key}]) of
        {ok, _} -> ok;
        Error   -> Error
    end.

private_get_property(Type, Id, Key) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER
                                ,{call,get_property}
                                ,{type,Type}
                                ,{id,Id}
                                ,{key,Key}]) of
        {ok, Data} -> 
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_result};
                {result,Value} -> Value
            end;
        Error   -> Error
    end.

private_get_properties(Type, Id) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER
                                ,{call,get_properties}
                                ,{type,Type}
                                ,{id,Id}]) of
        {ok, Data} -> 
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_result};
                {result,Value} -> Value
            end;
        Error   -> Error
    end.