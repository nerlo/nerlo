%% @doc server
%% @author Ingo Schramm

-module(neo4j_srv).
-behaviour(gen_server).

% public interface
%-export([]).
-export([start/0, start/1, start_link/0, start_link/1, stop/0]).

% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-author("Ingo Schramm").

-include("global.hrl").
-include("ej.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_N, 0).
-define(SRVNAME, ?MODULE).
-define(STARTSPEC, {local, ?SRVNAME}).

-record(neo4j, {workers  = []
                ,worker  = no
                ,n       = 0
                ,db      = false
                }).

%% ------ PUBLIC -----

start() ->
    start(?DEFAULT_N).

start(N) ->
    gen_server:start(?STARTSPEC, ?MODULE, #neo4j{n=N}, []).

start_link() ->
    start_link(?DEFAULT_N). 
    
start_link(N) ->
    gen_server:start_link(?STARTSPEC, ?MODULE, #neo4j{n=N}, []).

stop() ->
    gen_server:cast(?SRVNAME, {'STOP'}).


%% ------ GENERIC -----


% @hidden    
init(S) ->
    S1 =
    case S#neo4j.worker of
        yes -> S;
        no  -> initialize(S)
    end,
    ej_log:info("~w initialized with state ~w", [?MODULE, S1]),
    {ok,S1}.

% @hidden     
handle_call(Msg,From,S) ->
    ej_log:warn("Cannot understand call from ~w: ~w", [From,Msg]),
    {reply, {error, unknown_msg}, S}.

% @hidden
handle_cast({'STOP'}, S) ->
    case S#neo4j.worker of
        yes -> nop;
        no  -> shutdown(S)
    end,
    ej_log:info("stopping with state: ~w", [S]),
    {stop, normal, S};
handle_cast(Msg,S) ->
    ej_log:info("cannot handle cast: ~w", [Msg]),
    {noreply, S}.

% @hidden
handle_info(Msg={From, ej_notify, start}, S) ->
    ej_log:debug("got notification: ~p", [Msg]),
    timer:sleep(100),
    neo4j:start(),
    {noreply, S};
handle_info(Msg={From, ej_notify, stop}, S) ->
    ej_log:debug("got notification: ~p", [Msg]),
    erlang:send_after(50, self(), {'STOP'}),
    {noreply, S};
handle_info({'STOP'}, S) ->
    case S#neo4j.worker of
        yes -> nop;
        no  -> shutdown(S)
    end,
    ej_log:info("stopping with state: ~w", [S]),
    {stop, normal, S};
handle_info(Msg,S) ->
    ej_log:info("info: ~p", [Msg]),
    {noreply,S}.

% @hidden     
terminate(_Reason,S) ->
    {noreply, S}.

% @hidden     
code_change(_OldVsn, S, _Extra) -> 
    {ok, S}.


%% ------ PRIVATE -----

start_worker(S) ->
    gen_server:start(?MODULE, S#neo4j{worker=yes}, []).

initialize(S) ->
    S1 =
    case catch(neo4j:start()) of
        {'EXIT',Reason} -> 
            ej_log:error("starting neo4j failed: ~w", [Reason]),
            S;
        Msg -> 
            ej_log:debug("~p", [Msg]),
            case neo4j:has_db() of
                true  -> 
                    S#neo4j{db=true};
                false -> 
                    ej_log:error("no database available", []),
                    erlang:send_after(100, self(), {'STOP'}),
                    S#neo4j{db=false}
            end
    end,
    ej_srv:add_listener(self()),
    Workers =
        lists:foldl(fun(_I,Acc) -> 
                            case start_worker(S1) of
                                {ok,Pid} -> [Pid|Acc];
                                _Any     -> Acc
                            end
                    end, [], lists:seq(1,S1#neo4j.n)),
    S1#neo4j{workers=Workers}.

shutdown(S) ->
    lists:map(fun(W) -> W ! {'STOP'} end, S#neo4j.workers),
    neo4j:stop().

%% ------ TESTS ------

%% start_stop_test() ->
%%     {ok,Pid} = start(),
%%     timer:sleep(500),
%%     ?assert(is_pid(Pid)),
%%     stop(),
%%     timer:sleep(500).






  
