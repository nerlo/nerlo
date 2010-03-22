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
    log:info(self(), "~w initialized with state ~w", [?MODULE, S1]),
    {ok,S1}.

% @hidden     
handle_call(Msg,From,S) ->
    log:warn(self(), "Cannot understand call from ~w: ~w", [From,Msg]),
    {reply, {error, unknown_msg}, S}.

% @hidden
handle_cast({'STOP'}, S) ->
    case S#neo4j.worker of
        yes -> nop;
        no  -> lists:map(fun(W) -> W ! {'STOP'} end, S#neo4j.workers)
    end,
    log:info(self(),"stopping with state: ~w", [S]),
    {stop, normal, S};
handle_cast(Msg,S) ->
    log:info(self(),"cannot handle cast: ~w", [Msg]),
    {noreply, S}.

% @hidden
handle_info(Msg,S) ->
    log:info(self(),"info: ~p", [Msg]),
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
            log:error(self(), "starting neo4 failed: ~w", [Reason]),
            S;
        Msg           -> 
            log:debug(self(), "~p", [Msg]),
            S#neo4j{db=true}
    end,
    Workers =
        lists:foldl(fun(_I,Acc) -> 
                            case start_worker(S1) of
                                {ok,Pid} -> [Pid|Acc];
                                _Any     -> Acc
                            end
                    end, [], lists:seq(1,S1#neo4j.n)),
    S1#neo4j{workers=Workers}.

%% ------ TESTS ------

%% start_stop_test() ->
%%     {ok,Pid} = start(),
%%     timer:sleep(500),
%%     ?assert(is_pid(Pid)),
%%     stop(),
%%     timer:sleep(500).






  
