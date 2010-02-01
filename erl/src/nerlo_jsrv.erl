%% @doc This is the  Erlang server maintaining connections
%% to the hidden java node.
%%
%% @author Ingo Schramm

-module(nerlo_jsrv).
-behaviour(gen_server).

% public interface
-export([job/1]).
-export([start/0, start/1, start_link/0, start_link/1, stop/0]).

% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-author("Ingo Schramm").

-include("global.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_N, erlang:system_info(schedulers_online) * 2).
-define(SRVNAME, ?MODULE).
-define(STARTSPEC, {local, ?SRVNAME}).


-record(jsrv, {workers = []
              ,worker  = no
              ,n       = 0
              ,peer    = null
              }).

start() ->
    start(?DEFAULT_N).

start(N) ->
    gen_server:start(?STARTSPEC, ?MODULE, #jsrv{n=N}, []).


start_link() ->
    start_link(?DEFAULT_N). 
    
start_link(N) ->
    gen_server:start_link(?STARTSPEC, ?MODULE, #jsrv{n=N}, []).
    

stop() ->
    gen_server:cast(?SRVNAME, {'STOP'}).
    
job(Spec) ->
    gen_server:call(?SRVNAME, {job, Spec}).   

   
% @hidden    
init(S) ->
    S1 =
    case S#jsrv.worker of
        yes -> ok;
        no  ->
            Peer    = handshake(),
            S2 = S#jsrv{peer=Peer},
            Workers =
            lists:foldl(fun(_I,Acc) -> 
                            case start_worker(S2) of
                                {ok,Pid} -> [Pid|Acc];
                                _Any     -> Acc
                            end
                        end, [], lists:seq(1,S#jsrv.n)),
            S#jsrv{workers=Workers}
    end,
    log:info(self(), "~p initialized with state ~w", [?MODULE, S1]),
    {ok,S1}.

% @hidden     
handle_call({job,Spec},From,S) ->
    {W, L} = f:lrot(S#jsrv.workers),
    gen_server:cast(W,{job,From,Spec}),
    {noreply, S#jsrv{workers=L}};
handle_call(Msg,From,S) ->
    log:warn(self(), "Cannot understand call from ~p: ~p", [From,Msg]),
    {reply, {error, unknown_msg}, S}.

% @hidden
handle_cast({job,From,Spec}, S) ->
    Result = do_job(Spec),
    gen_server:reply(From, Result),
    {noreply, S};
handle_cast({'STOP'}, S) ->
    case S#jsrv.worker of
        yes -> nop;
        no  -> lists:map(fun(W) -> W ! {'STOP'} end, S#jsrv.workers)
    end,
    log:info(self(),"stopping with state: ~w", [S]),
    {stop, normal, S};    
handle_cast(_Msg,S) ->
    {noreply, S}.

% @hidden     
handle_info(_Msg,S) ->
    {noreply,S}.

% @hidden     
terminate(_Reason,S) ->
    {noreply, S}.

% @hidden     
code_change(_OldVsn, S, _Extra) -> 
    {ok, S}.


%% ------ PRIVATE PARTS -----

% TODO start JNode and shake hands
handshake() ->
    undef.

do_job(Spec) ->
    {self(),Spec}.

start_worker(S) ->
    gen_server:start(?MODULE, S#jsrv{worker=yes}, []).
    

   
%% ------ TESTS ------

start_stop_test() ->
    ?assertMatch({ok,_Pid}, start()),
    stop().

job_test() ->
    start(),
    Spec = test,
    {Pid1,Spec} = job(Spec),
    {Pid2,Spec} = job(Spec),
    ?assertNot(Pid1 =:= Pid2),
    stop().





  
