%% @doc This is the Erlang server maintaining connections
%% to the hidden Java node.
%% 
%% This one is trapping exists. You should always use ej_srv:stop()
%% to shutdown the server and the linked Java node. If the Java 
%% node dies it will be restarted. 
%%
%% To use this module in your code you should include ej.hrl.
%%
%% <pre>
%% (shell@host)1> {ok,Pid} = ej_srv:start().
%% (shell@host)2> ej_srv:send(call, [call, job]).
%% (shell@host)3> ej_srv:stop().
%% </pre>
%% @author Ingo Schramm

-module(ej_srv).
-behaviour(gen_server).

% public interface
-export([send/2, call/2, call/3]).
-export([start/0, start/1, start/2, start_link/0, start_link/1, start_link/2, stop/0]).

% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-author("Ingo Schramm").

-include("global.hrl").
-include("ej.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(DEBUG).
-export([bad/0]).
-endif. 

-define(DEFAULT_N, erlang:system_info(schedulers_online) * 2).
-define(SRVNAME, ?MODULE).
-define(STARTSPEC, {local, ?SRVNAME}).
-define(PEERNAME, jnode).
-define(PEERSTR, atom_to_list(?PEERNAME)).
-define(BINDIR, ".").
-define(JNODEBIN, "jnode").

-record(jsrv, {workers  = []
              ,worker   = no
              ,n        = 0
              ,peer     = null
              ,bindir   = ?BINDIR
              ,stopping = false
              }).

start() ->
    start(?DEFAULT_N).

start(N) ->
    gen_server:start(?STARTSPEC, ?MODULE, #jsrv{n=N}, []).

start(N,Bindir) ->
    gen_server:start(?STARTSPEC, ?MODULE, #jsrv{n=N,bindir=Bindir}, []).

start_link() ->
    start_link(?DEFAULT_N). 
    
start_link(N) ->
    gen_server:start_link(?STARTSPEC, ?MODULE, #jsrv{n=N}, []).

start_link(N,Bindir) ->
    gen_server:start_link(?STARTSPEC, ?MODULE, #jsrv{n=N,bindir=Bindir}, []).

stop() ->
    gen_server:cast(?SRVNAME, {'STOP'}).

% @doc Send a message to the peer and return immediately.
send(Tag,Msg = [_|_]) ->
    Ref = ?EJMSGREF(self(),erlang:make_ref()),
    gen_server:call(?SRVNAME, {send, Ref, Tag, Msg}),
    ok.

% @doc Send a message to the peer and wait for an answer.
% This runs with a default timeout of 10 seconds.
call(Tag,Msg) ->
    call(Tag,Msg,10).       

% @doc Send a message to the peer and wait for an answer.
% After Timeout seconds {error,timeout} will be returned.
% Set Timeout to 'infinity' to wait forever.
call(Tag,Msg = [_|_],Timeout) ->
    Ref = ?EJMSGREF(self(),erlang:make_ref()),
    gen_server:call(?SRVNAME, {send, Ref, Tag, Msg}),
    receive
        {_From, Ref, Result} -> Result
    after
        Timeout * 1000 -> {error,timeout}
    end.

-ifdef(DEBUG).
bad() -> 
    gen_server:call(?SRVNAME, {bad}).
-endif.
   
% @hidden    
init(S) ->
    S1 =
    case S#jsrv.worker of
        yes -> S;
        no  ->
            process_flag(trap_exit, true),
            {ok,Cwd} = file:get_cwd(),
            timer:start(),
            Bindir = 
                if 
                    S#jsrv.bindir =:= ?BINDIR -> Cwd;
                    true                      -> S#jsrv.bindir
                end,
            Peer = handshake(Bindir),
            if
                is_pid(Peer) -> erlang:link(Peer);
                true         -> nop
            end,
            S2 = S#jsrv{peer=Peer},
            Workers =
            lists:foldl(fun(_I,Acc) -> 
                            case start_worker(S2) of
                                {ok,Pid} -> [Pid|Acc];
                                _Any     -> Acc
                            end
                        end, [], lists:seq(1,S#jsrv.n)),
            S2#jsrv{workers=Workers,bindir=Bindir}
    end,
    log:info(self(), "~p initialized with state ~p", [?MODULE, S1]),
    {ok,S1}.

% @hidden     
handle_call({job,Spec},From,S) ->
    {W, L} = f:lrot(S#jsrv.workers),
    gen_server:cast(W,{job,From,Spec}),
    {noreply, S#jsrv{workers=L}};
handle_call({send,Ref,Tag,Msg},From,S) ->
    {W, L} = f:lrot(S#jsrv.workers),
    gen_server:cast(W,{send,From,Ref,Tag,Msg}),
    {noreply, S#jsrv{workers=L}};
handle_call({bad},From,S) ->
    erlang:foobar(),
    {noreply,S};
handle_call(Msg,From,S) ->
    log:warn(self(), "Cannot understand call from ~p: ~p", [From,Msg]),
    {reply, {error, unknown_msg}, S}.

% @hidden
handle_cast({send,From,Ref,Tag,Msg}, S) ->
    Result = send_peer(S#jsrv.peer,Ref,Tag,Msg),
    gen_server:reply(From, Result),
    {noreply, S};
handle_cast({'STOP'}, S) ->
    case S#jsrv.worker of
        yes -> nop;
        no  -> shutdown(S#jsrv.peer,S)
    end,
    timer:send_after(500,?EJMSG(erlang:make_ref(), ?TAG_OK,[?EJMSGPART(call,bye)])),
    log:info(self(),"stopping with state: ~p", [S]),
    {noreply, S#jsrv{stopping=true}};
handle_cast(Msg,S) ->
    log:info(self(),"cannot handle cast: ~p", [Msg]),
    {noreply, S}.

% @hidden
% port messages
handle_info({Port,{data,"\n"}},S) when is_port(Port) ->
    {noreply,S};
handle_info({Port,{data,Msg}},S) when is_port(Port) ->
    log:info(self(),"port says: ~p", [Msg]),
    {noreply,S};
% ej_srv messages
handle_info({From,Ref,{?TAG_OK,[?EJMSGPART(call,handshake)]}},S) ->
    % TODO check Ref
    log:debug(self(), "info handshake from: ~p", [From]),
    {noreply, S#jsrv{peer=From}};
handle_info({From,Ref,{?TAG_OK,[?EJMSGPART(call,bye)]}},S) ->
    % TODO check Ref
    case S#jsrv.stopping of
        false -> 
            log:warn(self(), "ignore 'bye' while not in stopping state: ~p", [From]),
            {noreply,S};
        true  ->
            % TODO only allow from peer and self()
            log:info(self(), "'bye' triggered: ~p", [From]),
            {stop, normal, S}
    end;
handle_info(Msg={'EXIT', Pid, Reason},S) ->
    log:warn(self(), "EXIT: ~p", [Msg]),
    Peer =  S#jsrv.peer,
    if 
        is_port(Pid) -> handshake(S#jsrv.bindir);
        true         -> 
            case Msg of
                {'EXIT', Peer, noconnection} ->
                    handshake(S#jsrv.bindir);
                Any ->
                    log:debug(self(), "don't know how to handle exit: ~p", [Msg])
            end
    end,
    {noreply, S}; 
handle_info({'STOP'},S) ->
    case S#jsrv.worker of
        yes -> 
            log:info(self(),"stopping with state: ~p", [S]),
            {stop, normal, S};
        no  -> 
            {noreply,S}
    end;
% messages to be routed to client
handle_info({From,Ref={Client,_Id},Msg},S) ->
    log:debug(self(), "got result: ~p ~p ~p", [From,Ref,Msg]),
    Client ! {self(),Ref,Msg},
    {noreply, S};
handle_info(Msg,S) ->
    log:info(self(),"info: ~p", [Msg]),
    {noreply,S}.

% @hidden     
terminate(_Reason,S) ->
    {noreply, S}.

% @hidden     
code_change(_OldVsn, S, _Extra) -> 
    {ok, S}.


%% ------ PRIVATE PARTS -----

send_peer(Peer,Ref,Tag,Msg) ->
    log:debug(self(), "send_peer: ~p", [?EJMSG(Ref,Tag,Msg)]),
    Peer ! ?EJMSG(Ref,Tag,Msg).

start_worker(S) ->
    gen_server:start(?MODULE, S#jsrv{worker=yes}, []).


handshake(Bindir) ->
    {ok, Hostname} = inet:gethostname(),
    Peer = {?PEERNAME,list_to_atom(?PEERSTR ++ "@" ++ Hostname)},
    case quick_handshake(Peer) of
        {ok,From}         -> From;
        {error,no_answer} -> full_handshake(Peer,Bindir)
    end.
    
quick_handshake(Peer) ->
    log:info(self(), "quick handshake to: ~p", [Peer]),
    run_handshake(Peer).

full_handshake(Peer,Bindir) ->
    Args = "-peer " ++ atom_to_list(node())
        ++ " -sname " ++ ?PEERSTR
        ++ " -cookie " ++ atom_to_list(erlang:get_cookie()),
    Cmd  = Bindir ++ "/" ++ ?JNODEBIN ++ " " ++ Args ++ " &",
    log:info(self(), "open port to org.ister.ej.Node: ~p", [Cmd]),
    erlang:open_port({spawn, Cmd},[]),
    timer:sleep(500),
    log:info(self(), "full handshake to: ~p", [Peer]),
    case run_handshake(Peer) of
        {ok,From}         -> From;
        {error,no_answer} -> Peer
    end.
    

run_handshake(Peer) ->
    Ref = ?EJMSGREF(self(),erlang:make_ref()),
    send_peer(Peer, Ref, ?TAG_NODE, [?EJMSGPART(call,handshake)]),
    receive
        {From,Ref,{?TAG_OK,[?EJMSGPART(call,handshake)]}} -> 
            log:info(self(), "got handshake from: ~p", [From]),
            {ok,From}
    after
        1000 -> 
            log:info(self(), "handshake timeout", []),
            {error,no_answer}
    end.

%% TODO block and wait for answer right here
shutdown(Peer,S) ->
    send_peer(Peer, ?EJMSGREF(self(),erlang:make_ref()), ?TAG_NODE, [?EJMSGPART(call,shutdown)]),
    lists:map(fun(W) -> W ! {'STOP'} end, S#jsrv.workers).

   
%% ------ TESTS ------

start_stop_test() ->
    {ok,Pid} = start(2,"../bin"),
    timer:sleep(500),
    ?assert(is_pid(Pid)),
    stop(),
    timer:sleep(500).






  
