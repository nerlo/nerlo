
%% logging interface
%%
%% author: ingo schramm

-module(ej_log).
-export([fatal/2, err/2, error/2, warn/2, warning/2, info/2, debug/2]).
% gen_server exports
-export([start/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([log/5,set/2]).

-author("Ingo Schramm").

-include("global.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state,
       {verbosity = verbose 
       ,f         = fun(Who, Node, Level, Msg, Args) -> print_log({Who,Node}, Level, Msg, Args) end
       }).

% @doc Write a logging message.
% @spec log(Who::pid(), Node::node(), L::Level, Msg::string(), Args::list()) -> ok
%       where
%             Level = 'FATAL' + 'ERROR' + 'WARNING' + 'INFO' + 'DEBUG'
log(Who, Node, Level, Msg, Args) ->
    gen_server:cast({global,?MODULE}, {Who, Node, Level, Msg, Args}).

log(Level, Msg, Args) ->
    gen_server:cast({global,?MODULE}, {self(), node(), Level, Msg, Args}).

fatal(Msg, Args) ->
    log(fatal, Msg, Args).

error(Msg, Args) ->
    err(Msg, Args).
err(Msg, Args) ->
    log(error, Msg, Args).
    
warning(Msg, Args) ->
    warn(Msg, Args).
warn(Msg, Args) ->
    log(warn, Msg, Args).

info(Msg, Args) ->
    log(info, Msg, Args).

debug(Msg, Args) ->
    log(debug, Msg, Args).

% @doc Set a log server property.
% 
% Avalable properties:
% <pre>
% verbosity      - silent | verbose
% </pre>
%
% @spec set(Name::atom(), Value::any()) -> ok
set(verbosity, silent) ->
    gen_server:cast({global,?MODULE}, {set, {verbosity, silent}});
set(verbosity, verbose) ->
    gen_server:cast({global,?MODULE}, {set, {verbosity, verbose}}).

% @doc Start the global log server. 
% @spec start() -> {ok, Pid::pid()} | {error, Reason::any()}   
start() ->
    start_link().

% @hidden
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, #state{}, []).

% @doc Stop the global log server.
% @spec stop() -> ok
stop() ->
    gen_server:cast({global, ?MODULE},{'STOP'}),
    global:unregister_name(?MODULE).

% @hidden 
init(State) ->
    S1 = 
        case has_log4erl() of
            true  ->
                log4erl:add_file_appender(file, {"../log", "ej_erl", {size, 100000}, 4, "log", debug}),
                log4erl:change_format(file1, "[%L] %j %T %l%n"), 
                State#state{f = fun(Who, Node, Level, Msg, Args) -> log4(Who, Node, Level, Msg, Args) end};
            false -> State
        end,
    info("logger is working", []),
    {ok,S1}.

% @hidden 
handle_call({Who, Level, Msg, Args},_From,State) ->
    log:print_log(Who, Level, Msg, Args),
    {noreply, State}.

% @hidden 
handle_cast({Who, Node, Level, Msg, Args}, State) ->
    case State#state.verbosity of
        %verbose -> log:print_log({Who,Node}, Level, Msg, Args);
        verbose -> F = State#state.f,
            F(Who, Node, Level, Msg, Args);
        _Any    -> ok
    end,
    {noreply, State};
handle_cast({set, {K,V}},State) ->
    NewState =
    case K of
        verbosity -> State#state{verbosity = V};
        _Any      -> State 
    end, 
    {noreply, NewState};
handle_cast({'STOP'}, State) ->
    log:info(self(),"stopping with state: ~p", [State]),
    {stop, normal, State};
handle_cast(Msg,State) ->
    log:print_log(self(), 'ERROR', "log_srv cannot understand: ~p", [Msg]),
    {noreply, State}.

% @hidden 
handle_info(_Msg,State) ->
    {noreply, State}.

% @hidden 
terminate(_Reason,State) ->
    {noreply, State}.

% @hidden 
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

    
%% ----- PRIVATE PARTS -----

print_log(Who, Level, Msg, Args) ->
    {Year, Month, Day}      = erlang:date(),
    {Hour, Minute, Second}  = erlang:time(),
    {_Megas, _Secs, Micros} = erlang:now(),
    List = [Level,Year,Month,Day,Hour,Minute,Second,Micros,Who|Args],
    io:format("~p ~p/~p/~p-~p:~p:~p.~p ~p -> " ++ Msg ++ "~n", List).

log4(Who, Node, Level, Msg, Args) ->
    List = [{Who, Node}|Args],
    log4erl:log(Level, " ~p -> " ++ Msg, List).

has_log4erl() ->
    case lists:keyfind(log4erl, 1, application:which_applications()) of
        false           -> false;
        {log4erl, _, _} -> true
    end.

%% ----- TESTS -----

start_stop_test() ->
    start(),
    timer:sleep(100),
    stop(),
    start(),
    timer:sleep(100),
    stop().













