%% @doc simple logging
%%
%% @author Ingo Schramm

-module(log).

-export([fatal/3, err/3, error/3, warn/3, warning/3, info/3, debug/3, print_log/4, do_log/4]).

-author("Ingo Schramm").

-include("global.hrl").
-include_lib("eunit/include/eunit.hrl").


fatal(Who, Msg, Args) ->
    do_log(Who, 'FATAL', Msg, Args).

error(Who, Msg, Args) ->
    err(Who, Msg, Args).
err(Who, Msg, Args) ->
    do_log(Who, 'ERROR', Msg, Args).
    
warning(Who, Msg, Args) ->
    warn(Who, Msg, Args).
warn(Who, Msg, Args) ->
    do_log(Who, 'WARNING', Msg, Args).

info(Who, Msg, Args) ->
    do_log(Who, 'INFO', Msg, Args).

debug(Who, Msg, Args) ->
    do_log(Who, 'DEBUG', Msg, Args).
    
%% ----- PRIVATE PARTS -----

% may also use error_logger

do_log(Who, Level, Msg, Args) ->
    case global:whereis_name(log_srv) of
        undefined -> print_log(Who, Level, Msg, Args);
        _Pid      -> log_srv:log(Who, node(), Level, Msg, Args)
    end.

print_log(Who, Level, Msg, Args) ->
    {Year, Month, Day}      = erlang:date(),
    {Hour, Minute, Second}  = erlang:time(),
    {_Megas, _Secs, Micros} = erlang:now(),
    List = [Level,Year,Month,Day,Hour,Minute,Second,Micros,Who|Args],
    io:format("~p ~p/~p/~p-~p:~p:~p.~p ~p -> " ++ Msg ++ "~n", List).
