
%% @doc Application module for Erlang/Java bridge..
%%
%% @author Ingo Schramm

-module(ej_app).
-behaviour(application).
-export([start/0,start/2,prep_stop/1,stop/1,stop/0]).

-author("Ingo Schramm").

-include("global.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(APPNAME, ej).

start() ->
    application:start(?APPNAME).

stop() ->
    application:stop(?APPNAME).

start(Type, Args) ->
    application:set_env(?APPNAME, listeners, sets:new()),
    case Type of
        normal   -> ej_sup:start_link([]);
        takeover -> ok;
        failover -> ok
    end.

prep_stop(State) ->     
    ej_log:info("prepare stopping with state: ~p", [State]),
    ej_srv:stop(),
    timer:sleep(1000),
    ok.

stop(State) ->     
    ej_log:info("stopping with state: ~p", [State]),
    ej_log:stop(),
    ok.
    
getenv(K,Def) ->
    case application:get_env(?APPNAME,K) of
        undefined -> Def;
        {ok,Val}  -> Val
    end.

