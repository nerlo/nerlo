
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
    log:info(self(), "starting; type: ~p args: ~p", [Type,Args]),
    case Type of
        normal   -> ej_sup:start_link([]);
        takeover -> ok;
        failover -> ok
    end.

prep_stop(State) ->     
    log:info(self(), "prepare stopping with state: ~p", [State]),
    ej_srv:stop(),
    timer:sleep(1000),
    ok.

stop(State) ->     
    log:info(self(), "stopping with state: ~p", [State]),
    ok.
    
getenv(K,Def) ->
    case application:get_env(?APPNAME,K) of
        undefined -> Def;
        {ok,Val}  -> Val
    end.

