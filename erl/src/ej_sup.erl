%% supervisor
%%
%% author: ingo schramm

-module(ej_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

-author("Ingo Schramm").

-include("global.hrl").
-include_lib("eunit/include/eunit.hrl").


start_link(Args) when is_list(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, [Args]).

init(Args = [_|_]) ->
    %log:info(self(), "initializing supervisor '~p' with Args: ~w", [?MODULE, Args]),
    [SrvArgs|_] = Args,
    Spec = 
    {ok,{{one_for_all,10,10},
        [get_log_spec()
        ,get_ej_spec(SrvArgs)
        ]
    }},
    %log:debug(self(), "child spec: ~w", [Spec]),
    Spec.

get_ej_spec(SrvArgs) ->
    {ej_srv
        ,{ej_srv, start_link, SrvArgs}
        ,transient % or permanent?
        ,5000
        ,worker
        ,[ej_srv]
        }.
    
get_log_spec() ->
    {ej_log
        ,{ej_log, start_link, []}
        ,transient % or permanent?
        ,5000
        ,worker
        ,[ej_log]
        }.   

%% ----- TESTS -----

start_test() ->
    {ok, Pid} = start_link([]),
    ?assert(is_pid(Pid)).
