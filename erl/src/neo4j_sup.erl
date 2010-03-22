%% supervisor
%%
%% author: ingo schramm

-module(neo4j_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

-author("Ingo Schramm").

-include("global.hrl").
-include_lib("eunit/include/eunit.hrl").


start_link(Args) when is_list(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, [Args]).

init(Args = [_|_]) ->
    log:info(self(), "initializing supervisor '~p' with Args: ~w", [?MODULE, Args]),
    [SrvArgs|_] = Args,
    Spec = 
    {ok,{{one_for_one,10,10},
        [get_spec(SrvArgs)]
    }},
    log:debug(self(), "child spec: ~w", [Spec]),
    Spec.

get_spec(SrvArgs) ->
    {reducer_sup
        ,{neo4j_srv, start_link, SrvArgs}
        ,transient % or permanent?
        ,5000
        ,worker
        ,[neo4j]
        }.
    
    

%% ----- TESTS -----

start_test() ->
    {ok, Pid} = start_link([]),
    ?assert(is_pid(Pid)).
