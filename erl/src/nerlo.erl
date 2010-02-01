%% @doc Main ;)
%%
%% @author Ingo Schramm

-module(nerlo).

-export([test/0,erlang/0]).

-include("global.hrl").
-include_lib("eunit/include/eunit.hrl").

-author("Ingo Schramm").

%% --- TESTS ---

test() ->
    net_adm:ping(test1@localhost),
    net_adm:ping(test2@localhost),

    f:test(),
    nerlo_jsrv:test(),

    lists:foreach(fun(Node) -> rpc:call(Node, erlang, halt, []) end, nodes()),
    erlang:halt().


erlang() ->
    rock_n_roll.


