%% @doc Some useful functions.
%%
%% @author Ingo Schramm

-module(f).

-export([for/3, for/4, step/4, step/5, split_list/2, lrot/1, rrot/1, avg/1]).

%% erank

-author("Ingo Schramm").
-include_lib("eunit/include/eunit.hrl").


%% --- for ---

for(Max, Max, F) -> F(Max);
for(I, Max, F)   -> F(I), for(I+1, Max, F).

for(Max, Max, F, Acc) -> F(Max, Acc);
for(I, Max, F, Acc)   -> NewAcc = F(I, Acc), for(I+1, Max, F, NewAcc).

%% --- step ---

step(I, Max, Step, F) 
    when I >= Max, I rem Step =:= 0 -> 
        F(Max);
step(I, Max, Step, F) 
    when I < Max -> 
        F(I), step(I+Step, Max, Step, F);
step(_I, _Max, _Step, _F) ->
    ok.

step(I, Max, Step, F, Acc) 
    when I >= Max, I rem Step =:= 0 ->
        F(Max, Acc);
step(I, Max, Step, F, Acc) 
    when I < Max ->
        NewAcc = F(I, Acc), step(I+Step, Max, Step, F, NewAcc);
step(_I, _Max, _Step, _F, Acc) ->
    Acc.

%% --- split_list ---

split_list(_L,0) ->
    [[]];
split_list(L,N) ->
    Len       = length(L),
    PartLen   = erlang:round(Len / N),
    {Out,_In} = ?MODULE:for(1,N,
            fun(I, {Out,In}) ->
                {Head,Tail} = do_split(PartLen,In,I,N),
                {[Head|Out],Tail}
            end, 
            {[],L}),
    lists:reverse(Out).

do_split(_Len,L,I,I) ->
    {L,[]};
do_split(_Len,L=[],_I,_N) ->
    {L,[]};
do_split(Len,L,_I,_N) ->
    lists:split(Len,L).


lrot([First|Rest]) ->
    {First, lists:reverse([First|lists:reverse(Rest)])}.

rrot(L=[_|_]) ->
    [Last|Rest] = lists:reverse(L),
    {Last,[Last|lists:reverse(Rest)]}.

% recursive average

avg([]) ->
    0;
avg(L = [H|_T]) when is_number(H) ->
    {_N,A} =
    lists:foldl(
        fun(Z,{N,Avg}) -> 
            {N+1, ((N-1)*Avg/N) + (1/N * Z)}
        end,
        {1,0},L),
    A.

%% ----- TESTS -----

for_3_test() ->
    put(l,[]),
    for(1,5,fun(I) -> L = get(l), put(l, L ++ [I]) end),
    L = get(l),
    erase(),
    ?assertEqual([1,2,3,4,5], L).
    
for_4_test() ->
    L = for(1,3,fun(I,Acc) -> Acc ++ [I] end, []),
    ?assertEqual([1,2,3], L).
    
step_3_test() ->
    put(l,[]),
    step(1,10,2,fun(I) -> L = get(l), put(l, L ++ [I]) end),
    L = get(l),
    erase(),
    ?assertEqual([1,3,5,7,9], L).

step_4_test() ->
    L = step(1,10,2,fun(I,Acc) -> Acc ++ [I] end, []),
    ?assertEqual([1,3,5,7,9], L).


split_list_test() ->
    L = [1,2,3,4,5,6,7,8,9],
    S = split_list(L,3),
    ?assertEqual([[1,2,3],[4,5,6],[7,8,9]],S).
    
split_list_with_remainder_test() ->
    L = [1,2,3,4,5,6,7,8,9,0],
    S = split_list(L,3),
    ?assertEqual([[1,2,3],[4,5,6],[7,8,9,0]],S).

split_list_with_shorter_tail_test() ->
    L = [1,2,3,4,5,6,7,8],
    S = split_list(L,3),
    ?assertEqual([[1,2,3],[4,5,6],[7,8]],S).

split_list_with_short_frags_test() ->
    L = [1,2,3,4,5,6,7,8],
    S = split_list(L,7),
    ?assertEqual([[1],[2],[3],[4],[5],[6],[7,8]],S).
    
split_list_with_resulting_empty_frags_test() ->
    L = [1,2,3,4,5,6],
    S = split_list(L,7),
    ?assertEqual([[1],[2],[3],[4],[5],[6],[]],S).

lrot_test() ->
    List = [1,2,3],
    {Next, List1} = lrot(List),
    ?assertEqual(1,Next),
    ?assertEqual([2,3,1],List1).

rrot_test() ->
    List = [1,2,3],
    {Next, List1} = rrot(List),
    ?assertEqual(3,Next),
    ?assertEqual([3,1,2],List1).

rot_bijection_test() ->
    List = [1,2,3],
    {_Next1, List1} = lrot(List),
    {_Next2, List2} = rrot(List1),
    ?assertEqual(List,List2).

avg1_test() ->
    ?assertEqual(1.0,avg([1,1,1,1,1,1])).

avg2_test() ->
    ?assertEqual(7.75,avg([4,6,9,12])).

avg3_test() ->
    ?assertEqual(2.75,avg([1.1,2.4,5.3,2.2])). 
        
avg4_test() ->
    ?assertEqual(0,avg([])).


