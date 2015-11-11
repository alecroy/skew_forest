-module(skew_forest).
-export([create/0, create/1, cons/2, is_empty/1, head/1, tail/1, foreach/2,
         map/2, nth/2, update/3]).
-include("skew_forest.hrl").
-include_lib("eunit/include/eunit.hrl").


create() -> create([]).
create(List) ->
    Size = length(List),
    #forest{size=Size, trees=trees(Size, List, [])}.


cons(Element, #forest{size=Size, trees=Trees}) ->
    #forest{size=Size + 1, trees=cons_trees(Element, Trees)}.


is_empty(#forest{size=Size}) -> Size == 0.


head(#forest{trees=[#tree{value=Value} | _Trees]}) -> Value.


tail(Empty = #forest{size=0}) -> Empty;
tail(#forest{size=Size, trees=[#tree{size=1} | Trees]}) ->
    #forest{size=Size - 1, trees=Trees};
tail(#forest{size=Size, trees=[#tree{left=L, right=R} | Trees]}) ->
    #forest{size=Size - 1, trees=[L, R | Trees]}.


foreach(Function, #forest{trees=Trees}) ->
    lists:foreach(fun (Tree) -> foreach_tree(Function, Tree) end, Trees).


map(Function, Forest = #forest{trees=Trees}) ->
    Forest#forest{trees=[ map_tree(Function, Tree) || Tree <- Trees ]}.


nth(N, Forest) -> nth_trees(N, Forest#forest.trees).


update(N, Value, Forest) ->
    Forest#forest{trees=update_trees(N, Value, Forest#forest.trees)}.



%% helpers

highest_power_of_2(N) -> trunc(math:pow(2, trunc(math:log2(N)))).

highest_skew_power_of_2(N) -> highest_power_of_2(N + 1) - 1.


trees(_Length, [], Trees) -> Trees;
trees(Length, List, Trees) ->
    TreeSize = highest_skew_power_of_2(Length),
    {Rest, Values} = lists:split(Length - TreeSize, List), % take values off end
    Tree = tree(TreeSize, Values),
    trees(Length - TreeSize, Rest, [Tree | Trees]).


tree(1, [Value]) -> #tree{size=1, value=Value};
tree(Size, [Value | Rest]) ->
    HalfSize = Size div 2,
    {Left, Right} = lists:split(HalfSize, Rest),
    {LeftTree, RightTree} = {tree(HalfSize, Left), tree(HalfSize, Right)},
    #tree{size=Size, value=Value, left=LeftTree, right=RightTree}.


cons_trees(E, [T = #tree{size=S}, T2 = #tree{size=S2} | Trees]) when S == S2 ->
    [#tree{size=2 * S + 1, value=E, left=T, right=T2} | Trees];
cons_trees(E, Trees) -> [#tree{size=1, value=E} | Trees].


foreach_tree(Function, #tree{size=1, value=Value}) -> Function(Value);
foreach_tree(Function, #tree{value=Value, left=L, right=R}) ->
    Function(Value), foreach_tree(Function, L), foreach_tree(Function, R).


map_tree(Function, Tree = #tree{size=1, value=Value}) ->
    Tree#tree{value=Function(Value)};
map_tree(Function, Tree = #tree{value=Value, left=L, right=R}) ->
    Tree#tree{value=Function(Value),
              left=map_tree(Function, L),
              right=map_tree(Function, R)}.


nth_trees(N, [Tree = #tree{size=Size} | _Trees]) when N =< Size ->
    nth_tree(N, Tree);
nth_trees(N, [#tree{size=Size} | Trees]) -> nth_trees(N - Size, Trees).

nth_tree(1, #tree{value=Value}) -> Value;
nth_tree(N, #tree{size=Size, left=L}) when N - 1 =< Size div 2 ->
    nth_tree(N - 1, L);
nth_tree(N, #tree{size=Size, right=R}) -> nth_tree(N - 1 - Size div 2, R).


update_trees(N, Value, [Tree = #tree{size=Size} | Trees]) when N =< Size ->
    [update_tree(N, Value, Tree) | Trees];
update_trees(N, Value, [Tree = #tree{size=Size} | Trees]) ->
    [Tree | update_trees(N - Size, Value, Trees)].

update_tree(1, Value, T) -> T#tree{value=Value};
update_tree(N, Value, T = #tree{size=Size, left=L}) when N - 1 =< Size div 2 ->
    T#tree{left=update_tree(N - 1, Value, L)};
update_tree(N, Value, T = #tree{size=Size, right=R}) ->
    T#tree{right=update_tree(N - 1 - Size div 2, Value, R)}.



%% tests (non-exports)

highest_skew_power_of_2_test() ->
    15 = highest_skew_power_of_2(15),
    7 = highest_skew_power_of_2(14),
    7 = highest_skew_power_of_2(10),
    7 = highest_skew_power_of_2(8),
    7 = highest_skew_power_of_2(7),
    1 = highest_skew_power_of_2(1),
    ok.
