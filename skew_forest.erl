-module(skew_forest).
-export([create/0, create/1, cons/2, is_empty/1, head/1, tail/1, foreach/2,
         map/2, nth/2]).
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



%% helpers

highest_power_of_2(N) -> trunc(math:pow(2, trunc(math:log2(N)))).

highest_skew_power_of_2(N) -> highest_power_of_2(N + 1) - 1.

lower_skew_power(Skew) -> Skew div 2.


trees(_Length, [], Trees) -> Trees;
trees(Length, List, Trees) ->
    TreeSize = highest_skew_power_of_2(Length),
    {Rest, Values} = lists:split(Length - TreeSize, List), % take values off end
    Tree = tree(TreeSize, Values),
    trees(Length - TreeSize, Rest, [Tree | Trees]).


tree(1, [Value]) -> #tree{size=1, value=Value};
tree(Size, [Value | Rest]) ->
    HalfSize = lower_skew_power(Size),
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



%% tests

highest_skew_power_of_2_test() ->
    15 = highest_skew_power_of_2(15),
    7 = highest_skew_power_of_2(14),
    7 = highest_skew_power_of_2(10),
    7 = highest_skew_power_of_2(8),
    7 = highest_skew_power_of_2(7),
    1 = highest_skew_power_of_2(1),
    ok.


lower_skew_power_test() ->
    7 = lower_skew_power(15),
    3 = lower_skew_power(7),
    1 = lower_skew_power(3),
    ok.


create_test() ->
    #forest{size=0, trees=[]} = create(),
    #forest{size=1, trees=[#tree{size=1, value=1}]}
        = create([1]),
    #forest{size=2, trees=[#tree{size=1, value=1},
                           #tree{size=1, value=2}]}
        = create([1, 2]),
    #forest{size=3,
            trees=[#tree{size=3, value=1,
                         left=#tree{size=1,value=2},
                         right=#tree{size=1, value=3}}]}
        = create([1, 2, 3]),
    ok.


cons_test() ->
    One = #forest{size=1, trees=[#tree{size=1, value=1}]},
    One = cons(1, create()),
    OneTwo = #forest{size=2, trees=[#tree{size=1, value=1},
                                      #tree{size=1, value=2}]},
    OneTwo = cons(1, cons(2, create())),
    OneTwoThree = #forest{size=3, trees=[#tree{size=3, value=1,
                                               left=#tree{size=1, value=2},
                                               right=#tree{size=1, value=3}}]},
    OneTwoThree = cons(1, cons(2, create([3]))),
    ok.


is_empty_test() ->
    true = is_empty(create()),
    false = is_empty(create([1])),
    ok.


tail_test() ->
    #forest{size=0, trees=[]} = tail(create([1])),
    #forest{size=1, trees=[#tree{size=1, value=2}]}
        = tail(create([1, 2])),
    ok.


head_test() ->
    1 = head(create([1])),
    3 = head(tail(tail(create([1, 2, 3])))),
    ok.


map_test() ->
    Identity = fun (X) -> X end,
    Forest = create(lists:seq(1, 5)),
    Forest = map(Identity, Forest),
    Squares = create([1, 4, 9, 16, 25]),
    Squares = map(fun (N) -> N * N end, Forest),
    ok.


nth_test() ->
    1 = nth(1, create([1])),
    100 = nth(100, create(lists:seq(1, 100))),
    ok.
