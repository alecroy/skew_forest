-module(skew_forest).
-export([create/0, create/1, cons/2]).
-include("skew_forest.hrl").
-include_lib("eunit/include/eunit.hrl").



create() -> create([]).
create(List) ->
    Size = length(List),
    #forest{size=Size, trees=trees(Size, List, [])}.


cons(Element, #forest{size=Size, trees=Trees}) ->
    #forest{size=Size + 1, trees=cons_trees(Element, Trees)}.



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