-module(mergesort_test).

-include_lib("eunit/include/eunit.hrl").

random_list(Len) ->
    [X||{_,X} <- lists:sort([{random:uniform(), N} || N <- lists:seq(1, Len)])].


smallest_test_() ->
    S = [1, 2],
    [
        ?_assertEqual(S, mergesort:sort([2, 1], 2, 2)),
        ?_assertEqual(S, mergesort:sort([2, 1], 2, 2))
    ].

smaller_test_() ->
    L = lists:seq(4, 1, -1),
    S = lists:seq(1, 4),
    [
        ?_assertEqual(S, mergesort:sort(L, 4, 2)),
        ?_assertEqual(S, mergesort:sort(L, 4, 4))
    ].

small_test_() ->
    L = lists:seq(16, 1, -1),
    S = lists:seq(1, 16),
    [
        ?_assertEqual(S, mergesort:sort(L, 16, 2)),
        ?_assertEqual(S, mergesort:sort(L, 16, 4))
    ].

many_test_() ->
    LengthsParts = [
        {4, [2, 4]},
        {8, [2]},
        {16, [2, 4, 16]},
        {32, [2]},
        {64, [2, 4, 8, 64]},
        {128, [2]},
        {256, [2, 4, 16, 256]}
    ],

    lists:map(
        fun({Len, LNumParts}) ->
                List = random_list(Len),
                Sorted = lists:sort(List),
                lists:map(
                    fun(Parts) ->
                            {lists:flatten(io_lib:format("~p", [{Len, Parts}])),
                                ?_assertEqual(
                                    Sorted,
                                    mergesort:sort(List, Len, Parts)
                                )
                            }
                    end,
                    LNumParts
                )
        end,
        LengthsParts
    ).
