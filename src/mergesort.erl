%%% @doc Merge sort
%%%
%%% Takes 3 parameters:
%%% 1. original list
%%% 2. length of the list
%%% 3. number Parts
%%%
%%% Limitations:
%%% Length of the list must be a power of Parts.
%%% In other words, this must be satisfied:
%%% Parts ^ n = Length, n must be integer >= 1.
-module(mergesort).

-include_lib("eunit/include/eunit.hrl").

%%% For bencherl
-export([bench_args/2, run/3]).

%%% API
-export([sort/3]).

%%% Internal functions for self-calling
-export([random_list/1, sort/4]).


%%% Benchmarking API
bench_args(Version, _Conf) ->
    Length = case Version of
        short -> 1 bsl 16;
        intermediate -> 1 bsl 18;
        long -> 1 bsl 24
    end,
    {ok, [[PartsStr]]} = init:get_argument(mergeparts),
    Parts = list_to_integer(PartsStr),
    [[Length, Parts]].

run([Length, Parts], _, _) ->
    List = lists:reverse(lists:seq(1, Length)),
    sort(List, Length, Parts),
    ok.

%% @doc Sort List of Length using Parts parts
-spec sort(list(A), pos_integer(), pos_integer()) -> list(A).
sort(List, Length, NumParts) ->
    {part, Ret} = sort(undefined, List, Length, NumParts),
    Ret.

-spec sort(undefined | pid(), list(A), pos_integer(), pos_integer()) ->
    {part, list(A)}.
sort(To, [Elem], 1, _) ->
    maybe_send(To, {part, [Elem]});
sort(To, List, Length, NumParts) ->
    distribute(List, Length, NumParts),
    Parts = collect(NumParts),
    maybe_send(To, {part, lists:merge(Parts)}).

distribute(Orig, Length, NumParts) ->
    Self = self(),
    NewLength = Length div NumParts,
    lists:foldl(
        fun(_, Acc) ->
                {H, T} = lists:split(NewLength, Acc),
                case NewLength of
                    1 ->
                        Self ! {part, H};
                    _ ->
                        spawn(?MODULE, sort, [Self, H, NewLength, NumParts])
                end,
                T
        end,
        Orig,
        lists:seq(1, NumParts)
    ).

collect(Parts) ->
    collect(Parts, [], 0).

collect(Parts, Acc, Parts) ->
    Acc;
collect(Parts, Acc, Collected) ->
    receive
        {part, Part} ->
            collect(Parts, [Part|Acc], Collected + 1)
    end.

-spec maybe_send(undefined | pid(), A) -> A.
maybe_send(undefined, X) ->
    X;
maybe_send(Pid, X) when is_pid(Pid) ->
    Pid ! X.

-spec random_list(pos_integer()) -> list(integer()).
random_list(Len) ->
    [X||{_,X} <- lists:sort([{random:uniform(), N} || N <- lists:seq(1, Len)])].
