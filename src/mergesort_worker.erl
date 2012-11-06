-module(mergesort_worker).

-include_lib("eunit/include/eunit.hrl").

%%% API
-export([sort/3]).

%%% Internal functions for self-calling
-export([sort/4]).

sort(List, Length, NumParts) ->
    {part, Ret} = sort(self(), List, Length, NumParts),
    Ret.

%% @doc Sort List of Length using Parts parts
-spec sort(list(A), pos_integer(), pos_integer()) -> list(A).
sort(To, [Elem], 1, _) ->
    To ! {part, [Elem]};
sort(To, List, Length, NumParts) ->
    distribute(List, Length, NumParts),
    Parts = collect(NumParts),
    To ! {part, lists:merge(Parts)}.

distribute(Orig, Length, NumParts) ->
    Self = self(),
    NewLength = Length div NumParts,
    lists:foldl(
        fun(_, Acc) ->
                {H, T} = lists:split(NewLength, Acc),
                spawn(?MODULE, sort, [Self, H, NewLength, NumParts]),
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
