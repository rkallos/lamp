-module(lamp).

-include("../include/lamp.hrl").

-export([
    counter_inc/1,
    counter_inc/2,
    dist_record/2,
    gauge_set/2,
    poll/0
]).

-spec counter_inc(binary()) -> ok | {error, term()}.

counter_inc(Key) ->
    counter_inc(Key, 1).

-spec counter_inc(binary(), integer()) -> ok | {error, term()}.

counter_inc(Key, Amt) ->
    {ok, Counter} = get_metric(counter, Key),
    counters:add(Counter, 1, Amt).

-spec dist_record(binary(), integer()) -> ok | {error, term()}.

dist_record(Key, Sample) ->
    {ok, Dist} = get_metric(dist, Key),
    Idx = case counters:get(Dist, ?DIST_COUNT) of
        % TODO: allow custom dist sizes?
        N when N >= 200 ->
            % evict a random sample at idx in [?DIST_FIELDS+1, ?DIST_FIELDS+N]
            % TODO: Use granderl for faster RNG?
            rand:uniform(200) + ?DIST_FIELDS;
        N ->
            N + 1
    end,
    counters:put(Dist, Idx, Sample),
    counters:add(Dist, ?DIST_COUNT, 1),

    Max = counters:get(Dist, ?DIST_MAX),
    Min = counters:get(Dist, ?DIST_MIN),
    counters:put(Dist, ?DIST_MAX, max(Sample, Max)),
    counters:put(Dist, ?DIST_MIN, min(Sample, Min)).

-spec gauge_set(binary(), integer()) -> ok | {error, term()}.

gauge_set(Key, Value) ->
    {ok, Gauge} = get_metric(gauge, Key),
    counters:put(Gauge, 1, Value).

%% TODO: polling backend, for now only prints the values
%% TODO: Reset counters to 0 when polled
poll() ->
    Values = [{Key, Type, poll_metric(Type, Ref)} || {Key, Type, Ref} <- get_metrics()],
    io:format(user, "~p~n", [Values]).

% private

-spec get_metric(atom(), binary()) -> counters:counters_ref() | {error, term()}.

get_metric(Type, Key) ->
    case persistent_term:get({?MODULE, Key}, key_not_found) of
        {Type, Ref} ->
            {ok, Ref};
        key_not_found ->
            Ref = make_metric(Type),
            persistent_term:put({?MODULE, Key}, {Type, Ref}),
            {ok, Ref};
        {Type2, _} ->
            {error, {metric_has_type, Type2}}
    end.

get_metrics() ->
    [{Key, Type, Ref} || {{?MODULE, Key}, {Type, Ref}} <- persistent_term:get()].

make_metric(counter) ->
    counters:new(1, [atomics]);
make_metric(dist) ->
    Ref = counters:new(200 + ?DIST_FIELDS, [atomics]),
    counters:put(Ref, ?DIST_MIN, 1 bsl 62),
    counters:put(Ref, ?DIST_MAX, -1 bsl 62),
    Ref;
make_metric(gauge) ->
    counters:new(1, [atomics]).

poll_metric(counter, {atomics, Ref}) ->
    atomics:exchange(Ref, 1, 0);
poll_metric(dist, {atomics, Ref}) ->
    NSamples = atomics:exchange(Ref, ?DIST_COUNT, 0),
    Max = atomics:exchange(Ref, ?DIST_MAX, -1 bsl 62),
    Min = atomics:exchange(Ref, ?DIST_MIN,  1 bsl 62),
    Samples0 = [atomics:exchange(Ref, ?DIST_FIELDS + Idx, 0) ||
        Idx <- lists:seq(1, min(200, NSamples))],
    Samples = list_to_tuple(lists:sort(Samples0)),
    SamplesLen = size(Samples),
    {P50, P90, P99} = case SamplesLen of
        0 -> {0, 0, 0};
        _ -> {
            element(round(SamplesLen / 2), Samples),
            element(round((9 * SamplesLen) / 10), Samples),
            element(round((90 * SamplesLen) / 100), Samples)
        }
    end,
    #{
        n_samples => NSamples,
        max => Max,
        min => Min,
        p50 => P50,
        p90 => P90,
        p99 => P99
     };
poll_metric(gauge, Ref) ->
    counters:get(Ref, 1).
