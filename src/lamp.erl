-module(lamp).

-export([
    counter_inc/1,
    counter_inc/2,
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

-spec gauge_set(binary(), integer()) -> ok | {error, term()}.

gauge_set(Key, Value) ->
    {ok, Gauge} = get_metric(gauge, Key),
    counters:put(Gauge, 1, Value).

%% TODO: polling backend, for now only prints the values
%% TODO: Reset counters to 0 when polled
poll() ->
    Values = [{Key, Type, counters:get(Counter, 1)} || {Key, Type, Counter} <- get_metrics()],
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
make_metric(gauge) ->
    counters:new(1, [atomics]).
