-module(lamp).

-export([
    counter_inc/1,
    counter_inc/2,
    gauge_set/2,
    poll/0
]).

-spec counter_inc(binary(), integer()) -> ok | {error, term()}.

counter_inc(Key) ->
    counter_inc(Key, 1).

counter_inc(Key, Amt) ->
    {ok, Counter} = get_counter(Key),
    counters:add(Counter, 1, Amt).

-spec gauge_set(binary(), integer()) -> ok | {error, term()}.

gauge_set(Key, Value) ->
    {ok, Gauge} = get_gauge(Key),
    counters:put(Gauge, 1, Value).


%% TODO: polling backend, for now only prints the values
%% TODO: Reset counters to 0 when polled
poll() ->
    Values = [{Key, Type, counters:get(Counter, 1)} || {Key, Type, Counter} <- get_counters()],
    io:format(user, "~p~n", [Values]).

% private

-spec get_counter(binary()) -> counters:counters_ref() | {error, term()}.

% returns an existing counter ref or create a new one and returns its reference
get_counter(Key) ->
    case persistent_term:get({?MODULE, counter, Key}, key_not_found) of
        key_not_found ->
            case check_key(Key) of
                false ->
                    CounterRef = counters:new(1, [atomics]),
                    % maybe write_concurrency would be better, but it comes at the
                    % price of potential "read inconsistency" and memory usage.
                    % Must investigate.
                    % Doc says add and sub could benefit from it, but not put and get,
                    % so gauges will most likely stay atomic.
                    persistent_term:put({?MODULE, counter, Key}, CounterRef),
                    {ok, CounterRef};
                true ->
                    {error, <<"key already registered">>}  %% TODO: specify which key
            end;
        CounterRef ->
            {ok, CounterRef}
    end.

get_gauge(Key) ->
    case persistent_term:get({?MODULE, gauge, Key}, key_not_found) of
        key_not_found ->
            case check_key(Key) of
                false ->
                    GaugeRef = counters:new(1, [atomics]),
                    persistent_term:put({?MODULE, gauge, Key}, GaugeRef),
                    {ok, GaugeRef};
                true ->
                    {error, <<"key already registered">>}
            end;
        GaugeRef ->
            {ok, GaugeRef}
    end.

get_counters() ->
    % persistent term contains things unrelated to our counters that are filtered here
    Counters = lists:filter(fun(Elem) ->
        case Elem of
            {{?MODULE, _Type, _Key}, _Counter} ->
                true;
            _ ->
                false
        end
        end, persistent_term:get()),
    % trimming the returned term to be of the form {Key, Type, CounterRef}
    lists:map(fun({{_Module, Type, Key}, Val}) -> {Key, Type, Val} end, Counters).

% This is not pretty, should be refactored in something readable
check_key(Key) ->
    case lists:filter(fun(Elem) ->
        case Elem of
            {{?MODULE, _Type, Key}, _Counter} ->
                true;
            _ ->
                false
        end
        end, persistent_term:get()) of
        [] ->
            false;
        _  ->
            true
    end.
