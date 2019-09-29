-module(lamp).

-export([
    counter_inc/1,
    counter_inc/2,
    poll/0
]).

-spec counter_inc(binary(), integer()) -> ok | {error, term()}.

counter_inc(Key) ->
    counter_inc(Key, 1).

counter_inc(Key, Amt) ->
    Counter = get_counter(Key),
    counters:add(Counter, 1, Amt).

%% TODO: polling backend, for now only prints the values
%% TODO: Reset counters to 0 when polled
poll() ->
    Values = [{Key, counters:get(Counter, 1)} || {Key, Counter} <- get_counters()],
    io:format(user, "~p~n", [Values]).

% private

-spec get_counter(binary()) -> counters:counters_ref() | {error, term()}.

% returns an existing counter ref or create a new one and returns its reference
get_counter(Key) ->
    case persistent_term:get({?MODULE, Key}, key_not_found) of
        key_not_found ->
            CounterRef = counters:new(1, [atomics]),
            % maybe write_concurrency would be better, but it comes at the
            % price of potential "read inconsistency" and memory usage.
            % Must investigate.
            % Doc says add and sub could benefit from it, but not put and get,
            % so gauges will most likely stay atomic.
            persistent_term:put({?MODULE, Key}, CounterRef),
            CounterRef;
        CounterRef ->
            CounterRef
    end.

get_counters() ->
    % persistent term contains things unrelated to our counters that are filtered here
    Counters = lists:filter(fun(Elem) ->
        case Elem of
            {{?MODULE, _Key}, _Counter} ->
                true;
            _ ->
                false
        end
        end, persistent_term:get()),
    % trimming the returned term to be of the form {Key, CounterRef}
    lists:map(fun({{_Module, Key}, Val}) -> {Key, Val} end, Counters).
