# Porkrind


A Hamcrest library for Erlang. Ham Crest. Pork ridge. Pork rind!

## About Hamcrest Matchers


[Hamcrest][hamcrest] was originally written in Java as a way to make unit testing more expressive for the intent of the test. That is, instead of writing something like such:

    ?assertEqual(MyValue, 1.0)

you would instead write something like:

    ?assert_that(MyValue, is(equal_to(1.0)))

The simplest examples tend to make that seem a little silly. Where Hamcrest truly shines is when matching complex terms. Consider something like such:

    Value = [
        {key, value},
        {numbers, [0, 5, 16, 18, 21, 30, 40, 80]}
    ],

    ?assert_that(Value, is(all_of([
        has_item({key, value}),
        has_item({numbers, only_contains([
            all_of([is_integer(), greater_than_or_equal_to(0)])
        ])})
    ])))

Or alternatively using a couple helpers:

    ?assert_that(Value, matches([
        {key, value},
        {numbers, [tail(is_positive())]}
    ]))


## How to Use Porkrind

Simply add the include to your tests:

    -include_lib("porkrind/include/porkrind.hrl").

This pulls in the `?assert_that/2` macro and imports all of the builtin matchers.


## Automatic Matcher Creation


Where it makes sense, any matcher argument can also be any term that is then wrapped with `is(equal_to(Term))`. Thus you can do things like:

    ?assert_that(5, is(any_of([4, 5, 6])))

without the need to wrap each of `4`, `5`, and `6` with `is(equal_to(N))`. And, as a shorthand, the atom `'_'` (i.e., an atom that is a single underscore) will be replaced with `anything()` so that you can write matches like:

    ?assert_that({error, something_broke}, matches({error, '_'}))


## Matcher Helpers

These helpers allow you to create matchers from nested lists and tuples. Both of the `matches*` and `matching*` versions are identical and only exist to help with readability. (I.e., `has_item(matching(...))` vs `has_item(matches(...))`)

    matches(Term)
    matching(Term)

    matches_inanyorder(Term)
    matching_inanyorder(Term)

Both of these helpers work by recursively applying either a `contains/1` or `contains_inanyorder/1` matcher to the contents of lists and tuples. For instance:

    matches({foo, [a, b]})

would translate to:

    contains({is(equal_to(foo)), contains([is(equal_to(a), is(equal_to(b))])})

This is necessary when some elements are matchers, for instance:

    matches({foo, [is_atom(), is_number()]})

What is not obvious here is that this check would fail:

    ?assert_that({foo, [a, 1]}, contains({foo, [is_atom(), is_number()]}))

because the contains matcher is not recursively applied. I.e., it would end up making this check and failing:

    ?assert_that(
        [a, 1],
        is(equal_to(#'porkrind.matcher'{...}, #'porkrind.matcher'{...})
    )

Thus we have the `matches/1` family of helpers to recursively ensure everything is properly converted to a matcher.

A special helper that we can use in conjunction with contains and matches is the special `tail/1` matcher. If this exists as the last element of a list then it will succeed when either the tail of the list being matched is empty or all elements in it successfully match Matcher.

So something like this would succeed:

    ?assert_that([a, b, 1, 2, 3], matches([a, b, tail(is_integer())]))

You could also write `matches([tail(Matcher)])` to assert that all elements in the list match Matcher though that would be better written using `only_contains([Matcher])`.


### Matching Records

One of the nice uses for `matches` is that you can quite easily use it to build matchers for any record. For example:


	-record(foo, {
	    id = <<"whee">>,
	    count = 3,
	    items = [a, b, c],
	    ignored = i_dont_care,
	    also_ignored = still_dont_care
	}).

	RecordMatcher = #foo{
		id = is_binary(),
		count = all_of([is_integer(), is_positive()]),
		items = has_item(a),
		_ = '_'
	},

	?assert_that(#foo{}, matches(RecordMatcher)). % Succeeds



### Basic Type Assertions

| Matcher | Description |
| :-- | :-- |
| `is_atom()` | Matches any atom |
| `is_binary()` | Matches any binary (not bitstrings) |
| `is_bitstring()` | Matches any bitstring (including binaries) |
| `is_boolean()` | Matches the atoms `true` and `false` |
| `is_integer()` | Matches any integer |
| `is_float()` | Matches any floating point number |
| `is_fun()` | Matches any function |
| `is_fun(Arity)` | Matches any function with the specified Arity |
| `is_list()` | Matches any list |
| `is_negative()` | Matches any number less than zero |
| `is_non_negative()` | Matches any number greater than or equal to zero |
| `is_number()` | Matches any integer or float |
| `is_pid()` | Matches any process |
| `is_port()` | Matches any port |
| `is_positive()` | Matches any number greater than zero |
| `is_ref()` | Matches any reference |
| `is_string()` | Matches anything that is accepted by `erlang:iolist_to_binary/1` |
| `is_tuple()` | Matches any tuple |


### Common Atoms

| Matcher | Description |
| :-- | :-- |
| `is_true()` | Matches the atom `true` |
| `is_false()` | Matches the atom `false` |
| `is_undefined()` | Matches the atom `undefined` |
| `is_null()` | Matches the atom `null` |


### Logical Matchers


| Matcher | Description |
| :-- | :-- |
| `is(Matcher`) | Matches exactly the same as `Matcher`, exists for readability |
| `is_not(Matcher)` | Matches the logical inverse of `Matcher` |
| `anything()` | Matches anything |
| `equal_to(Term)` | Matches `Term` using the `==` operator |
| `exactly_equal_to(Term)` | Matches `Term` using the `=:=` operator |
| `greater_than(Term)` | Matches `Term` using the `>` operator |
| `gt(Term)` | Alias for `greater_than(Term)` |
| `greater_than_or_equal_to(Term)` | Matches `Term` using the `>=` operator |
| `gteq(Term)` | Alias for `greater_than_or_equal_to(Term)` |
| `less_than(Term)` | Matches `Term` using the `<` operator |
| `lt(Term)` | Alias for `less_than(Term)` |
| `less_than_or_equal_to(Term)` | Matches `Term` using the `=<` operator |
| `lteq(Term)` | Alias for `less_than_or_equal_to(Term)` |
| `close_to(Number, Delta)` | Matches `Number` withing a distance of `Delta`, useful for floating point math |
| `close_to(Number)` | Matches `Number` within a distance of `0.000001` |
| `all_of(List)` | Matches the logical `AND` of all matchers in `List` |
| `any_of(List)` | Matches the logical `OR` of all matchers in `List` |


### String Matchers

The string matchers apply to anything that is accepted by `erlang:iolist_to_binary/1`. There is no attempt at interpreting or accounting in anyway for Unicdoe characters. All operations use basic  ASCII and 8-bit characters. Patches welcome.


| Matcher | Description |
| :-- | :-- |
| `has_length(Integer)` | Matches any string with length `Integer` |
| `contains_string(String)` | Matches any string that has `String` as a substring |
| `starts_with(String)` | Matches any string that has `String` as a prefix |
| `ends_with(String)` | Matches any string that has `String` as a suffix |
| `equal_to_string(String)` | Matches any string equal to `String` |
| `equal_ignoring_case(String)` | Matches any string that is equivalent to `String` after a to_lower on both |
| `equal_ignoring_whitespace(String)` | Matches any string that is equivalent to `String` after any `$\n`, `$\r`, `$\t`, and `$\s` characters have been removed from both |
| `matches_re(String)` | Matches any string that satisifies the regular expression, `String` |
| `matches_re(String, ReOpts)` | Matches any string that satisifies the regular expression, `String` using options `ReOpts` |
| `string_contains_in_order(List)` | Matches any string that contains each element of `List` in order without overlaps |


### List Specific Tests

| Matcher | Description |
| :-- | :-- |
| `has_item(Matcher)` | Matches any list that has at least one element matching `Matcher` |
| `has_items(List)` | Matches any list that has at least one element matching each matcher in `List`, items in the list can satisfy multiple matchers in `List` |
| `empty()` | Matches the empty list |


### Tuple Matchers


| Matcher | Description |
| :-- | :-- |
| `has_tuple_size(Integer)` | Matches any tuple with size `Integer` |
| `has_element_at(Integer, Matcher)` | Matches any tuple that has an element at index `Integer` that matches `Matcher`, the index is 1-based to behave the same as `erlang:element/2` |


### Container (Tuples or Lists) Matchers

Each of the container matchers will accept either a list or tuple of matchers. Whichever type is passed dictates what type the term being matched must have. I.e., if you pass a list, it matches a list and will fail to match a tuple.

    ?assert_that({a, b}, contains({is_atom(), is_atom()})) % Passes
    ?assert_that({a, b}, contains([is_atom(), is_atom()])) % Fails

Notice that the second example uses a list when creating the contains matcher.


| Matcher | Description |
| :-- | :-- |
| `contains(ListOrTuple)` | Matches any term that has an element that matches the corresponding matcher in `ListOrTuple` |
| `contains_inanyorder(ListOrTuple)` | Matches any term that has elements matching each matcher in `ListOrTuple`, elements in the term are removed after a match, `ListOrTuple` is applied left to right |
| `only_contains(Term)` | Matches any term that only contains elements that match any matcher in `Term`, if `Term` is not a list or tuple, it is treated as a matcher and can match either a list or tuple |



### Function Call Matchers


The `does_not_raise/0` and `raises/0,1,2` matchers both require that the
term being matched is either a zero-arity function or the return value from:

    calling(Module, Function, ArgList)

which is an auto-imported function from Porkrind. Thus you can use either of these two approaches:

    ?assert_that(calling(mymod, myfun, [a, b]), raises(error))
    ?assert_that(fun() -> mymod:myfun(a, b) end, raises(error))


| Matcher | Description |
| :-- | :-- |
| `does_not_raise()` | Matches any function that when called does not raise an exception |
| `raises()` | Matches any function that when called raises an exception |
| `raises(Type)` | Matches any function that when called raises an exception with type equal to `Type` |
| `raises(Type, ReasonMatcher)` | Matches any function that when called raises an exception with type equal to `Type` and a reason that matches `ReasonMatcher`, `Type` can be the atom `'_'` to match any exception type if for some reason there is a desire to only match the exception reason |


### Pid and Port Matchers

Note that the monitor related matches will only work for Ports if your Erlang VM is new enough (19.0 or newer) to support that.

| Matcher | Description |
| :-- | :-- |
| `registered_as_anything()` | Matches any process or port that is registered |
| `registered_as(Name)` | Matches any process or port that is registered as `Name` |
| `has_link(Pid)` | Matches any process or port that is linked to `Pid` |
| `is_linked()` | Matches any process or port that has one or more links |
| `is_not_linked() ` | Matches any process or port that has zero links |
| `has_monitor(PidOrPort)` | Matches any process or port that has a monitor for `PidOrPort` |
| `is_monitoring()` | Matches any process or port that has one or more monitors |
| `is_not_monitoring()` | Matches any process or port with zero monitors |
| `is_monitored_by(Pid)` | Matches any process or port that is monitored by `Pid` |
| `is_monitored()` | Matches any process or port that is monitored by one or more processes |
| `is_not_monitored()` | Matches any process or port that has zero processes monitoring it |


### Pid Matchers

| Matcher | Description |
| :-- | :-- |
| `is_alive()` | Matches any process that is alive |
| `is_dead()` | Matches any process that is not alive |
| `is_in_function(Module, Function)` | Matches any process currently in `Module:Function` |
| `is_in_function(Module, Function, Arity)` | Matches any process currently in `Module:Function/Arity` |
| `has_initial_function(Module, Function)` | Matches any process with initial call `Module:Function` |
| `has_initial_function(Module, Function, Arity)` | Matches any process with initial call `Module:Function/Arity` |
| `has_no_messages()` | Matches any process with an empty mailbox |
| `has_messages()` | Matches any process that has one or more messages in its mailbox |
| `has_messages(Integer)` | Matches any process with `Integer` messages in its mailbox |
| `has_message(Matcher)` | Matches any process that has a message matching `Matcher` in its mailbox |
| `is_trapping_exits()` | Matches any process that is trapping process exits |
| `is_not_trapping_exits()` | Matches any process that is not trapping process exits |
| `has_pdict_key(KeyTerm)` | Matches any process that has a process dictionary entry for key `KeyTerm` |
| `has_pdict_entry(KeyTerm, ValueTerm)` | Matches any process that has a process dictionary entry for key `KeyTerm` with value equal to `ValueTerm` |


### Port Matchers

| Matcher | Description |
| :-- | :-- |
| `has_id(Id)` | Matches any port with id equal to `Id` |
| `has_name(Name)` | Matches an port with name equal to `Name` |
| `has_os_pid(OsPid)` | Matches any port with an os process id equal to `OsPid` |
| `has_lock_level(Level)` | Matches any port with a lock level equal to `Level` |
| `has_port_propert(Name, Value)` | Matches any port with a property `Name` equal to `Value` |


### JSON Matchers

Note that these JSON matchers all match [Jiffy's][jiffy] default output from decoding JSON. I wrote Jiffy so I'm a bit partial. Also of note is that these JSON matchers also properly convert and compare binaries and atoms. Thus these two checks are both sucessful:

	?assert_that({[{foo, bar}]}, has_json_key(<<"foo">>)) % Success
	?assert_that({[{<<"foo">>, bar}]}, has_json_key(foo)) % Success

| Matcher | Description |
| :-- | :-- |
| `is_json_object()` | Matches a term if it is a valid JSON object, this does not apply recursively |
| `is_json_array()` | Matches a term if it is a valid JSON array, this does not apply recursively |
| `is_json_equal_to(EJson)` | Matches a term if it is JSON-equivalent to `EJson`, where JSON-equivalent means that keys can be any order and binaries and atoms are compared after conversion |
| `has_json_key(Name)` | Matches any JSON object with a key JSON-equal to `Name` |
| `has_json_path(Path)` | Matches any JSON object that contains a path matching `Path`, where `Path` is a list of binaries or atoms that are successfully retrieved from nested JSON objects |
| `has_json_matching(Key, Matcher)` | Matches any JSON object that has a key JSON-equal to `Key` with a value matching `Matcher` |
| `has_json_matching_at(Path, Matcher)` | Matches any JSON object that has a path `Path` with a value matching `Matcher` |
| `has_json_entry(Key, Value)` | Matches any JSON object that has a key JSON-equal to `Key` with value JSON-equal to `Value` |
| `has_json_entry_at(Path, Key, Value)` | Matches any JSON object that has a JSON object at path `Path` with key JSON-equal to `Key` with value JSON-equal to `Value` |
| `has_json_entries(List)` | Matches any JSON object that has a key and value JSON-equal to the first and second element respectively of each two-tuple in `List` |
| `has_json_entries_at(Path, List)` | Matches an JSON object that has a JSON object at path `Path` that matches `has_json_entries(List)` |
| `has_json_value(Value)` | Matches any JSON object that contains a value JSON -equal to `Value` for one or more keys |


## Custom Matchers

Writing your own matcher is easy. Here is a complete example:

    my_matcher() ->
        #'porkrind.matcher'{
            name = my_matcher,
            args = [],
            match = fun(Value) ->
                % To fail a match you do:
                ?PR_FAIL({some_reason, Value})
                % To not fail, simply don't raise an exception
            end,
            reason = fun({some_reason, Value}) ->
                % Anything passed to ?PR_FAIL/1 will be given
                % to your reason function so that you can
                % create a helpful message for the test failure
                % output.
                %
                % If your matcher does not call ?PR_FAIL/1 then
                % you do not need to define a reason funciton as
                % it will never be called.
                io_lib:format("~p did not match my matcher", [Value])
            end
        }.

If you're interested in some slightly more complicated examples you should start by looking at the `all_of` and `any_of` matchers in `porkrind_logic.erl` and then the `contains` and `contains_inanyorder` matchers in `porkrind_containers.erl`.


[hamcrest]: http://hamcrest.org/
[jiffy]: http://github.com/davisp/jiffy
