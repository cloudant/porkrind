% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(porkrind_containers_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").


-record(test_rec, {
    f1 = 1,
    f2 = foo,
    f3 = 4,
    f4 = {thing, here, []}
}).


matches_test() ->
    ?assert_that([a, 4], matches([a, 4])),
    ?assert_that({a, 4}, matches({a, 4})),
    ?assert_that([a, 4], matches([is_atom(), lt(5)])),
    ?assert_that({a, 4}, matches({is_atom(), lt(5)})),
    ?assert_that([{a, 4}, true], matches([{is_atom(), lt(5)}, is_boolean()])),
    ?assert_that({a, [4, true]}, matches({is_atom(), [lt(5), is_boolean()]})),

    ?assert_that(matches({a, 4}), is(exactly_equal_to(matching({a, 4})))),
    ?assert_that([b, {a, 4}, c], has_item(matching({a, 4}))),

    ?assert_that([1, 2, a, b, c, d], matches([1, 2, tail(is_atom())])),

    RecordMatcher = #test_rec{
        f1 = is_integer(),
        f2 = foo,
        f3 = lt(5),
        _ = '_'
    },

    ?assert_that(#test_rec{}, matches(RecordMatcher)).


matches_inanyorder_test() ->
    ?assert_that([a, 4], matches_inanyorder([4, a])),
    ?assert_that({a, 4}, matches_inanyorder({4, a})),
    ?assert_that({a, 4}, matches_inanyorder({is_atom(), is_integer()})),
    ?assert_that([a, {b, c}], has_item(matching_inanyorder({c, b}))).


badarg_test() ->
    % Its easy to accidentally pass a matcher to contains
    % instead of a list or tuple of matchers. Check that
    % we throw an error instead of trying to match the
    % matcher record.
    Matcher = is(anything()),
    ?assertError({badarg, Matcher}, contains(Matcher)),
    ?assertError({badarg, Matcher}, contains_inanyorder(Matcher)),
    ?assertError({badarg, Matcher}, only_contains(Matcher)).


contains_test() ->
    ListMatchers = [is_integer(), foo, lt(5)],
    TupleMatchers = {is_integer(), foo, lt(5)},

    ?assert_that([1, foo, 4], contains(ListMatchers)),
    ?assert_that({1, foo, 4}, contains(TupleMatchers)),

    ?assertError(
        {assertion_failed, _},
        ?assert_that([1, foo, 4], contains(TupleMatchers))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that({1,foo, 4}, contains(ListMatchers))
    ),

    Errors = [
        [1.1, foo, 4],
        [1, bar, 4],
        [1, foo, 6],
        [1, foo],
        [1, foo, 4, {}]
    ],

    lists:foreach(fun(Error) ->
        ?assertError(
            {assertion_failed, _},
            ?assert_that(Error, contains(ListMatchers))
        ),
        ?assertError(
            {assertion_failed, _},
            ?assert_that(list_to_tuple(Error), contains(TupleMatchers))
        )
    end, Errors),

    ?assert_that(porkrind:describe(contains(ListMatchers)), is_list()).


contains_tail_test() ->
    Data = [1, foo, 4, a, b, c, d, e, f],

    Matchers1 = [is_integer(), foo, lt(5), tail(is_atom())],
    ?assert_that(Data, contains(Matchers1)),

    Matchers2 = [1, tail(is_atom()), lt(5), a, b, c, d, e, f],
    ?assert_that(Data, contains(Matchers2)),

    ?assert_that(Data, contains([tail(is(anything()))])),
    ?assert_that([], contains([tail(is(anything()))])),

    ?assert_that(porkrind:describe(tail(is_atom())), is_list()).



contains_inanyorder_test() ->
    ListMatchers = [is_integer(), foo, lt(5)],
    TupleMatchers = {is_integer(), foo, lt(5)},

    Permutations = [
        [1, foo, 4],
        [1, 4, foo],
        [foo, 1, 4],
        [foo, 4, 1],
        [4, 1, foo],
        [4, foo, 1]
    ],

    lists:foreach(fun(Perm) ->
        ?assert_that(Perm, contains_inanyorder(ListMatchers)),
        ?assert_that(list_to_tuple(Perm), contains_inanyorder(TupleMatchers))
    end, Permutations),

    Errors = [
        [6.1, foo, 4],
        [1, bar, 4],
        [1, foo, 6],
        [1, foo, 4, {}]
    ],

    lists:foreach(fun(Error) ->
        ?assertError(
            {assertion_failed, _},
            ?assert_that(Error, contains_inanyorder(ListMatchers))
        ),
        ?assertError(
            {assertion_failed, _},
            ?assert_that(
                list_to_tuple(Error),
                contains_inanyorder(TupleMatchers)
            )
        )
    end, Errors),

    ?assert_that(
        porkrind:describe(contains_inanyorder(ListMatchers)),
        is_list()
    ).


only_contains_test() ->
    ListMatchers = [foo, lt(5)],
    TupleMatchers = {foo, lt(5)},

    ?assert_that([1, foo, 4, -10, foo], only_contains(ListMatchers)),
    ?assert_that({1, foo, 4, -10, foo}, only_contains(TupleMatchers)),

    ?assertError(
        {assertion_failed, _},
        ?assert_that([1, foo, 4, -10, bar], only_contains(ListMatchers))
    ),

    ?assertError(
        {assertion_failed, _},
        ?assert_that({1, foo, 4, -10, bar}, only_contains(TupleMatchers))
    ),

    ?assert_that(porkrind:describe(only_contains(ListMatchers)), is_list()).