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

-module(porkrind_json_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").


%% -export([
%%     is_json_object/0,
%%     is_json_array/0,
%%
%%     has_json_key/1,
%%     has_json_path/1,
%%
%%     has_json_entry/2,
%%     has_json_entry_at/2,
%%
%%     has_json_entries/1,
%%     has_json_entries_at/2,
%%
%%     has_json_value/1
%% ]).


is_json_object_test() ->
    ?assert_that({[]}, is_json_object()),
    ?assert_that({[{foo, bar}]}, is_json_object()),
    ?assert_that({[{<<"foo">>, <<"bar">>}]}, is_json_object()),
    ?assert_that({[{<<"foo">>, {[{<<"bar">>, <<"baz">>}]}}]}, is_json_object()),
    ?assert_that(test_obj(), is_json_object()),

    ?assertError(
        {assertion_failed, _},
        ?assert_that([], is_json_object())
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(2.0, is_json_object())
    ).


is_json_array_test() ->
    ?assert_that([], is_json_array()),
    ?assert_that([true, 1.0, {[]}], is_json_array()),

    ?assertError(
        {assertion_failed, _},
        ?assert_that({[]}, is_json_array())
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(false, is_json_array())
    ).


is_json_equal_test() ->
    Success = [
        {true, true},
        {false, false},
        {null, null},
        {<<"ohai">>, <<"ohai">>},
        {<<"ohai">>, ohai},
        {ohai, <<"ohai">>},
        {ohai, ohai},
        {1, 1},
        {1, 1.0},
        {[], []},
        {[1, true, <<"ohai">>], [1.0, true, ohai]},
        {[{[]}], [{[]}]},
        {[[[[]]]], [[[[]]]]},
        {{[]}, {[]}},
        {{[{foo, 1}]}, {[{foo, 1}]}},
        {{[{foo, 1}]}, {[{<<"foo">>, 1.0}]}},
        {{[{<<"foo">>, 1}]}, {[{<<"foo">>, 1.0}]}},
        {{[{<<"foo">>, 1}]}, {[{foo, 1.0}]}},
        {{[{foo, [bar, {[]}]}]}, {[{foo, [<<"bar">>, {[]}]}]}},
        {test_obj(), test_obj()}
    ],

    Failures = [
        {true, <<"true">>},
        {<<"true">>, true},
        {false, <<"false">>},
        {<<"false">>, false},
        {null, <<"null">>},
        {<<"null">>, null},
        {1, 2},
        {1, 1.1},
        {2.0, 1},
        {[], [foo]},
        {[foo], []},
        {[foo, bar], [bar]},
        {{[]}, []},
        {[], {[]}},
        {[[[]]], [[[[]]]]},
        {{[{foo, bar}]}, {[]}},
        {{[]}, {[{foo, bar}]}},
        {{[{foo, bar}]}, {[{foo, baz}]}}
    ],

    lists:foreach(fun({A, B}) ->
        ?assert_that(A, is_json_equal_to(B))
    end, Success),

    lists:foreach(fun({A, B}) ->
        ?assertError(
            {assertion_failed, _},
            ?assert_that(A, is_json_equal_to(B))
        )
    end, Failures).


has_json_key_test() ->
    ?assert_that(test_obj(), has_json_key(<<"foo">>)),
    ?assert_that(test_obj(), has_json_key(foo)),
    ?assert_that(test_obj(), has_json_key(<<"baz">>)),
    ?assert_that(test_obj(), has_json_key(baz)),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(test_obj(), has_json_key(<<"video games">>))
    ),
    ?assertError(
        function_clause,
        ?assert_that(test_obj(), has_json_key(2.0))
    ).


has_json_path_test() ->
    ?assert_that(test_obj(), has_json_path([path])),
    ?assert_that(test_obj(), has_json_path([path, to])),
    ?assert_that(test_obj(), has_json_path([path, to, t])),
    ?assert_that(test_obj(), has_json_path([<<"path">>])),
    ?assert_that(test_obj(), has_json_path([<<"path">>, <<"to">>])),
    ?assert_that(test_obj(), has_json_path([<<"path">>, <<"to">>, <<"t">>])),


    ?assertError(function_clause, has_json_path(foo)),
    ?assertError({badarg, 42}, has_json_path([42])),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(test_obj(), has_json_path([movies, 'Alien']))
    ),
    ?assertError(
        {badarg, 2.0},
        ?assert_that(test_obj(), has_json_path([moviews, 2.0]))
    ).


has_json_matching_test() ->
    ?assert_that(test_obj(), has_json_matching(foo, is_binary())),
    ?assert_that(test_obj(), has_json_matching(foo, <<"bar">>)),
    ?assert_that(test_obj(), has_json_matching(bang, [true, 'a thing', [bar]])),
    ?assert_that(test_obj(), has_json_matching(bang, is_json_array())),
    ?assert_that(test_obj(), has_json_matching(foo, is_string())),
    ?assert_that(test_obj(), has_json_matching(int, 1)),
    ?assert_that(test_obj(), has_json_matching(int, lt(5))),
    ?assert_that(test_obj(), has_json_matching(float, 2)),
    ?assert_that(test_obj(), has_json_matching(float, 2.0)),

    ?assertError(function_clause, has_json_matching(2.0, foo)),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(test_obj(), has_json_matching(not_a_key, bam))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(test_obj(), has_json_matching(foo, bam))
    ).


has_json_matching_at_test() ->
    ?assert_that(test_obj(), has_json_matching_at([foo], is_binary())),
    ?assert_that(test_obj(), has_json_matching_at([foo], <<"bar">>)),
    ?assert_that(test_obj(), has_json_matching_at([bang], is_json_array())),
    ?assert_that(test_obj(), has_json_matching_at([path], is_json_object())),
    ?assert_that(test_obj(), has_json_matching_at([path, to, t], true)),

    ?assertError(function_clause, has_json_matching_at(foo, bar)),
    ?assertError({badarg, 42}, has_json_matching_at([42], blammo)),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(test_obj(), has_json_matching_at([not_a_key], bam))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(test_obj(), has_json_matching_at([path, to, t], false))
    ).


has_json_entry_test() ->
    ?assert_that(test_obj(), has_json_entry(foo, <<"bar">>)),
    ?assert_that(test_obj(), has_json_entry(foo, bar)),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(test_obj(), has_json_entry(foo, 2.0))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(test_obj(), has_json_entry(not_a_key, nothing))
    ).


has_json_entry_at_test() ->
    ?assert_that(test_obj(), has_json_entry_at([], foo, bar)),
    ?assert_that(test_obj(), has_json_entry_at([path, to], a, an_atom)),
    ?assert_that(test_obj(), has_json_entry_at([path, to], a, <<"an_atom">>)),
    ?assert_that(test_obj(), has_json_entry_at([path, to], i, 7)),
    ?assert_that(test_obj(), has_json_entry_at([path, to], i, 7.0)),
    ?assert_that(test_obj(), has_json_entry_at([path, to], r, 13.5)),

    ?assertError({badarg, 42}, has_json_entry_at([42], question, 'LtUaE')),
    ?assertError(function_clause, has_json_entry_at(foo, bar, self())),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(test_obj(), has_json_entry_at([path, to], a, red))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(test_obj(), has_json_entry_at([path, to], f, true))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(test_obj(), has_json_entry_at([foo], bar, baz))
    ).


has_json_entries_test() ->
    ?assert_that(test_obj(), has_json_entries([{foo, bar}, {baz, bam}])),

    ?assertError(function_clause, has_json_entries(foo)),
    ?assertError({badarg, 42}, has_json_entries([42])),
    ?assertError(function_clause, has_json_entries([])),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(test_obj(), has_json_entries([{bling, blong}]))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(test_obj(), has_json_entries([{foo, bar}, {blink, blunk}]))
    ).


has_json_entries_at_test() ->
    ?assert_that(test_obj(), has_json_entries_at([], [{foo, bar}, {baz, bam}])),
    ?assert_that(test_obj(), has_json_entries_at([path, to], [
        {t, true},
        {a, an_atom}
    ])),

    ?assertError(function_clause, has_json_entries_at(foo, [{foo, bar}])),
    ?assertError(function_clause, has_json_entries_at([], foo)),
    ?assertError(function_clause, has_json_entries_at([], [])),
    ?assertError({badarg, 42}, has_json_entries_at([42], [{foo, bar}])),
    ?assertError({badarg, 42}, has_json_entries_at([], [42])),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(test_obj(), has_json_entries_at([path], [{bling, blong}]))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(
            test_obj(),
            has_json_entries_at([path, to], [{t, true}, {not_a_key, blammo}])
        )
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(
            test_obj(),
            has_json_entries_at([path, to, a, thing], [{foo, bar}])
        )
    ).


has_json_value_test() ->
    ?assert_that({[{foo, bar}]}, has_json_value(bar)),
    ?assertError(
        {assertion_failed, _},
        ?assert_that({[]}, has_json_value(bar))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that({[{foo, bar}]}, has_json_value(baz))
    ),

    ?assert_that(porkrind:describe(has_json_value(bar)), is_list()).


test_obj() ->
    {[
        {<<"foo">>, <<"bar">>},
        {baz, <<"bam">>},
        {bang, [true, 'a thing', [bar]]},
        {int, 1},
        {float, 2.0},
        {path, {[
            {to, {[
                {<<"t">>, true},
                {<<"f">>, false},
                {<<"n">>, null},
                {<<"a">>, an_atom},
                {<<"i">>, 7},
                {<<"r">>, 13.5}
            ]}}
        ]}}
    ]}.
