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

-module(porkrind_strings_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").


has_length_test() ->
    SuccessPairs = [
        {"", 0},
        {"f", 1},
        {"foobar", 6},

        {<<"">>, 0},
        {<<"f">>, 1},
        {<<"foobar">>, 6}
    ],

    FailurePairs = [
        {"", 1},
        {"foobar", 2},
        {<<"">>, 1},
        {<<"foobar">>, 2},
        {foo, 3}
    ],

    check_success(SuccessPairs, fun(E) -> has_length(E) end),
    check_failure(FailurePairs, fun(E) -> has_length(E) end).


has_length_describe_test() ->
    ?assert_that(porkrind:describe(has_length(0)), is_list()).


bitstring_not_a_string_test() ->
    ?assertError(
        {assertion_failed, _},
        ?assert_that(<<1:1>>, contains_string("foo"))
    ).


contains_string_test() ->
    SuccessPairs = [
        {"foobar", "f"},
        {"foobar", "o"},
        {"foobar", "b"},
        {"foobar", "r"},
        {"foobar", "ooba"},
        {"foobar", "foo"},
        {"foobar", "bar"},
        {"foobar", "foobar"},

        {<<"foobar">>, <<"f">>},
        {<<"foobar">>, <<"o">>},
        {<<"foobar">>, <<"b">>},
        {<<"foobar">>, <<"r">>},
        {<<"foobar">>, <<"ooba">>},
        {<<"foobar">>, <<"foo">>},
        {<<"foobar">>, <<"bar">>},
        {<<"foobar">>, <<"foobar">>}
    ],

    FailurePairs = [
        {"foobar", "zebra"},
        {"foobar", "raboof"},
        {"foobar", "rab"},
        {"foobar", "obo"},
        {"foobar", "bars"},

        {<<"foobar">>, <<"zebra">>},
        {<<"foobar">>, <<"raboof">>},
        {<<"foobar">>, <<"rab">>},
        {<<"foobar">>, <<"obo">>},
        {<<"foobar">>, <<"bars">>},

        {foobar, "ooba"},
        {foobar, <<"ooba">>}
    ],

    check_success(SuccessPairs, fun(E) -> contains_string(E) end),
    check_failure(FailurePairs, fun(E) -> contains_string(E) end).


starts_with_test() ->
    SuccessPairs = [
        {"foobar", "f"},
        {"foobar", "foo"},
        {"foobar", "foobar"},

        {<<"foobar">>, <<"f">>},
        {<<"foobar">>, <<"foo">>},
        {<<"foobar">>, <<"foobar">>}
    ],

    FailurePairs = [
        {"foobar", "zebra"},
        {"foobar", "foobarbaz"},

        {<<"foobar">>, <<"zebra">>},
        {<<"foobar">>, <<"foobarbaz">>},

        {foobar, "foo"},
        {foobar, <<"foo">>}
    ],

    check_success(SuccessPairs, fun(E) -> starts_with(E) end),
    check_failure(FailurePairs, fun(E) -> starts_with(E) end).


ends_with_test() ->
    SuccessPairs = [
        {"foobar", "r"},
        {"foobar", "bar"},
        {"foobar", "foobar"},

        {<<"foobar">>, <<"r">>},
        {<<"foobar">>, <<"bar">>},
        {<<"foobar">>, <<"foobar">>}
    ],

    FailurePairs = [
        {"foobar", "zebra"},
        {"foobar", "bazfoobar"},

        {<<"foobar">>, <<"zebra">>},
        {<<"foobar">>, <<"bazfoobar">>},

        {foobar, "bar"},
        {foobar, <<"bar">>}
    ],

    check_success(SuccessPairs, fun(E) -> ends_with(E) end),
    check_failure(FailurePairs, fun(E) -> ends_with(E) end).


equal_to_string_test() ->
    ?assert_that(<<"foo">>, is(equal_to_string("foo"))),
    ?assert_that(["foo bar"], is(equal_to_string(<<"foo bar">>))),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(<<"foo">>, is(equal_to_string("bar")))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(foo, is(equal_to_string("foo")))
    ).


equal_ignoring_case_test() ->
    Run = fun(Cases) ->
        lists:foreach(fun(C1) ->
            lists:foreach(fun(C2) ->
                ?assert_that(C1, is(equal_ignoring_case(C2)))
            end, Cases)
        end, Cases)
    end,

    Run([
        "foobar", "FooBar", "FOOBAR",
        <<"foobar">>, <<"FooBar">>, <<"FOOBAR">>
    ]),

    FailurePairs = [
        {"foobar", "ZeBra"},
        {<<"foobar">>, <<"ZeBra">>},
        {foobar, "FooBar"}
    ],

    check_failure(FailurePairs, fun(E) -> is(equal_ignoring_case(E)) end).


equal_ignoring_whitespace_test() ->
    Run = fun(Cases) ->
        lists:foreach(fun(C1) ->
            lists:foreach(fun(C2) ->
                ?assert_that(C1, is(equal_ignoring_whitespace(C2)))
            end, Cases)
        end, Cases)
    end,

    Cases = [
        "foobar",
        "f o\ro\tb\ta\rr\n",
        "foo\t\r\n bar",
        "\t\r\n foo bar \t\r\n"
    ],

    Run(Cases ++ lists:map(fun erlang:list_to_binary/1, Cases)),

    FailurePairs = [
        {"foobar", "ze bra\r\n\t"},
        {<<"foobar">>, <<"ze bra\r\n\t">>},
        {foobar, "foo bar"},
        {foobar, <<"foo bar">>}
    ],

    check_failure(FailurePairs, fun(E) -> is(equal_ignoring_whitespace(E)) end).


matches_re_test() ->
    SuccessPairs = [
        {"foobar", "foo"},
        {"foobar", "o[bc]a"},
        {"foobar", "foobar(baz)?"},

        {<<"foobar">>, <<"foo">>},
        {<<"foobar">>, <<"o[bc]a">>},
        {<<"foobar">>, <<"foobar(baz)?">>}
    ],

    FailurePairs = [
        {"foobar", "zebra"},
        {"foobar", "foo$"},

        {<<"foobar">>, <<"zebra">>},
        {<<"foobar">>, <<"foo$">>},

        {foobar, "foo"},
        {foobar, <<"foo">>}
    ],

    check_success(SuccessPairs, fun(E) -> matches_re(E) end),
    check_failure(FailurePairs, fun(E) -> matches_re(E) end),
    check_failure(FailurePairs, fun(E) -> matches_re(E, [unicode]) end),

    Opts = [{capture, none}, unicode],
    ?assert_that("foobar", matches_re("foo")),
    ?assert_that("foobar", matches_re("foo", Opts)),
    ?assert_that("foobar", matches_re("foo", [{capture, all, list}])),
    ?assert_that(porkrind:describe(matches_re("foo", Opts)), is_list()).


string_contains_in_order_test() ->
    Subjects = ["foobar", <<"foobar">>],
    SuccessPatterns = [
        ["f"],
        ["b"],
        ["r"],
        ["f", "b", "r"],
        ["foo"],
        ["bar"],
        ["foo", "bar"],
        ["fo", "ob", "ar"],
        ["f", "oobar"],
        ["fooba", "r"]
    ],

    FailurePatterns = [
        ["zebra"],
        ["b", "f"],
        ["bar", "foo"],
        ["foob", "obar"],
        ["ar", "fo"]
    ],

    SuccessPairs = lists:foldl(fun(S, Acc1) ->
        lists:foldl(fun(SP, Acc2) ->
            [{S, SP}, {S, lists:map(fun erlang:list_to_binary/1, SP)} | Acc2]
        end, Acc1, SuccessPatterns)
    end, [], Subjects),

    FailurePairs = lists:foldl(fun(S, Acc1) ->
        lists:foldl(fun(FP, Acc2) ->
            [{S, FP}, {S, lists:map(fun erlang:list_to_binary/1, FP)} | Acc2]
        end, Acc1, FailurePatterns)
    end, [], Subjects),

    check_success(SuccessPairs, fun(E) -> string_contains_in_order(E) end),
    check_failure(FailurePairs, fun(E) -> string_contains_in_order(E) end),

    ?assertError(
        {badarg, foo},
        string_contains_in_order([foo])
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(foo, string_contains_in_order(["foo"]))
    ).


check_success(Pairs, MakeMatcher) ->
    lists:foreach(fun({Value, Arg}) ->
        ?assert_that(Value, MakeMatcher(Arg))
    end, Pairs).


check_failure(Pairs, MakeMatcher) ->
    lists:foreach(fun({Value, Arg}) ->
        ?assertError(
            {assertion_failed, _},
            ?assert_that(Value, MakeMatcher(Arg))
        )
    end, Pairs).
