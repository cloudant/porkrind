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

-module(porkrind_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").


is_describe_test() ->
    ?assert_that(porkrind:describe(is(1)), is_list()).


is_not_test() ->
    ?assert_that(1, is_not(2)),
    ?assertError({assertion_failed, _}, ?assert_that(1, is_not(1))).


anything_test() ->
    ?assert_that(1, is(anything())),
    ?assert_that(porkrind:describe(anything()), is_list()).


comparison_test() ->
    Items = [
        foo,
        bar,
        <<"foo">>,
        <<"bar">>,
        <<0:1>>,
        <<1:1>>,
        1,
        2,
        1.1,
        2.2,
        fun erlang:system_info/1,
        fun erlang:is_list/1,
        [],
        [foo],
        [bar],
        self(),
        whereis(error_logger),
        hd(erlang:ports()),
        hd(tl(erlang:ports())),
        erlang:make_ref(),
        erlang:make_ref(),
        {},
        {foo, bar},
        {baz, bam, bing}
    ],
    Pairs = [
        {fun porkrind_logic:equal_to/1, fun erlang:'=='/2},
        {fun porkrind_logic:exactly_equal_to/1, fun erlang:'=:='/2},
        {fun porkrind_logic:lt/1, fun erlang:'<'/2},
        {fun porkrind_logic:less_than/1, fun erlang:'<'/2},
        {fun porkrind_logic:lteq/1, fun erlang:'=<'/2},
        {fun porkrind_logic:less_than_or_equal_to/1, fun erlang:'=<'/2},
        {fun porkrind_logic:gt/1, fun erlang:'>'/2},
        {fun porkrind_logic:greater_than/1, fun erlang:'>'/2},
        {fun porkrind_logic:gteq/1, fun erlang:'>='/2},
        {fun porkrind_logic:greater_than_or_equal_to/1, fun erlang:'>='/2}
    ],
    lists:foreach(fun({Matcher, Filter}) ->
        lists:foreach(fun(I1) ->
            lists:foreach(fun(I2) ->
                case Filter(I1, I2) of
                    true ->
                        ?assert_that(I1, Matcher(I2));
                    false ->
                        ?assertError(
                            {assertion_failed, _},
                            ?assert_that(I1, Matcher(I2))
                        )
                end
            end, Items)
        end, Items)
    end, Pairs).


close_to_test() ->
    ?assert_that(1.0, is(close_to(1.000000001))),
    ?assert_that(1, is(close_to(1))),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(1.0, is(close_to(2.0)))
    ),

    ?assert_that(1.0, is(close_to(2.0, 3.0))),
    ?assert_that(1, is(close_to(5, 6))).


all_of_test() ->
    Matchers = [is_integer(), is(gteq(1)), is(lteq(10))],
    ?assert_that(1, is(all_of(Matchers))),
    ?assert_that(5, is(all_of(Matchers))),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(foo, is(all_of(Matchers)))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(0, is(all_of(Matchers)))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(11, is(all_of(Matchers)))
    ).


all_of_combined_test() ->
    M1 = all_of([is_integer(), is(gt(1))]),
    M2 = all_of([is_atom(), is(lt(4))]),
    M3 = all_of([M1, M2]),

    M4 = all_of([is_integer(), is(gt(1)), is_atom(), is(lt(4))]),
    ?assert_that(M3, is(equal_to(M4))),
    ?assert_that(porkrind:describe(M3), is(porkrind:describe(M4))).



any_of_test() ->
    Matchers = [is_float(), foo, lt(5)],
    ?assert_that(1.1, is(any_of(Matchers))),
    ?assert_that(foo, is(any_of(Matchers))),
    ?assert_that(4, is(any_of(Matchers))),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(bar, is(any_of(Matchers)))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(6, is(any_of(Matchers)))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(self(), is(any_of(Matchers)))
    ).