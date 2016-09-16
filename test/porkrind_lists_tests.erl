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

-module(porkrind_lists_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").


has_item_test() ->
    ?assert_that([1, foo, self()], has_item(is_pid())),
    ?assert_that([1, foo, self()], has_item(lt(5))),
    ?assert_that([1, foo, self()], has_item(foo)),

    ?assertError(
        {assertion_failed, _},
        ?assert_that([], has_item(is_float()))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that([1, foo, self()], has_item(is_float()))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that([1, foo, self()], has_item(is_fun()))
    ),

    ?assert_that(porkrind:describe(has_item(is_fun())), is_list()).


has_items_test() ->
    Items = [1, foo, self(), erlang:make_ref()],
    ?assert_that(Items, has_items([1, foo])),
    ?assert_that(Items, has_items(Items)),
    ?assert_that(Items, has_items([is_integer()])),
    ?assert_that(Items, has_items([is_integer(), foo])),

    % A value can match multiple matchers
    Matchers = [is_integer() || _ <- lists:seq(1, 100)],
    ?assert_that(Items, has_items(Matchers)),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(Items, has_items([is_integer(), is_float()]))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Items, has_items([is_fun(), is_integer()]))
    ),

    ?assert_that(porkrind:describe(has_items([is_integer()])), is_list()).


empty_test() ->
    ?assert_that([], is(empty())),
    ?assert_that("", is(empty())),

    ?assertError(
        {assertion_failed, _},
        ?assert_that([a], is(empty()))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that("a", is(empty()))
    ),

    ?assert_that(porkrind:describe(is(empty())), is_list()).
