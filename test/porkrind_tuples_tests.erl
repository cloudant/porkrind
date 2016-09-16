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

-module(porkrind_tuples_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").


has_tuple_size_test() ->
    ?assert_that(porkrind:describe(has_tuple_size(1)), is_list()),
    ?assert_that({}, has_tuple_size(0)),
    ?assert_that({a,b}, has_tuple_size(2)),
    ?assertError({assertion_failed, _}, ?assert_that(foo, has_tuple_size(0))),
    ?assertError({assertion_failed, _}, ?assert_that({}, has_tuple_size(1))),
    ?assertError({assertion_failed, _}, ?assert_that({a,b}, has_tuple_size(0))).


has_element_at_test() ->
    ?assert_that({a, b}, has_element_at(1, a)),
    ?assert_that({a, b}, has_element_at(2, is(b))),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(foo, has_element_at(1, a))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that({a, b}, has_element_at(1, b))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that({a, b}, has_element_at(3, thing))
    ).
