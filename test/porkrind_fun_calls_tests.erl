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

-module(porkrind_fun_calls_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").


calling_test() ->
    ?assert_that(
        calling(erlang, length, [[]]),
        is({apply, erlang, length, [[]]})
    ).


does_not_raise_test() ->
    ?assert_that(calling(erlang, length, [[]]), does_not_raise()),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(calling(erlang, length, [foo]), does_not_raise())
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(foo, does_not_raise())
    ).


raises_test() ->
    ?assert_that(
        calling(erlang, length, [foo]),
        raises()
    ),
    ?assert_that(fun() -> lists:suffix(foo, []) end, raises()).


raises_type_test() ->
    ?assert_that(
        calling(erlang, length, [foo]),
        raises(error)
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(
            calling(erlang, length, [foo]),
            raises(throw)
        )
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(
            calling(erlang, length, [[]]),
            raises(error)
        )
    ).


raises_reason_test() ->
    ?assert_that(
        calling(erlang, length, [foo]),
        raises(error, badarg)
    ),
    ?assert_that(
        calling(erlang, length, [foo]),
        raises(error, is_atom())
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(
            calling(erlang, length, [foo]),
            raises(error, function_clause)
        )
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(
            calling(erlang, length, [foo]),
            is_integer()
        )
    ).


raises_errors_test() ->
    ?assertError(
        {assertion_failed, _},
        ?assert_that(fun(_) -> ok end, raises())
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(fun() -> ok end, raises())
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(fun() -> ok end, raises(error, is_integer()))
    ).
