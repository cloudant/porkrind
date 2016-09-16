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

-module(porkrind_atoms_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").


atom_test() ->
    ?assert_that(true, is_true()),
    ?assert_that(false, is_false()),
    ?assert_that(undefined, is_undefined()),
    ?assert_that(null, is_null()),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(false, is_true())
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(true, is_false())
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(null, is_undefined())
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(undefined, is_null())
    ).
