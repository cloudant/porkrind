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

-module(porkrind_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").


simple_assert_that_test() ->
    ?assert_that(5, is(equal_to(5))).


simple_assert_that_failure_test() ->
    ?assertError({assertion_failed, _}, ?assert_that(5, is(equal_to(4)))).


bad_matcher_test() ->
    Matcher = #'porkrind.matcher'{
        name = bad_matcher,
        args = [],
        match = fun(_) ->
            erlang:error(foo)
        end
    },

    {assertion_failed, Info} = porkrind:check(foo, Matcher),
    ?assert_that(Info, has_item({error, {error, foo}})),
    ?assertError({badmatcher, Matcher}, porkrind:reason(Matcher, bling)),

    ?assertError({badarg, _}, porkrind:describe(#'porkrind.matcher'{})).
