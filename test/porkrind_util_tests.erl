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

-module(porkrind_util_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").


maybe_wrap_wraps_value_test() ->
    Expect = porkrind_logic:equal_to(5),
    Value = porkrind_util:maybe_wrap(5),
    ?assertEqual(Expect, Value).


maybe_wrap_returns_anything_test() ->
    ?assertEqual(anything(), porkrind_util:maybe_wrap('_')).


maybe_wrap_doesnt_wrap_matcher_test() ->
    M = is_pid(),
    ?assertEqual(M, porkrind_util:maybe_wrap(M)).


find_first_match_test() ->
    Pairs = [
        {5, is_integer()},
        {self(),is_pid()},
        {foo, is_atom()}
    ],
    Matchers = [M || {_, M} <- Pairs],
    lists:foreach(fun({Val, M}) ->
        ?assertEqual({match, M}, porkrind_util:find_first_match(Val, Matchers))
    end, Pairs),
    ?assertEqual(nomatch, porkrind_util:find_first_match({}, Matchers)).


mfa_to_string_test() ->
    ?assertEqual("a:b/2", porkrind_util:mfa_to_string(a, b, [c, d])),
    ?assertEqual("e:f/1", porkrind_util:mfa_to_string(e, f, 1)),
    ?assertEqual("g:h/any", porkrind_util:mfa_to_string(g, h, '_')).


str_join_test() ->
    Result = porkrind_util:str_join(["a", "b"], " and ", "()"),
    ?assertEqual("(a and b)", lists:flatten(Result)).


vm_has_port_monitors_test() ->
    ?assert_that(porkrind_util:vm_has_port_monitors(), is_boolean()).
