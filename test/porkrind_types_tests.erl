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

-module(porkrind_types_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").


check_boolean(A) when A == true; A == false ->
    true;

check_boolean(_) ->
    false.


check_string(A) ->
    try iolist_to_binary(A) of
        B when is_binary(B) -> true;
        _ -> false
    catch _:_ ->
        false
    end.


funs() ->
    [
        is_atom,
        is_binary,
        is_bitstring,
        {is_boolean, fun check_boolean/1},
        is_integer,
        is_float,
        {is_fun, is_function},
        is_list,
        {is_negative, fun(N) -> is_number(N) andalso N < 0 end},
        {is_non_negative, fun(N) -> is_number(N) andalso N >= 0 end},
        is_number,
        is_pid,
        is_port,
        {is_positive, fun(N) -> is_number(N) andalso N > 0 end},
        {is_ref, is_reference},
        {is_string, fun check_string/1},
        is_tuple
    ].


matchers() ->
    lists:map(fun
        ({F, _}) ->
            porkrind_types:F();
        (F) ->
            porkrind_types:F()
    end, funs()).


filters() ->
    lists:map(fun
        ({_, F}) when is_atom(F) ->
            fun erlang:F/1;
        ({_, F}) when is_function(F, 1) ->
            F;
        (F) ->
            fun erlang:F/1
    end, funs()).


items() ->
    [
        foo,
        true,
        false,
        <<"foo">>,
        <<1:1>>,
        -1,
        -1.1,
        0,
        0.0,
        42,
        3.14,
        fun erlang:system_info/1,
        fun() -> ok end,
        fun(_A, _B) -> ok end,
        [],
        [a, b, c],
        self(),
        hd(erlang:ports()),
        erlang:make_ref(),
        {},
        {a, b, c}
    ].


type_test() ->
    lists:foreach(fun(Item) ->
        lists:foreach(fun({Filt, Matcher}) ->
            ?assert_that(porkrind:describe(Matcher), is_list()),
            case Filt(Item) of
                true ->
                    ?assert_that(Item, Matcher);
                false ->
                    ?assertError(
                        {assertion_failed, _},
                        ?assert_that(Item, Matcher)
                    )
            end
        end, lists:zip(filters(), matchers()))
    end, items()).


fun_arity_test() ->
    ZeroFun = fun() -> ok end,
    OneFun = fun(_) -> ok end,
    ?assert_that(ZeroFun, is_fun(0)),
    ?assert_that(OneFun, is_fun(1)),
    ?assertError({assertion_failed, _}, ?assert_that(foo, is_fun(1))),
    ?assertError({assertion_failed, _}, ?assert_that(ZeroFun, is_fun(1))),
    ?assertError({assertion_failed, _}, ?assert_that(OneFun, is_fun(0))).
