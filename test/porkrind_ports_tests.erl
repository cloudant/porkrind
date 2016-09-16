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

-module(porkrind_ports_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").


not_a_port_test() ->
    ?assertError(
        {assertion_failed, _},
        ?assert_that(foo, has_port_property(id, "foo"))
    ).


has_id_test() ->
    with_udp_port(fun(Port) ->
        Info = erlang:port_info(Port),
        {id, Id} = lists:keyfind(id, 1, Info),
        ?assert_that(Port, has_id(Id)),
        ?assertError({assertion_failed, _}, ?assert_that(Port, has_id(Id - 1)))
    end).


has_name_test() ->
    with_udp_port(fun(Port) ->
        ?assert_that(Port, has_name("udp_inet")),
        ?assertError({assertion_failed, _}, ?assert_that(Port, has_name("foo")))
    end),
    ?assertError(function_clause, has_name(udp_inet)).


has_os_pid_test() ->
    with_udp_port(fun(Port) ->
        ?assert_that(Port, has_os_pid(undefined))
    end),

    ?assertError(function_clause, has_os_pid(foo)).


has_lock_level_test() ->
    with_udp_port(fun(Port) ->
        ?assert_that(Port, has_lock_level(port_level)),
        ?assertError(
            {assertion_failed, _},
            ?assert_that(Port, has_lock_level(driver_level))
        )
    end).


dead_port_test() ->
    Port = with_udp_port(fun(P) -> P end),
    ?assertError({assertion_failed, _}, ?assert_that(Port, has_id(5))).


with_udp_port(Fun) ->
    {ok, Port} = gen_udp:open(0),
    try
        Fun(Port)
    after
        gen_udp:close(Port)
    end.