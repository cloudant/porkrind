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

-module(porkrind_pids_or_ports_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").


-define(REGISTERED_NAME, porkrind_test_pid).


matchers() ->
    [
        is(registered_as_anything()),
        is(registered_as(foo)),
        has_link(self()),
        is_linked(),
        is_not_linked(),
        has_monitor(self()),
        is_monitoring(),
        is_not_monitored(),
        is_monitored_by(self()),
        is_monitored(),
        is_not_monitoring()
    ].


not_a_pid_or_port_test() ->
    lists:foreach(fun(M) ->
        ?assertError({assertion_failed, _}, ?assert_that(foo, M))
    end, matchers()).


dead_pid_test() ->
    {Pid, Ref} = spawn_monitor(fun() -> ok end),
    receive {'DOWN', Ref, _, _, _} -> ok end,
    lists:foreach(fun(M) ->
        ?assertError({assertion_failed, _}, ?assert_that(Pid, M))
    end, matchers()).


dead_port_test() ->
    {ok, Port} = gen_udp:open(0),
    gen_udp:close(Port),
    lists:foreach(fun(M) ->
        ?assertError({assertion_failed, _}, ?assert_that(Port, M))
    end, matchers()).


registered_as_anything_test() ->
    ?assert_that(whereis(error_logger), is(registered_as_anything())),
    Pid = spawn(fun() -> receive _ -> ok end end),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, is(registered_as_anything()))
    ),
    Pid ! close.


registered_as_test() ->
    ErrorLogger = whereis(error_logger),

    ?assert_that(ErrorLogger, is(registered_as(error_logger))),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(ErrorLogger, is(registered_as(code_server)))
    ),

    {Pid, Ref} = spawn_monitor(fun() -> receive _ -> ok end end),
    register(?REGISTERED_NAME, Pid),

    ?assert_that(Pid, is(registered_as(?REGISTERED_NAME))),

    Pid ! close,
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    end,

    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, is(registered_as(?REGISTERED_NAME)))
    ),

    {ok, Port} = gen_udp:open(0),
    try
        register(?REGISTERED_NAME, Port),
        ?assert_that(Port, is(registered_as(?REGISTERED_NAME)))
    after
        gen_udp:close(Port)
    end,

    gen_udp:close(Port),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(Port, is(registered_as(?REGISTERED_NAME)))
    ).


link_test() ->
    LinkedPid = spawn_link(fun() -> receive _ -> ok end end),

    ?assert_that(LinkedPid, has_link(self())),
    ?assert_that(self(), has_link(LinkedPid)),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(LinkedPid, has_link(whereis(error_logger)))
    ),

    ?assert_that(LinkedPid, is_linked()),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(LinkedPid, is_not_linked())
    ),

    LinkedPid ! close,

    UnlinkedPid = spawn(fun() -> receive _ -> ok end end),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(UnlinkedPid, has_link(self()))
    ),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(UnlinkedPid, is_linked())
    ),

    ?assert_that(UnlinkedPid, is_not_linked()),

    UnlinkedPid ! close.


monitor_pid_test() ->
    Self = self(),
    Pid = spawn(fun() ->
        Ref = erlang:monitor(process, Self),
        Self ! {self(), monitoring},
        receive _ -> ok end,
        erlang:demonitor(Ref, [flush]),
        Self ! {self(), not_monitoring},
        receive _ -> ok end
    end),

    receive {Pid, monitoring} -> ok end,

    ?assert_that(Pid, has_monitor(self())),
    ?assert_that(Pid, is_monitoring()),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, is_not_monitoring())
    ),

    Pid ! drop_monitor,

    receive {Pid, not_monitoring} -> ok end,

    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_monitor(self()))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, is_monitoring())
    ),
    ?assert_that(Pid, is_not_monitoring()),

    Pid ! close.


monitor_port_test() ->
    case porkrind_util:vm_has_port_monitors() of
        true ->
            Self = self(),
            {ok, Port} = gen_udp:open(0),
            Pid = spawn(fun() ->
                Ref = erlang:monitor(port, Port),
                Self ! {self(), monitoring},
                receive _ -> ok end,
                erlang:demonitor(Ref, [flush]),
                Self ! {self(), not_monitoring},
                receive _ -> ok end
            end),

            receive {Pid, monitoring} -> ok end,

            ?assert_that(Pid, has_monitor(Port)),
            ?assert_that(Pid, is_monitoring()),
            ?assertError(
                {assertion_failed, _},
                ?assert_that(Pid, is_not_monitoring())
            ),

            Pid ! drop_monitor,
            receive {Pid, not_monitoring} -> ok end,

            ?assertError(
                {assertion_failed, _},
                ?assert_that(Pid, has_monitor(Port))
            ),
            ?assertError(
                {assertion_failed, _},
                ?assert_that(Pid, is_monitoring())
            ),
            ?assert_that(Pid, is_not_monitoring()),

            Pid ! close;
        false ->
            ok
    end.


monitored_test() ->
    {MonitoredPid, _} = spawn_monitor(fun() -> receive _ -> ok end end),
    ?assert_that(MonitoredPid, is_monitored_by(self())),
    ?assert_that(MonitoredPid, is_monitored()),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(MonitoredPid, is_not_monitored())
    ),
    MonitoredPid ! close,

    UnmonitoredPid = spawn(fun() -> receive _ -> ok end end),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(UnmonitoredPid, is_monitored_by(self()))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(UnmonitoredPid, is_monitored())
    ),
    ?assert_that(UnmonitoredPid, is_not_monitored()),
    UnmonitoredPid ! close.
