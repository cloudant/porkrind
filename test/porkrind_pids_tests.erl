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

-module(porkrind_pids_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").


-export([
    init/1
]).


%% has_no_messages/0,
%% has_messages/0,
%% has_message/1,
%%
%% is_trapping_exits/0,
%% is_not_trapping_exits/0,
%%
%% has_pdict_key/1,
%% has_pdict_entry/2


init(Parent) ->
    process_flag(trap_exit, true),
    do_receive(Parent).


do_receive(Parent) ->
    Parent ! {self(), awake},
    put(foo, bar),
    receive _ -> ok end.


liveness_test() ->
    {Pid, Ref} = spawn_monitor(fun() -> receive _ -> ok end end),

    ?assert_that(Pid, is_alive()),
    ?assertError({assertion_failed, _}, ?assert_that(Pid, is_dead())),

    Pid ! close,
    receive {'DOWN', Ref, _, _, _} -> ok end,

    ?assertError({assertion_failed, _}, ?assert_that(Pid, is_alive())),
    ?assert_that(Pid, is_dead()).


function_state_test() ->
    {Pid, Ref} = spawn_monitor(?MODULE, init, [self()]),
    receive {Pid, awake} -> ok end,

    ?assert_that(Pid, is_in_function(porkrind_pids_tests, do_receive)),
    ?assert_that(Pid, is_in_function(porkrind_pids_tests, do_receive, 1)),
    ?assert_that(Pid, has_initial_function(porkrind_pids_tests, init)),
    ?assert_that(Pid, has_initial_function(porkrind_pids_tests, init, 1)),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, is_in_function(erlang, system_info))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, is_in_function(porkrind_pids_tests, do_receive, 3))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_initial_function(erlang, system_info))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_initial_function(porkrind_pids_tests, init, 4))
    ),

    Pid ! close,
    receive {'DOWN', Ref, _, _, _} -> ok end,

    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, is_in_function(porkrind_pids_tests, do_receive))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_initial_function(porkrind_pids_tests, init))
    ).


mailbox_test() ->
    {Pid, Ref} = spawn_monitor(fun() -> receive close -> ok end end),

    ?assert_that(Pid, has_no_messages()),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_messages())
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_messages(1))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_message(ohai))
    ),

    Pid ! ohai,

    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_no_messages())
    ),
    ?assert_that(Pid, has_messages()),
    ?assert_that(Pid, has_messages(1)),
    ?assert_that(Pid, has_message(ohai)),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_messages(2))
    ),

    Pid ! close,
    receive {'DOWN', Ref, _, _, _} -> ok end,

    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_no_messages())
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_messages(1))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_message(ohai))
    ).


trapping_exits_test() ->
    TrappingPid = spawn(?MODULE, init, [self()]),
    receive {TrappingPid, awake} -> ok end,
    ?assert_that(TrappingPid, is_trapping_exits()),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(TrappingPid, is_not_trapping_exits())
    ),
    TrappingPid ! close,

    {NotTrappingPid, Ref} = spawn_monitor(fun() -> receive _ -> ok end end),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(NotTrappingPid, is_trapping_exits())
    ),
    ?assert_that(NotTrappingPid, is_not_trapping_exits()),
    NotTrappingPid ! close,

    receive {'DOWN', Ref, _, _, _} -> ok end,

    ?assertError(
        {assertion_failed, _},
        ?assert_that(NotTrappingPid, is_trapping_exits())
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(NotTrappingPid, is_not_trapping_exits())
    ).


has_pdict_test() ->
    {Pid, Ref} = spawn_monitor(?MODULE, init, [self()]),
    receive {Pid, awake} -> ok end,

    ?assert_that(Pid, has_pdict_key(foo)),
    ?assert_that(Pid, has_pdict_entry(foo, bar)),

    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_pdict_key(bar))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_pdict_entry(foo, baz))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_pdict_entry(baz, bar))
    ),

    Pid ! close,
    receive {'DOWN', Ref, _, _, _} -> ok end,

    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_pdict_key(foo))
    ),
    ?assertError(
        {assertion_failed, _},
        ?assert_that(Pid, has_pdict_entry(foo, bar))
    ).
