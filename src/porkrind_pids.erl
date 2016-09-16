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

-module(porkrind_pids).

-include("porkrind_internal.hrl").


-export([
    is_alive/0,
    is_dead/0,

    is_in_function/2,
    is_in_function/3,
    has_initial_function/2,
    has_initial_function/3,

    has_no_messages/0,
    has_messages/0,
    has_messages/1,
    has_message/1,

    is_trapping_exits/0,
    is_not_trapping_exits/0,

    has_pdict_key/1,
    has_pdict_entry/2
]).


is_alive() ->
    M = #'porkrind.matcher'{
        name = is_alive,
        args = [],
        match = fun(Value) ->
            case erlang:is_process_alive(Value) of
                true ->
                    ok;
                false ->
                    ?PR_FAIL({dead_pid, Value})
            end
        end,
        reason = fun({dead_pid, Value}) ->
            io_lib:format("~w is not alive", [Value])
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_pid(),
        M
    ]).


is_dead() ->
    M = #'porkrind.matcher'{
        name = is_dead,
        args = [],
        match = fun(Value) ->
            case erlang:is_process_alive(Value) of
                true ->
                    ?PR_FAIL({live_pid, Value});
                false ->
                    ok
            end
        end,
        reason = fun({live_pid, Value}) ->
            io_lib:format("~w is a live process", [Value])
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_pid(),
        M
    ]).


is_in_function(Mod, Fun) when is_atom(Mod), is_atom(Fun) ->
    is_in_function_int({Mod, Fun, '_'}).


is_in_function(Mod, Fun, Arity)
        when is_atom(Mod), is_atom(Fun), is_integer(Arity) ->
    is_in_function_int({Mod, Fun, Arity}).


is_in_function_int({Mod, Fun, Arity}) ->
    M = #'porkrind.matcher'{
        name = is_in_function,
        args = [Mod, Fun, Arity],
        match = fun(Value) ->
            case erlang:process_info(Value, current_function) of
                {current_function, {Mod, Fun, Arity}} ->
                    true;
                {current_function, {Mod, Fun, _}} when Arity == '_' ->
                    true;
                undefined ->
                    ?PR_FAIL({dead_pid, Value});
                _ ->
                    ?PR_FAIL({not_current, Value})
            end
        end,
        reason = fun
            ({dead_pid, Value}) ->
                io_lib:format("~w is a dead pid", [Value]);
            ({not_current, Value}) ->
                FunStr = porkrind_util:mfa_to_string(Mod, Fun, Arity),
                Args = [Value, FunStr],
                io_lib:format("~w is not currently in function ~s", Args)
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_pid(),
        M
    ]).


has_initial_function(Mod, Fun) ->
    has_initial_function_int({Mod, Fun, '_'}).


has_initial_function(Mod, Fun, Arity) when is_integer(Arity) ->
    has_initial_function_int({Mod, Fun, Arity}).


has_initial_function_int({Mod, Fun, Arity}) ->
    M = #'porkrind.matcher'{
        name = has_initial_function,
        args = [Mod, Fun, Arity],
        match = fun(Value) ->
            case erlang:process_info(Value, initial_call) of
                {initial_call, {Mod, Fun, Arity}} ->
                    true;
                {initial_call, {Mod, Fun, _}} when Arity == '_' ->
                    true;
                undefined ->
                    ?PR_FAIL({dead_pid, Value});
                _ ->
                    ?PR_FAIL({not_initial, Value})
            end
        end,
        reason = fun
            ({dead_pid, Value}) ->
                io_lib:format("~w is not a live process", [Value]);
            ({not_initial, Value}) ->
                FunStr = porkrind_util:mfa_to_string(Mod, Fun, Arity),
                Args = [Value, FunStr],
                io_lib:format("~w does not have initial function ~s", Args)
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_pid(),
        M
    ]).


has_no_messages() ->
    M = #'porkrind.matcher'{
        name = has_no_messages,
        args = [],
        match = fun(Value) ->
            case erlang:process_info(Value, message_queue_len) of
                {message_queue_len, 0} ->
                    ok;
                {message_queue_len, N} ->
                    ?PR_FAIL({has_messages, Value, N});
                undefined ->
                    ?PR_FAIL({dead_pid, Value})
            end
        end,
        reason = fun
            ({dead_pid, Value}) ->
                io_lib:format("~w is a dead pid", [Value]);
            ({has_messages, Value, N}) ->
                io_lib:format("~w has ~b messages", [Value, N])
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_pid(),
        M
    ]).


has_messages() ->
    has_messages('_').


has_messages(N) when is_integer(N), N > 0; N == '_' ->
    M = #'porkrind.matcher'{
        name = has_messages,
        args = [N],
        match = fun(Value) ->
            case erlang:process_info(Value, message_queue_len) of
                {message_queue_len, 0} ->
                    ?PR_FAIL({no_messages, Value});
                {message_queue_len, N} ->
                    ok;
                {message_queue_len, _} when N == '_' ->
                    ok;
                {message_queue_len, Mismatch} ->
                    ?PR_FAIL({mismatch, Value, Mismatch});
                undefined ->
                    ?PR_FAIL({dead_pid, Value})
            end
        end,
        reason = fun
            ({dead_pid, Value}) ->
                io_lib:format("~w is a dead pid", [Value]);
            ({no_messages, Value}) ->
                io_lib:format("~w has no messages", [Value]);
            ({mismatch, Value, Mismatch}) ->
                Args = [Value, Mismatch, N],
                io_lib:format("~w has ~b messages, not ~b", Args)
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_pid(),
        M
    ]).


has_message(Matcher0) ->
    Matcher = porkrind_util:maybe_wrap(Matcher0),
    ItemMatcher = porkrind_lists:has_item(Matcher),
    M = #'porkrind.matcher'{
        name = has_message,
        args = [Matcher0],
        match = fun(Value) ->
            case erlang:process_info(Value, messages) of
                {messages, Messages} ->
                    porkrind:match(Messages, ItemMatcher);
                undefined ->
                    ?PR_FAIL({dead_pid, Value})
            end
        end,
        reason = fun({dead_pid, Value}) ->
            io_lib:format("~w is a dead pid", [Value])
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_pid(),
        M
    ]).



is_trapping_exits() ->
    M = #'porkrind.matcher'{
        name = is_trapping_exits,
        args = [],
        match = fun(Value) ->
            case erlang:process_info(Value, trap_exit) of
                {trap_exit, true} ->
                    ok;
                {trap_exit, false} ->
                    ?PR_FAIL({not_trapping, Value});
                undefined ->
                    ?PR_FAIL({dead_pid, Value})
            end
        end,
        reason = fun
            ({dead_pid, Value}) ->
                io_lib:format("~w is a dead pid", [Value]);
            ({not_trapping, Value}) ->
                io_lib:format("~w is not trapping exits", [Value])
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_pid(),
        M
    ]).


is_not_trapping_exits() ->
    M = #'porkrind.matcher'{
        name = is_not_trapping_exits,
        args = [],
        match = fun(Value) ->
            case erlang:process_info(Value, trap_exit) of
                {trap_exit, true} ->
                    ?PR_FAIL({trapping, Value});
                {trap_exit, false} ->
                    ok;
                undefined ->
                    ?PR_FAIL({dead_pid, Value})
            end
        end,
        reason = fun
            ({dead_pid, Value}) ->
                io_lib:format("~w is a dead pid", [Value]);
            ({trapping, Value}) ->
                io_lib:format("~w is trapping exits", [Value])
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_pid(),
        M
    ]).


has_pdict_key(Key) ->
    M = #'porkrind.matcher'{
        name = has_pdict_key,
        args = [Key],
        match = fun(Value) ->
            case erlang:process_info(Value, dictionary) of
                {dictionary, D} ->
                    case lists:keymember(Key, 1, D) of
                        true ->
                            ok;
                        false ->
                            ?PR_FAIL({missing_key, Value, D})
                    end;
                undefined ->
                    ?PR_FAIL({dead_pid, Value})
            end
        end,
        reason = fun
            ({dead_pid, Value}) ->
                io_lib:format("~w is a dead pid", [Value]);
            ({missing_key, Value, D}) ->
                Fmt = "~w has no entry for ~w in its process dictionary: ~w",
                io_lib:format(Fmt, [Value, Key, D])
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_pid(),
        M
    ]).


has_pdict_entry(PKey, PVal) ->
    M = #'porkrind.matcher'{
        name = has_pdict_entry,
        args = [PKey, PVal],
        match = fun(Value) ->
            case erlang:process_info(Value, dictionary) of
                {dictionary, D} ->
                    case lists:keyfind(PKey, 1, D) of
                        {PKey, PVal} ->
                            ok;
                        {PKey, Mismatch} ->
                            ?PR_FAIL({bad_value, Value, Mismatch});
                        false ->
                            ?PR_FAIL({missing_key, Value, D})
                    end;
                undefined ->
                    ?PR_FAIL({dead_pid, Value})
            end
        end,
        reason = fun
            ({dead_pid, Value}) ->
                io_lib:format("~w is a dead pid", [Value]);
            ({bad_value, Value, Mismatch}) ->
                Fmt = "~w has a value of ~w for ~w, not ~w",
                io_lib:format(Fmt, [Value, Mismatch, PKey, PVal]);
            ({missing_key, Value, D}) ->
                Fmt = "~w has no entry for ~w in its process dictionary: ~w",
                io_lib:format(Fmt, [Value, PKey, D])
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_pid(),
        M
    ]).
