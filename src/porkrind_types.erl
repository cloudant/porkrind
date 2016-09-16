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

-module(porkrind_types).

-include("porkrind_internal.hrl").


-export([
    is_atom/0,
    is_binary/0,
    is_bitstring/0,
    is_boolean/0,
    is_integer/0,
    is_float/0,
    is_fun/0,
    is_fun/1,
    is_list/0,
    is_negative/0,
    is_non_negative/0,
    is_number/0,
    is_pid/0,
    is_port/0,
    is_positive/0,
    is_ref/0,
    is_string/0,
    is_tuple/0
]).


is_atom() ->
    #'porkrind.matcher'{
        name = is_atom,
        args = [],
        match = fun(Value) -> check_type(Value, is_atom) end,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~w is not an atom", [Value])
        end
    }.


is_binary() ->
    #'porkrind.matcher'{
        name = is_binary,
        args = [],
        match = fun(Value) -> check_type(Value, is_binary) end,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~w is not a binary", [Value])
        end
    }.


is_bitstring() ->
    #'porkrind.matcher'{
        name = is_bitstring,
        args = [],
        match = fun(Value) -> check_type(Value, is_bitstring) end,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~w is not a bitstring", [Value])
        end
    }.


is_boolean() ->
    #'porkrind.matcher'{
        name = is_boolean,
        args = [],
        match = fun(Value) ->
            case Value of
                true -> ok;
                false -> ok;
                _ -> ?PR_FAIL({bad_type, Value})
            end
        end,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~w is not a boolean", [Value])
        end
    }.


is_integer() ->
    #'porkrind.matcher'{
        name = is_integer,
        args = [],
        match = fun(Value) -> check_type(Value, is_integer) end,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~w is not an integer", [Value])
        end
    }.


is_float() ->
    #'porkrind.matcher'{
        name = is_float,
        args = [],
        match = fun(Value) -> check_type(Value, is_float) end,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~w is not a float", [Value])
        end
    }.


is_fun() ->
    #'porkrind.matcher'{
        name = is_fun,
        args = [],
        match = fun(Value) -> check_type(Value, is_function) end,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~w is not a function", [Value])
        end
    }.


is_fun(Arity) when is_integer(Arity) ->
    M = #'porkrind.matcher'{
        name = is_fun_with_arity,
        args = [Arity],
        match = fun(Value) ->
            Info = erlang:fun_info(Value),
            {arity, A} = lists:keyfind(arity, 1, Info),
            if A == Arity -> ok; true ->
                ?PR_FAIL({arity_mismatch, A})
            end
        end,
        reason = fun({arity_mismatch, A}) ->
            io_lib:format("has arity ~b, not ~b", [A, Arity])
        end
    },
    porkrind_logic:all_of([is_fun(), M]).


is_list() ->
    #'porkrind.matcher'{
        name = is_list,
        args = [],
        match = fun(Value) -> check_type(Value, is_list) end,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~w is not a list", [Value])
        end
    }.


is_negative() ->
    porkrind_logic:all_of([
        is_number(),
        porkrind_logic:less_than(0)
    ]).


is_non_negative() ->
    porkrind_logic:all_of([
        is_number(),
        porkrind_logic:gteq(0)
    ]).


is_number() ->
    #'porkrind.matcher'{
        name = is_number,
        args = [],
        match = fun(Value) -> check_type(Value, is_number) end,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~w is not a number", [Value])
        end
    }.


is_pid() ->
    #'porkrind.matcher'{
        name = is_pid,
        args = [],
        match = fun(Value) -> check_type(Value, is_pid) end,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~w is not a pid", [Value])
        end
    }.


is_port() ->
    #'porkrind.matcher'{
        name = is_port,
        args = [],
        match = fun(Value) -> check_type(Value, is_port) end,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~w is not a port", [Value])
        end
    }.


is_positive() ->
    porkrind_logic:all_of([
        is_number(),
        porkrind_logic:greater_than(0)
    ]).


is_ref() ->
    #'porkrind.matcher'{
        name = is_ref,
        args = [],
        match = fun(Value) -> check_type(Value, is_reference) end,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~w is not a reference", [Value])
        end
    }.


is_string() ->
    #'porkrind.matcher'{
        name = is_string,
        args = [],
        match = fun(Value) ->
            case (catch iolist_to_binary(Value)) of
                Bin when is_binary(Bin) ->
                    ok;
                _ ->
                    ?PR_FAIL({bad_type, Value})
            end
        end,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~p is not a string", [Value])
        end
    }.


is_tuple() ->
    #'porkrind.matcher'{
        name = is_tuple,
        args = [],
        match = fun(Value) -> check_type(Value, is_tuple) end,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~w is not a tuple", [Value])
        end
    }.


check_type(Value, Fun) ->
    case erlang:Fun(Value) of
        true -> ok;
        false -> ?PR_FAIL({bad_type, Value})
    end.
