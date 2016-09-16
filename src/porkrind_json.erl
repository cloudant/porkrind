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

-module(porkrind_json).


-include("porkrind_internal.hrl").


% These are based on the JSON structures returned by Jiffy. That means:
%
% An object is a single element tuple that contains a list of two-tuples. Each
% of the two-tuples first element is a binary. The second element is
% any valid JSON value.
%
% An array is an Erlang list of valid JSON values


-export([
    is_json_object/0,
    is_json_array/0,

    is_json_equal_to/1,

    has_json_key/1,
    has_json_path/1,

    has_json_matching/2,
    has_json_matching_at/2,

    has_json_entry/2,
    has_json_entry_at/3,

    has_json_entries/1,
    has_json_entries_at/2,

    has_json_value/1
]).


is_json_object() ->
    #'porkrind.matcher'{
        name = is_json_object,
        args = [],
        match = fun(Value) ->
            case Value of
                {Props} when is_list(Props) ->
                    ok;
                _ ->
                    ?PR_FAIL({not_object, Value})
            end
        end,
        reason = fun({not_object, Value}) ->
            io_lib:format("~p is not a JSON object", [Value])
        end
    }.


is_json_array() ->
    M = porkrind_types:is_list(),
    M#'porkrind.matcher'{
        name = is_json_array,
        reason = fun({bad_type, Value}) ->
            io_lib:format("~p is not a JSON array", [Value])
        end
    }.


is_json_equal_to(Expect) ->
    #'porkrind.matcher'{
        name = is_json_equal,
        args = [Expect],
        match = fun(Value) ->
            case json_eq(Expect, Value) of
                true ->
                    ok;
                false ->
                    ?PR_FAIL({not_equal, Value})
            end
        end,
        reason = fun({not_equal, Value}) ->
            io_lib:format("~p is not JSON equivalent to ~p", [Value, Expect])
        end
    }.


has_json_key(Key) when is_binary(Key); is_atom(Key) ->
    M = #'porkrind.matcher'{
        name = has_json_key,
        args = [Key],
        match = fun({Props}) ->
            case find_key(Key, Props) of
                {_, _} ->
                    ok;
                not_found ->
                    ?PR_FAIL({not_found, {Props}})
            end
        end,
        reason = fun({not_found, Value}) ->
            io_lib:format("~p is missing key ~p", [Value, Key])
        end
    },
    porkrind_logic:all_of([
        is_json_object(),
        M
    ]).


has_json_path(Path) when is_list(Path) ->
    lists:foreach(fun(P) ->
        if is_binary(P) orelse is_atom(P) -> ok; true ->
            erlang:error({badarg, P})
        end
    end, Path),
    M = #'porkrind.matcher'{
        name = has_json_path,
        args = [Path],
        match = fun(Value) ->
            case json_path(Value, Path) of
                {ok, _} ->
                    ok;
                not_found ->
                    ?PR_FAIL({no_match, Value})
            end
        end,
        reason = fun({no_match, Value}) ->
            io_lib:format("~p has no path ~p", [Value, Path])
        end
    },
    porkrind_logic:all_of([
        is_json_object(),
        M
    ]).


has_json_matching(Key, Matcher0)  when is_binary(Key); is_atom(Key) ->
    Matcher = porkrind_util:maybe_wrap(Matcher0),
    M = #'porkrind.matcher'{
        name = has_json_matching,
        args = [Key, Matcher0],
        match = fun({Props}) ->
            case find_key(Key, Props) of
                {_, Found} ->
                    porkrind:match(Found, Matcher);
                not_found ->
                    ?PR_FAIL({not_found, {Props}})
            end
        end,
        reason = fun
            ({not_found, Value}) ->
                io_lib:format("~p is missing key ~p", [Value, Key])
        end
    },
    porkrind_logic:all_of([
        is_json_object(),
        M
    ]).


has_json_matching_at(Path, Matcher0) when is_list(Path) ->
    lists:foreach(fun(P) ->
        if is_binary(P) orelse is_atom(P) -> ok; true ->
            erlang:error({badarg, P})
        end
    end, Path),
    Matcher = porkrind_util:maybe_wrap(Matcher0),
    M = #'porkrind.matcher'{
        name = has_json_matching_at,
        args = [Path, Matcher0],
        match = fun(Value) ->
            case json_path(Value, Path) of
                {ok, Found} ->
                    porkrind:match(Found, Matcher);
                not_found ->
                    ?PR_FAIL({not_found, Value})
            end
        end,
        reason = fun
            ({not_found, Value}) ->
                Args = [Value, Path],
                io_lib:format("~p does not have a JSON path matching ~p", Args)
        end
    },
    porkrind_logic:all_of([
        is_json_object(),
        M
    ]).


has_json_entry(Key, Expect) ->
    ?PR_NAME(has_json_entry, has_json_matching(Key, is_json_equal_to(Expect))).


has_json_entry_at(Path, Key, Expect) when is_list(Path) ->
    M = has_json_matching_at(Path ++ [Key], is_json_equal_to(Expect)),
    ?PR_NAME(has_json_entry_at, M).


has_json_entries(Entries) when is_list(Entries), length(Entries) >= 1 ->
    Matchers = lists:map(fun
        ({K, V}) ->
            has_json_entry(K, V);
        (Else) ->
            erlang:error({badarg, Else})
    end, Entries),
    ?PR_NAME(has_json_entries, porkrind_logic:all_of(Matchers)).


has_json_entries_at(Path, Entries) ->
    M = has_json_matching_at(Path, has_json_entries(Entries)),
    ?PR_NAME(has_json_entries, M).


has_json_value(Expect) ->
    Matcher = porkrind_lists:has_item(Expect),
    M = #'porkrind.matcher'{
        name = has_json_value,
        args = [Expect],
        match = fun({Props}) when is_list(Props) ->
            Values = [V || {_K, V} <- Props],
            porkrind:match(Values, Matcher)
        end
    },
    porkrind_logic:all_of([
        is_json_object(),
        M
    ]).


json_eq({A}, {B}) ->
    json_eq_obj(lists:sort(A), lists:sort(B));

json_eq(A, B) when is_list(A), is_list(B) ->
    json_eq_array(A, B);

json_eq(A, B) when A == true; B == true ->
    A == B;

json_eq(A, B) when A == false; B == false ->
    A == B;

json_eq(A, B) when A == null; B == null ->
    A == B;

json_eq(A, B) when is_atom(A); is_atom(B) ->
    maybe_to_bin(A) == maybe_to_bin(B);

json_eq(A, B) ->
    A == B.


json_eq_obj([], []) ->
    true;

json_eq_obj([{KeyA, ValA} | RestA], [{KeyB, ValB} | RestB]) ->
    case json_eq(KeyA, KeyB) andalso json_eq(ValA, ValB) of
        true ->
            json_eq_obj(RestA, RestB);
        false ->
            false
    end;

json_eq_obj(_, _) ->
    % One of the two has fewer elements
    false.


json_eq_array([], []) ->
    true;

json_eq_array([A | RestA], [B | RestB]) ->
    case json_eq(A, B) of
        true ->
            json_eq_array(RestA, RestB);
        false ->
            false
    end;

json_eq_array(_, _) ->
    % One of the two has fewer elements
    false.


maybe_to_bin(Value) ->
    if not is_atom(Value) -> Value; true ->
        list_to_binary(atom_to_list(Value))
    end.



json_path(Value, []) ->
    {ok, Value};

json_path({Props}, [NextKey | RestKeys]) when is_list(Props) ->
    case find_key(NextKey, Props) of
        {_, Value} ->
            json_path(Value, RestKeys);
        not_found ->
            not_found
    end;

json_path(_Value, _Path) ->
    not_found.


find_key(Key, Props) when is_list(Props) ->
    case lists:keyfind(Key, 1, Props) of
        {Key, Value} ->
            {Key, Value};
        false ->
            case lists:keyfind(swap_bin_and_atom(Key), 1, Props) of
                {SwappedKey, Value} ->
                    {SwappedKey, Value};
                false ->
                    not_found
            end
    end.


swap_bin_and_atom(Key) when is_binary(Key) ->
    list_to_atom(binary_to_list(Key));

swap_bin_and_atom(Key) when is_atom(Key) ->
    list_to_binary(atom_to_list(Key)).


