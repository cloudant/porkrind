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

-module(porkrind_strings).

-include("porkrind_internal.hrl").


% For string matchers, the args determine if the expect value
% should be a binary or list. I.e., if the arg is a binary
% then the value being matched must also be a binary.


-export([
    has_length/1,

    contains_string/1,
    starts_with/1,
    ends_with/1,

    equal_to_string/1,
    equal_ignoring_case/1,
    equal_ignoring_whitespace/1,

    matches_re/1,
    matches_re/2,

    string_contains_in_order/1
]).



has_length(Length) when is_integer(Length), Length >= 0 ->
    #'porkrind.matcher'{
        name = has_length,
        args = [Length],
        match = fun(Value0) ->
            String = to_binary(Value0),
            case size(String) of
                Length ->
                    ok;
                Mismatch ->
                    ?PR_FAIL({mismatch, Value0, Mismatch})
            end
        end,
        reason = fun
            ({bad_type, Value}) ->
                io_lib:format("~p is not a string", [Value]);
            ({mismatch, Value, Mismatch}) ->
                Args = [Value, Mismatch, Length],
                io_lib:format("~p has length ~b, not ~b", Args)
        end
    }.


contains_string(Pattern0) ->
    Pattern = pattern_to_binary(Pattern0),
    #'porkrind.matcher'{
        name = contains_string,
        args = [Pattern0],
        match = fun(Value0) ->
            Value = to_binary(Value0),
            case binary:match(Value, Pattern) of
                {_Pos, _Len} ->
                    ok;
                nomatch ->
                    ?PR_FAIL({nomatch, Value0})
            end
        end,
        reason = fun
            ({bad_type, Value}) ->
                io_lib:format("~p is not a string", [Value]);
            ({nomatch, Value}) ->
                io_lib:format("~p does not contain ~p", [Value, Pattern0])
        end
    }.


starts_with(Pattern0) ->
    Pattern = pattern_to_binary(Pattern0),
    #'porkrind.matcher'{
        name = starts_with,
        args = [Pattern0],
        match = fun(Value0) ->
            Value = to_binary(Value0),
            case binary:longest_common_prefix([Value, Pattern]) of
                Length when Length == size(Pattern) ->
                    ok;
                _ ->
                    ?PR_FAIL({nomatch, Value0})
            end
        end,
        reason = fun
            ({bad_type, Value}) ->
                io_lib:format("~p is not a string", [Value]);
            ({nomatch, Value}) ->
                io_lib:format("~p does not start with ~p", [Value, Pattern0])
        end
    }.

ends_with(Pattern0) ->
    Pattern = pattern_to_binary(Pattern0),
    #'porkrind.matcher'{
        name = ends_with,
        args = [Pattern0],
        match = fun(Value0) ->
            Value = to_binary(Value0),
            case binary:longest_common_suffix([Value, Pattern]) of
                Length when Length == size(Pattern) ->
                    ok;
                _ ->
                    ?PR_FAIL({nomatch, Value0})
            end
        end,
        reason = fun
            ({bad_type, Value}) ->
                io_lib:format("~p is not a string", [Value]);
            ({nomatch, Value}) ->
                io_lib:format("~p does not end with ~p", [Value, Pattern0])
        end
    }.


equal_to_string(String0) ->
    String = to_binary(String0),
    #'porkrind.matcher'{
        name = equal_to_string,
        args = [String0],
        match = fun(Value0) ->
            Value = to_binary(Value0),
            if Value == String -> ok; true ->
                ?PR_FAIL({nomatch, Value0})
            end
        end,
        reason = fun
            ({bad_type, Value}) ->
                io_lib:format("~p is not a string", [Value]);
            ({nomatch, Value}) ->
                io_lib:format("~p does not equal ~p", [Value, String0])
        end
    }.


equal_ignoring_case(Pattern0) ->
    Pattern = to_lower(pattern_to_binary(Pattern0)),
    #'porkrind.matcher'{
        name = equal_ignoring_case,
        args = [Pattern0],
        match = fun(Value0) ->
            Value = to_lower(to_binary(Value0)),
            case Value == Pattern of
                true ->
                    ok;
                false ->
                    ?PR_FAIL({nomatch, Value})
            end
        end,
        reason = fun
            ({bad_type, Value}) ->
                io_lib:format("~p is not a string", [Value]);
            ({nomatch, Value}) ->
                io_lib:format("~p does not equal ~p", [Value, Pattern])
        end
    }.


equal_ignoring_whitespace(Pattern0) ->
    Pattern = strip_whitespace(pattern_to_binary(Pattern0)),
    #'porkrind.matcher'{
        name = equal_ignoring_whitespace,
        args = [Pattern0],
        match = fun(Value0) ->
            Value = strip_whitespace(to_binary(Value0)),
            case Value == Pattern of
                true ->
                    ok;
                false ->
                    ?PR_FAIL({nomatch, Value})
            end
        end,
        reason = fun
            ({bad_type, Value}) ->
                io_lib:format("~p is not a string", [Value]);
            ({nomatch, Value}) ->
                io_lib:format("~p does not equal ~p", [Value, Pattern])
        end
    }.


matches_re(Pattern) ->
    matches_re(Pattern, []).


matches_re(Pattern0, Opts) when is_list(Opts) ->
    % Compile ahead of time so that errors in the regexp
    % are noted before the test runs.
    Pattern = pattern_to_binary(Pattern0),
    CompileOpts = compile_opts(Opts),
    RunOpts = run_opts(Opts),
    {ok, MP} = re:compile(Pattern, CompileOpts),
    #'porkrind.matcher'{
        name = matches_re,
        args = [Pattern0, Opts],
        match = fun(Value0) ->
            Value = to_binary(Value0),
            case re:run(Value, MP, RunOpts) of
                {match, _} ->
                    ok;
                match ->
                    ok;
                _ ->
                    ?PR_FAIL({nomatch, Value0})
            end
        end,
        reason = fun
            ({bad_type, Value}) ->
                io_lib:format("~p is not a string", [Value]);
            ({nomatch, Value}) ->
                Args = [Value, Pattern, opts_to_string(Opts)],
                io_lib:format("~p does not match ~p~s", Args)
        end
    }.


string_contains_in_order(Patterns0) when is_list(Patterns0) ->
    Patterns = lists:map(fun(P) ->
        pattern_to_binary(P)
    end, Patterns0),
    #'porkrind.matcher'{
        name = contains_in_order,
        args = [Patterns0],
        match = fun(Value0) ->
            Value = to_binary(Value0),
            lists:foldl(fun(P, {S, L}) ->
                case binary:match(Value, P, [{scope, {S, L}}]) of
                    {PS, PL} ->
                        {PS + PL, size(Value) - PS - PL};
                    nomatch ->
                        Tail = binary:part(Value, {S, L}),
                        ?PR_FAIL({nomatch, Tail, P})
                end
            end, {0, size(Value)}, Patterns)
        end,
        reason = fun
            ({bad_type, Value}) ->
                io_lib:format("~p is not a string", [Value]);
            ({nomatch, Value, Pattern}) ->
                io_lib:format("~p does not match ~p", [Value, Pattern])
        end
    }.


to_binary(String) ->
    try iolist_to_binary(String) of
        Bin when is_binary(Bin) ->
            Bin;
        _ ->
            ?PR_FAIL({bad_type, String})
    catch _:_ ->
        ?PR_FAIL({bad_type, String})
    end.


pattern_to_binary(String) ->
    try
        to_binary(String)
    catch _:_ ->
        erlang:error({badarg, String})
    end.


to_lower(Binary) ->
    to_lower(binary_to_list(Binary), []).


to_lower([], Acc) ->
    list_to_binary(lists:reverse(Acc));

to_lower([C | Rest], Acc) when C >= $A, C =< $Z ->
    to_lower(Rest, [(C - $A + $a) | Acc]);

to_lower([C | Rest], Acc) ->
    to_lower(Rest, [C | Acc]).


strip_whitespace(Binary) ->
    strip_whitespace(binary_to_list(Binary), []).


strip_whitespace([], Acc) ->
    list_to_binary(lists:reverse(Acc));

strip_whitespace([C | Rest], Acc)
        when C == $\n; C == $\t; C == $\r; C == $\s ->
    strip_whitespace(Rest, Acc);

strip_whitespace([C | Rest], Acc) ->
    strip_whitespace(Rest, [C | Acc]).


opts_to_string([]) ->
    "";

opts_to_string(Opts) ->
    io_lib:format(" with options: ~p", [Opts]).


compile_opts(Opts) ->
    filter_opts(Opts, compile_opt_names()).


run_opts(Opts) ->
    filter_opts(Opts, run_opt_names()).


filter_opts([], _Allowed) ->
    [];

filter_opts([Opt | Rest], Allowed) ->
    OptName = case Opt of
        {Name, _} -> Name;
        {Name, _, _} -> Name;
        _ -> Opt
    end,
    case lists:member(OptName, Allowed) of
        true -> [Opt | filter_opts(Rest, Allowed)];
        false -> filter_opts(Rest, Allowed)
    end.


compile_opt_names() ->
    [
        unicode,
        anchored,
        caseless,
        dollar_endonly,
        dotall,
        extended,
        firstline,
        multiline,
        no_auto_capture,
        dupnames,
        ungreedy,
        newline,
        bsr_anycrlf,
        bsr_unicode,
        no_start_optimize,
        ucp,
        never_utf
    ].


run_opt_names() ->
    [
        anchored,
        capture,
        global,
        match_limit,
        match_limit_recursion,
        newline,
        notbol,
        notempty,
        notempty_atstart,
        noteol,
        offset,
        report_errors
    ].
