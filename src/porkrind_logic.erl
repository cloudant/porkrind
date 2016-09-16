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

-module(porkrind_logic).

-include("porkrind_internal.hrl").


-export([
    is/1,
    is_not/1,

    anything/0,

    equal_to/1,
    exactly_equal_to/1,

    gt/1,
    gteq/1,
    greater_than/1,
    greater_than_or_equal_to/1,

    lt/1,
    lteq/1,
    less_than/1,
    less_than_or_equal_to/1,

    close_to/1,
    close_to/2,

    all_of/1,
    any_of/1
]).


is(Matcher0) ->
    Matcher = porkrind_util:maybe_wrap(Matcher0),
    #'porkrind.matcher'{
        name = is,
        args = [Matcher0],
        match = fun(Value) -> porkrind:match(Value, Matcher) end
    }.


is_not(Matcher0) ->
    Matcher = porkrind_util:maybe_wrap(Matcher0),
    #'porkrind.matcher'{
        name = is_not,
        args = [Matcher0],
        match = fun(Value) ->
            try porkrind:match(Value, Matcher) of
                _ -> ?PR_FAIL({is_not_fail, Value})
            catch _:_ ->
                ok
            end
        end,
        reason = fun({is_not_fail, Value}) ->
            [io_lib:format("~w is ", [Value]), porkrind:describe(Matcher)]
        end
    }.


anything() ->
    #'porkrind.matcher'{
        name = anything,
        args = [],
        match = fun(_Value) -> ok end
    }.


equal_to(Term) ->
    #'porkrind.matcher'{
        name = equal_to,
        args = [Term],
        match = fun(Value) ->
            if Value == Term -> ok; true ->
                ?PR_FAIL({not_equal, Value})
            end
        end,
        reason = fun({not_equal, Value}) ->
            io_lib:format("~w is not equal to ~w", [Value, Term])
        end
    }.


exactly_equal_to(Term) ->
    #'porkrind.matcher'{
        name = exactly_equal_to,
        args = [Term],
        match = fun(Value) ->
            if Value =:= Term -> ok; true ->
                ?PR_FAIL({not_exactly_equal, Value})
            end
        end,
        reason = fun({not_exactly_equal, Value}) ->
            io_lib:format("~w is not exactly equal to ~w", [Value, Term])
        end
    }.


gt(Term) ->
    greater_than(Term).


gteq(Term) ->
    greater_than_or_equal_to(Term).


greater_than(Term) ->
    #'porkrind.matcher'{
        name = greater_than,
        args = [Term],
        match = fun(Value) ->
            if Value > Term -> ok; true ->
                ?PR_FAIL({gt_fail, Value})
            end
        end,
        reason = fun({gt_fail, Value}) ->
            io_lib:format("~w is less than or equal to ~w", [Value, Term])
        end
    }.


greater_than_or_equal_to(Term) ->
    #'porkrind.matcher'{
        name = greater_than_or_equal_to,
        args = [Term],
        match = fun(Value) ->
            if Value >= Term -> ok; true ->
                ?PR_FAIL({gteq_fail, Value})
            end
        end,
        reason = fun({gteq_fail, Value}) ->
            io_lib:format("~w is less than ~w", [Value, Term])
        end
    }.


lt(Term) ->
    less_than(Term).


lteq(Term) ->
    less_than_or_equal_to(Term).


less_than(Term) ->
    #'porkrind.matcher'{
        name = less_than,
        args = [Term],
        match = fun(Value) ->
            if Value < Term -> ok; true ->
                ?PR_FAIL({lt_fail, Value})
            end
        end,
        reason = fun({lt_fail, Value}) ->
            io_lib:format("~w is greater than or equal to ~w", [Value, Term])
        end
    }.


less_than_or_equal_to(Term) ->
    #'porkrind.matcher'{
        name = less_than_or_equal_to,
        args = [Term],
        match = fun(Value) ->
            if Value =< Term -> ok; true ->
                ?PR_FAIL({lteq_fail, Value})
            end
        end,
        reason = fun({lteq_fail, Value}) ->
            io_lib:format("~w is greater than ~w", [Value, Term])
        end
    }.


close_to(Number) ->
    close_to(Number, 0.000001).


close_to(Number, Delta) when is_number(Number), is_number(Delta), Delta >= 0 ->
    M = #'porkrind.matcher'{
        name = close_to,
        args = [Number, Delta],
        match = fun(Value) ->
            AbsVal = erlang:abs(Value),
            AbsNum = erlang:abs(Number),
            case erlang:abs(AbsVal - AbsNum) < Delta of
                true -> ok;
                false -> ?PR_FAIL({not_close, Value})
            end
        end,
        reason = fun({not_close, Value}) ->
            Args = [Value, Delta, Number],
            io_lib:format("~w is more than ~w different than ~w", Args)
        end
    },
    all_of([porkrind_types:is_number(), M]).


all_of(Matchers0) when is_list(Matchers0), length(Matchers0) > 0 ->
    % Simplify when we have nested all_of matchers
    % which is a fairly common pattern. Make sure that
    % we keep the same left-to-right ordering though.
    Matchers1 = lists:flatmap(fun(M) ->
        case M of
            #'porkrind.matcher'{name = all_of, args = [SubMatchers]} ->
                SubMatchers;
            Else ->
                [Else]
        end
    end, Matchers0),
    Matchers = lists:map(fun porkrind_util:maybe_wrap/1, Matchers1),
    #'porkrind.matcher'{
        name = all_of,
        args = [Matchers1],
        match = fun(Value) ->
            lists:foreach(fun(M) -> porkrind:match(Value, M) end, Matchers)
        end
    }.


any_of(Matchers0) when is_list(Matchers0), length(Matchers0) > 0 ->
    Matchers = lists:map(fun porkrind_util:maybe_wrap/1, Matchers0),
    #'porkrind.matcher'{
        name = any_of,
        args = [Matchers0],
        match = fun(Value) ->
            case porkrind_util:find_first_match(Value, Matchers) of
                {match, _} ->
                    ok;
                nomatch ->
                    ?PR_FAIL({any_of_fail, Value})
            end
        end,
        reason = fun({any_of_fail, Value}) ->
            Prefix = io_lib:format("~w does not match any of", [Value]),
            Descrs = lists:map(fun porkrind:describe/1, Matchers),
            [Prefix, porkrind_util:str_join(Descrs, " or ", "()")]
        end
    }.
