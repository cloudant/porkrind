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

-module(porkrind_containers).

-include("porkrind_internal.hrl").


-export([
    matches/1,
    matching/1,
    matches_inanyorder/1,
    matching_inanyorder/1,

    contains/1,
    contains_inanyorder/1,
    only_contains/1
]).

-export([
    tail/1
]).


matches(Matcher) when ?IS_MATCHER(Matcher) ->
    Matcher;

matches(Container) when is_list(Container) ->
    Matchers = lists:map(fun matches/1, Container),
    contains(Matchers);

matches(Container) when is_tuple(Container) ->
    Matchers = lists:map(fun matches/1, tuple_to_list(Container)),
    contains(list_to_tuple(Matchers));

matches(Term) ->
    porkrind_util:maybe_wrap(Term).


matching(Term) ->
    matches(Term).


matches_inanyorder(Matcher) when ?IS_MATCHER(Matcher) ->
    Matcher;

matches_inanyorder(Container) when is_list(Container) ->
    Matchers = lists:map(fun matches_inanyorder/1, Container),
    contains_inanyorder(Matchers);

matches_inanyorder(Container) when is_tuple(Container) ->
    Matchers = lists:map(fun matches_inanyorder/1, tuple_to_list(Container)),
    contains_inanyorder(list_to_tuple(Matchers));

matches_inanyorder(Term) ->
    porkrind_util:maybe_wrap(Term).


matching_inanyorder(Term) ->
    matches_inanyorder(Term).


contains(Matcher) when ?IS_MATCHER(Matcher) ->
    erlang:error({badarg, Matcher});

contains(Matchers0) when is_list(Matchers0) ->
    M = contains_int(Matchers0),
    porkrind_logic:all_of([
        porkrind_types:is_list(),
        M
    ]);

contains(Matchers0) when is_tuple(Matchers0) ->
    M1 = contains_int(tuple_to_list(Matchers0)),
    M2 = tuple_wrap(M1),
    porkrind_logic:all_of([
        porkrind_types:is_tuple(),
        M2
    ]).


contains_inanyorder(Matcher) when ?IS_MATCHER(Matcher) ->
    erlang:error({badarg, Matcher});

contains_inanyorder(Matchers0) when is_list(Matchers0) ->
    M = contains_inanyorder_int(Matchers0),
    porkrind_logic:all_of([
        porkrind_types:is_list(),
        M
    ]);

contains_inanyorder(Matchers0) when is_tuple(Matchers0) ->
    M1 = contains_inanyorder_int(tuple_to_list(Matchers0)),
    M2 = tuple_wrap(M1),
    porkrind_logic:all_of([
        porkrind_types:is_tuple(),
        M2
    ]).


only_contains(Matcher) when ?IS_MATCHER(Matcher) ->
    only_contains_single(Matcher);

only_contains(Matchers0) when is_list(Matchers0) ->
    M = only_contains_int(Matchers0),
    porkrind_logic:all_of([
        porkrind_types:is_list(),
        M
    ]);

only_contains(Matchers0) when is_tuple(Matchers0) ->
    M1 = only_contains_int(tuple_to_list(Matchers0)),
    M2 = tuple_wrap(M1),
    porkrind_logic:all_of([
        porkrind_types:is_tuple(),
        M2
    ]);


only_contains(Term) ->
    only_contains_single(Term).


tail(Matcher0) ->
    Matcher = porkrind_util:maybe_wrap(Matcher0),
    #'porkrind.matcher'{
        name = tail,
        args = [Matcher0],
        match = fun(Value) ->
            porkrind:match(Value, Matcher)
        end
    }.


contains_int(Matchers0) ->
    Matchers = lists:map(fun porkrind_util:maybe_wrap/1, Matchers0),
    #'porkrind.matcher'{
        name = contains,
        args = [Matchers0],
        match = fun(Values) ->
            case apply_contains_matchers(Values, Matchers) of
                ok -> ok;
                Else -> ?PR_FAIL({Else, Values})
            end
        end,
        reason = fun({bad_length, Values}) ->
            Args = [Values, length(Values), length(Matchers)],
            io_lib:format("~p has length ~b, not ~b", Args)
        end
    }.



apply_contains_matchers(Values, [#'porkrind.matcher'{name = tail} = M]) ->
    apply_contains_tail_matcher(Values, M);


apply_contains_matchers([], []) ->
    ok;

apply_contains_matchers(_, []) ->
    bad_length;

apply_contains_matchers([], _) ->
    bad_length;

apply_contains_matchers([Value | RestValues], [Matcher | RestMatchers]) ->
    porkrind:match(Value, Matcher),
    apply_contains_matchers(RestValues, RestMatchers).


apply_contains_tail_matcher([], _Matcher) ->
    ok;

apply_contains_tail_matcher([Value | RestValues], Matcher) ->
    porkrind:match(Value, Matcher),
    apply_contains_tail_matcher(RestValues, Matcher).


contains_inanyorder_int(Matchers0) ->
    Matchers = lists:map(fun porkrind_util:maybe_wrap/1, Matchers0),
    #'porkrind.matcher'{
        name = contains_inanyorder,
        args = [Matchers0],
        match = fun(Values) ->
            if length(Values) == length(Matchers) -> ok; true ->
                ?PR_FAIL({bad_length, Values})
            end,
            [] = lists:foldl(fun(Value, Acc) ->
                case porkrind_util:find_first_match(Value, Acc) of
                    {match, M} ->
                        Acc -- [M];
                    nomatch ->
                        ?PR_FAIL({no_match_for, Value})
                end
            end, Matchers, Values)
        end,
        reason = fun
            ({bad_length, Value}) ->
                Args = [Value, length(Value), length(Matchers)],
                io_lib:format("~p has length ~b, not ~b", Args);
            ({no_match_for, Value}) ->
                Descs = lists:map(fun(M) -> porkrind:describe(M) end, Matchers),
                Prefix = io_lib:format("no match for ~w in ", [Value]),
                [Prefix, porkrind_util:str_join(Descs, " or ", "()")]
        end
    }.


only_contains_int(Matchers0) ->
    Matchers = lists:map(fun porkrind_util:maybe_wrap/1, Matchers0),
    AnyOfMatcher = porkrind_logic:any_of(Matchers),
    #'porkrind.matcher'{
        name = only_contains,
        args = [Matchers0],
        match = fun(Values) ->
            lists:foreach(fun(Value) ->
                porkrind:match(Value, AnyOfMatcher)
            end, Values)
        end
    }.


only_contains_single(Matcher0) ->
    Matcher = porkrind_util:maybe_wrap(Matcher0),
    M1 = #'porkrind.matcher'{
        name = only_contains,
        args = [Matcher0],
        match = fun(Values0) ->
            Values = case Values0 of
                _ when is_list(Values0) -> Values0;
                _ when is_tuple(Values0) -> tuple_to_list(Values0)
            end,
            lists:foreach(fun(Value) ->
                porkrind:match(Value, Matcher)
            end, Values)
        end
    },
    porkrind_logic:all_of([
        porkrind_logic:any_of([
            porkrind_types:is_list(),
            porkrind_types:is_tuple()
        ]),
        M1
    ]).


tuple_wrap(#'porkrind.matcher'{args = [Arg], match = Match} = M) ->
    M#'porkrind.matcher'{
        args = [list_to_tuple(Arg)],
        match = fun(Tuple) ->
            Match(tuple_to_list(Tuple))
        end
    }.
