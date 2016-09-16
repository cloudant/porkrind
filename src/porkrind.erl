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

-module(porkrind).

-include("porkrind_internal.hrl").

-export([
    check/2,
    match/2,
    describe/1,
    reason/2
]).


check(Value, Matcher) ->
    try match(Value, Matcher) of
        _ ->
            ok
    catch
        throw:{porkrind_terminate, {FailedMatcher, Reason}} ->
            {assertion_failed, [
                {stack, erlang:get_stacktrace()},
                {actual, Value},
                {expected, describe(FailedMatcher)},
                {reason, reason(FailedMatcher, Reason)}
            ]};
        Type:Reason ->
            {assertion_failed, [
                {stack, erlang:get_stacktrace()},
                {actual, Value},
                {expected, describe(Matcher)},
                {error, {Type, Reason}}
            ]}
    end.


match(Value, Matcher) when ?IS_MATCHER(Matcher) ->
    MatchFun = Matcher#'porkrind.matcher'.match,
    try
        MatchFun(Value)
    catch throw:{porkrind_fail, Reason} ->
        throw({porkrind_terminate, {Matcher, Reason}})
    end.


describe(Matcher) ->
    lists:flatten(describe_int(Matcher)).


describe_int(Matcher) when ?IS_MATCHER(Matcher) ->
    #'porkrind.matcher'{
        name = Name,
        args = Args
    } = Matcher,
    if Args /= undefined -> ok; true ->
        erlang:error({badarg, Matcher})
    end,
    ArgStr = string:join(lists:map(fun describe_int/1, Args), ", "),
    io_lib:format("~s(~s)", [Name, ArgStr]);

describe_int(Tuple) when is_tuple(Tuple) ->
    Strs = lists:map(fun describe_int/1, tuple_to_list(Tuple)),
    porkrind_util:str_join(Strs, ", ", "{}");

describe_int(List) when is_list(List) ->
    Strs = lists:map(fun describe_int/1, List),
    porkrind_util:str_join(Strs, ", ", "[]");

describe_int(Term) ->
    io_lib:format("~120p", [Term]).


reason(Matcher, Reason) when ?IS_MATCHER(Matcher) ->
    ReasonFun = Matcher#'porkrind.matcher'.reason,
    if ReasonFun /= undefined -> ok; true ->
        erlang:error({badmatcher, Matcher})
    end,
    lists:flatten(ReasonFun(Reason)).

