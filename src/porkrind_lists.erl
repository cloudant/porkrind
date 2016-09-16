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

-module(porkrind_lists).

-include("porkrind_internal.hrl").


-export([
    has_item/1,
    has_items/1,

    empty/0
]).


has_item(Matcher0) ->
    Matcher = porkrind_util:maybe_wrap(Matcher0),
    M = #'porkrind.matcher'{
        name = has_item,
        args = [Matcher0],
        match = fun(Values) ->
            case has_item_int(Values, Matcher) of
                ok ->
                    ok;
                nomatch ->
                    ?PR_FAIL({no_match, Values})
            end
        end,
        reason = fun({no_match, Values}) ->
            Prefix = io_lib:format("~w has no item matching ", [Values]),
            [Prefix, porkrind:describe(Matcher)]
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_list(),
        M
    ]).


has_item_int([], _Matcher) ->
    nomatch;

has_item_int([Value | Rest], Matcher) ->
    case porkrind:check(Value, Matcher) of
        ok ->
            ok;
        {assertion_failed, _} ->
            has_item_int(Rest, Matcher)
    end.


has_items(Matchers0) ->
    Matchers = lists:map(fun porkrind_util:maybe_wrap/1, Matchers0),
    M = #'porkrind.matcher'{
        name = has_items,
        args = [Matchers0],
        match = fun(Values) ->
            lists:foreach(fun(Matcher) ->
                case has_item_int(Values, Matcher) of
                    ok ->
                        ok;
                    nomatch ->
                        ?PR_FAIL({nomatch, Values, Matcher})
                end
            end, Matchers)
        end,
        reason = fun({nomatch, Values, Matcher}) ->
            Prefix = io_lib:format("~w has no item matching ", [Values]),
            [Prefix, porkrind:describe(Matcher)]
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_list(),
        M
    ]).


empty() ->
    M = #'porkrind.matcher'{
        name = empty,
        args = [],
        match = fun(Value) ->
            if length(Value) == 0 -> ok; true ->
                ?PR_FAIL({notempty, Value})
            end
        end,
        reason = fun({notempty, Value}) ->
            io_lib:format("~p is not an empty list", [Value])
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_list(),
        M
    ]).
