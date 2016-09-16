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

-module(porkrind_tuples).

-include("porkrind_internal.hrl").


-export([
    has_tuple_size/1,
    has_element_at/2
]).


has_tuple_size(Size) when is_integer(Size), Size >= 0 ->
    M = #'porkrind.matcher'{
        name = has_tuple_size,
        args = [Size],
        match = fun(Value) ->
            case tuple_size(Value) of
                Size ->
                    ok;
                Mismatch ->
                    ?PR_FAIL({mismatch, Value, Mismatch})
            end
        end,
        reason = fun({mismatch, Value, Mismatch}) ->
            io_lib:format("~w has size ~b, not ~b", [Value, Mismatch, Size])
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_tuple(),
        M
    ]).


has_element_at(Index, Matcher0) when is_integer(Index), Index >= 1 ->
    Matcher = porkrind_util:maybe_wrap(Matcher0),
    M = #'porkrind.matcher'{
        name = has_element_at,
        args = [Index, Matcher0],
        match = fun(Value) ->
            case tuple_size(Value) of
                S when S >= Index ->
                    porkrind:match(element(Index, Value), Matcher);
                S ->
                    ?PR_FAIL({too_small, Value, S})
            end
        end,
        reason = fun
            ({too_small, Value, Size}) ->
                Args = [Value, Size, Index],
                io_lib:format("~w has size ~b, which is smaller than ~b", Args)
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_tuple(),
        M
    ]).
