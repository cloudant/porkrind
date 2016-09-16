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

-module(porkrind_util).

-include("porkrind_internal.hrl").


-export([
    maybe_wrap/1,
    find_first_match/2,
    mfa_to_string/3,
    str_join/3,
    vm_has_port_monitors/0
]).


maybe_wrap(Matcher) when ?IS_MATCHER(Matcher) ->
    Matcher;

maybe_wrap('_') ->
    porkrind_logic:anything();

maybe_wrap(Term) ->
    porkrind_logic:equal_to(Term).


find_first_match(_Value, []) ->
    nomatch;

find_first_match(Value, [Matcher | Rest]) ->
    try
        porkrind:match(Value, Matcher),
        {match, Matcher}
    catch _:_ ->
        find_first_match(Value, Rest)
    end.


mfa_to_string(Mod, Fun, Arity) when is_integer(Arity) ->
    lists:flatten(io_lib:format("~s:~s/~b", [Mod, Fun, Arity]));

mfa_to_string(Mod, Fun, Args) when is_list(Args) ->
    lists:flatten(mfa_to_string(Mod, Fun, length(Args)));

mfa_to_string(Mod, Fun, '_') ->
    lists:flatten(io_lib:format("~s:~s/any", [Mod, Fun])).


str_join(Strs, Join, [Left, Right]) ->
    [Left] ++ string:join(Strs, Join) ++ [Right].


vm_has_port_monitors() ->
    VersionBin = list_to_binary(erlang:system_info(version)),
    [MajorBin | _] = binary:split(VersionBin, <<".">>, [global]),
    case (catch list_to_integer(binary_to_list(MajorBin))) of
        N when N >= 8 ->
            true;
        _ ->
            false
    end.
