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

-module(porkrind_ports).

-include("porkrind_internal.hrl").


-export([
    has_id/1,
    has_name/1,
    has_os_pid/1,
    has_lock_level/1,

    has_port_property/2
]).


has_id(Id) when is_integer(Id) ->
    ?PR_NAME(has_id, has_port_property(id, Id)).


has_name(Name) when is_list(Name) ->
    ?PR_NAME(has_name, has_port_property(name, Name)).


has_os_pid(OsPid) when is_integer(OsPid); OsPid == undefined ->
    ?PR_NAME(has_os_pid, has_port_property(os_pid, OsPid)).


has_lock_level(Level) when is_atom(Level) ->
    ?PR_NAME(has_lock_level, has_port_property(locking, Level)).


has_port_property(Key, Expect) ->
    M = #'porkrind.matcher'{
        name = has_port_property,
        args = [Key, Expect],
        match = fun(Value) ->
            case erlang:port_info(Value, Key) of
                {Key, Expect} ->
                    ok;
                {Key, Mismatch} ->
                    ?PR_FAIL({mismatch, Value, Mismatch});
                undefined ->
                    ?PR_FAIL({dead, Value})
            end
        end,
        reason = fun
            ({dead, Value}) ->
                io_lib:format("~w is not a live port", [Value]);
            ({mismatch, Value, Mismatch}) ->
                Args = [Value, Key, Mismatch, Expect],
                io_lib:format("~w has ~w ~w, not ~w", Args)
        end
    },
    porkrind_logic:all_of([
        porkrind_types:is_port(),
        M
    ]).
