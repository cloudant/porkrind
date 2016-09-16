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

-include_lib("porkrind/include/porkrind_internal.hrl").


-compile({no_auto_import, [
    is_alive/0
]}).


-import(porkrind_atoms, [
    is_true/0,
    is_false/0,
    is_undefined/0,
    is_null/0
]).

-import(porkrind_containers, [
    matches/1,
    matching/1,
    matches_inanyorder/1,
    matching_inanyorder/1,
    contains/1,
    contains_inanyorder/1,
    only_contains/1,
    tail/1
]).

-import(porkrind_fun_calls, [
    calling/3,
    does_not_raise/0,
    raises/0,
    raises/1,
    raises/2
]).

-import(porkrind_json, [
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

-import(porkrind_lists, [
    has_item/1,
    has_items/1,
    empty/0
]).

-import(porkrind_logic, [
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

-import(porkrind_pids, [
    is_alive/0,
    is_dead/0,
    is_in_function/2,
    is_in_function/3,
    has_initial_function/2,
    has_initial_function/3,
    has_no_messages/0,
    has_messages/0,
    has_messages/1,
    has_message/1,
    is_trapping_exits/0,
    is_not_trapping_exits/0,
    has_pdict_key/1,
    has_pdict_entry/2
]).

-import(porkrind_pids_or_ports, [
    registered_as_anything/0,
    registered_as/1,
    has_link/1,
    is_linked/0,
    is_not_linked/0,
    has_monitor/1,
    is_monitoring/0,
    is_not_monitoring/0,
    is_monitored_by/1,
    is_monitored/0,
    is_not_monitored/0
]).

-import(porkrind_ports, [
    has_id/1,
    has_name/1,
    has_os_pid/1,
    has_lock_level/1,
    has_port_property/2
]).

-import(porkrind_strings, [
    has_length/1,
    contains_string/1,
    starts_with/1,
    ends_with/1,
    equal_ignoring_case/1,
    equal_ignoring_whitespace/1,
    matches_re/1,
    matches_re/2,
    string_contains_in_order/1
]).

-import(porkrind_tuples, [
    has_tuple_size/1,
    has_element_at/2
]).

-import(porkrind_types, [
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


-define(assert_that(Value, Matcher), begin
    ((fun() ->
        case porkrind:check(Value, Matcher) of
            ok ->
                ok;
            {assertion_failed, ___Info} ->
                erlang:error({assertion_failed, [
                    {module, ?MODULE},
                    {line, ?LINE}
                ] ++ ___Info})
        end
    end)())
end).
