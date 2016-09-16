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

-module(porkrind_pids_or_ports).

-include("porkrind_internal.hrl").


-export([
    registered_as_anything/0,
    registered_as/1,

    has_link/1,
    is_linked/0,
    is_not_linked/0,

    % Port support for monitors is relatively new so make sure
    % your VM supports such things before using these matchers
    % on ports.

    has_monitor/1,
    is_monitoring/0,
    is_not_monitoring/0,

    is_monitored_by/1,
    is_monitored/0,
    is_not_monitored/0
]).


registered_as_anything() ->
    ?PR_NAME(registered_as_anything, registered_as('_')).


registered_as(Name) when is_atom(Name) ->
    M = #'porkrind.matcher'{
        name = registred_as,
        args = [Name],
        match = fun(Value) ->
            case get_info(Value, registered_name) of
                {registered_name, Name} ->
                    ok;
                {registered_name, _} when Name == '_' ->
                    ok;
                {registered_name, Mismatch} ->
                    ?PR_FAIL({not_registered_as, Value, Mismatch});
                [] ->
                    ?PR_FAIL({not_registered, Value});
                undefined ->
                    ?PR_FAIL({dead, Value})
            end
        end,
        reason = fun
            ({dead, Value}) ->
                io_lib:format("~w is not alive", [Value]);
            ({not_registered, Value}) ->
                io_lib:format("~w is not registered", [Value]);
            ({not_registered_as, Value, Mismatch}) ->
                Fmt = "~w is registered as ~w, not ~w",
                io_lib:format(Fmt, [Value, Mismatch, Name])
        end
    },
    check_pid_or_port(M).



has_link(Target) when is_pid(Target) ->
    M1 = has_info_item(links, Target),
    M2 = M1#'porkrind.matcher'{
        name = has_link,
        args = [Target]
    },
    check_pid_or_port(M2).


is_linked() ->
    M1 = has_info_items(links),
    M2 = M1#'porkrind.matcher'{
        name = is_linked,
        args = []
    },
    check_pid_or_port(M2).


is_not_linked() ->
    M1 = has_no_info_items(links),
    M2 = M1#'porkrind.matcher'{
        name = is_not_linked,
        args = []
    },
    check_pid_or_port(M2).


has_monitor(Target) when is_pid(Target) ->
    has_monitor({process, Target});

has_monitor(Target) when is_port(Target) ->
    has_monitor({port, Target});

has_monitor({Type, Obj} = Target) when Type == process; Type == port ->
    M1 = has_info_item(monitors, Target),
    M2 = M1#'porkrind.matcher'{
        name = has_monitor,
        args = [Obj]
    },
    check_pid_or_port(M2).


is_monitoring() ->
    M1 = has_info_items(monitors),
    M2 = M1#'porkrind.matcher'{
        name = is_monitoring,
        args = []
    },
    check_pid_or_port(M2).


is_not_monitoring() ->
    M1 = has_no_info_items(monitors),
    M2 = M1#'porkrind.matcher'{
        name = is_not_monitoring,
        args = []
    },
    check_pid_or_port(M2).


is_monitored_by(Target) when is_pid(Target) ->
    M1 = has_info_item(monitored_by, Target),
    M2 = M1#'porkrind.matcher'{
        name = is_monitored_by,
        args = [Target]
    },
    check_pid_or_port(M2).


is_monitored() ->
    M1 = has_info_items(monitored_by),
    M2 = M1#'porkrind.matcher'{
        name = is_monitored,
        args = []
    },
    check_pid_or_port(M2).


is_not_monitored() ->
    M1 = has_no_info_items(monitored_by),
    M2 = M1#'porkrind.matcher'{
        name = is_not_monitored,
        args = []
    },
    check_pid_or_port(M2).


has_info_item(Key, Item) ->
    #'porkrind.matcher'{
        name = has_info_item,
        match = fun(Value) ->
            case get_info(Value, Key) of
                {Key, InfoItems} ->
                    case lists:member(Item, InfoItems) of
                        true ->
                            ok;
                        false ->
                            ?PR_FAIL({not_found, Value, InfoItems})
                    end;
                undefined ->
                    ?PR_FAIL({dead, Value})
            end
        end,
        reason = fun
            ({dead, Value}) ->
                io_lib:format("~w is not alive", [Value]);
            ({not_found, Value, InfoItems}) ->
                Fmt = "~w has ~s: ~w, which does not include ~w",
                io_lib:format(Fmt, [Value, Key, InfoItems, Item])
        end
    }.


has_info_items(Key) ->
    #'porkrind.matcher'{
        match = fun(Value) ->
            case get_info(Value, Key) of
                {Key, []} ->
                    ?PR_FAIL({no_items, Value});
                {Key, InfoItems} when length(InfoItems) > 0 ->
                    ok;
                undefined ->
                    ?PR_FAIL({dead, Value})
            end
        end,
        reason = fun
            ({dead, Value}) ->
                io_lib:format("~w is not alive", [Value]);
            ({no_items, Value}) ->
                io_lib:format("~w has no ~w", [Value, Key])
        end
    }.


has_no_info_items(Key) ->
    #'porkrind.matcher'{
        match = fun(Value) ->
            case get_info(Value, Key) of
                {Key, []} ->
                    ok;
                {Key, InfoItems} when length(InfoItems) > 0 ->
                    ?PR_FAIL({has_items, Value, InfoItems});
                undefined ->
                    ?PR_FAIL({dead, Value})
            end
        end,
        reason = fun
            ({dead, Value}) ->
                io_lib:format("~w is not alive", [Value]);
            ({has_items, Value, InfoItems}) ->
                io_lib:format("~w has ~w: ~w", [Value, Key, InfoItems])
        end
    }.


get_info(Pid, Key) when is_pid(Pid) ->
    erlang:process_info(Pid, Key);

get_info(Port, Key) when is_port(Port) ->
    erlang:port_info(Port, Key).


check_pid_or_port(Matcher) ->
    porkrind_logic:all_of([
        porkrind_logic:any_of([
            porkrind_types:is_pid(),
            porkrind_types:is_port()
        ]),
        Matcher
    ]).
