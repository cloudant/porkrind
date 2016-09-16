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

-module(porkrind_atoms).

-include("porkrind_internal.hrl").


-export([
    is_true/0,
    is_false/0,
    is_undefined/0,
    is_null/0
]).


is_true() ->
    ?PR_NAME(is_true, same_atom(true)).


is_false() ->
    ?PR_NAME(is_false, same_atom(false)).


is_undefined() ->
    ?PR_NAME(is_undefined, same_atom(undefined)).


is_null() ->
    ?PR_NAME(is_null, same_atom(null)).


same_atom(Atom) when is_atom(Atom) ->
    porkrind_logic:all_of([
        porkrind_types:is_atom(),
        porkrind_logic:equal_to(Atom)
    ]).
