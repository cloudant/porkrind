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

-record('porkrind.matcher', {
    name,
    args,
    match, % fun(Value) -> any() - throw to signal error, see ?PR_FAIL/1
    reason % fun(Value) -> iolist - describe the failure
}).


-define(IS_MATCHER(M), is_record(M, 'porkrind.matcher')).

-define(PR_FAIL(Reason), throw({porkrind_fail, Reason})).
-define(PR_EQUAL(Value, Expect), begin
    if Value == Expect -> ok; true ->
        throw({porkrind_fail, {not_equal, Value, Expect}})
    end
end).

-define(PR_NAME(Name, Base), ((Base)#'porkrind.matcher'{name = Name})).
