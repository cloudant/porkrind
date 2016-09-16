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

-module(porkrind_fun_calls).


-include("porkrind_internal.hrl").


-export([
    calling/3,
    does_not_raise/0,
    raises/0,
    raises/1,
    raises/2
]).



calling(Mod, Fun, Args) when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    % Rather than create an anonymous function here
    % we pass the required bits as a tuple through so
    % that test formatting is prettier.
    {apply, Mod, Fun, Args}.


does_not_raise() ->
    #'porkrind.matcher'{
        name = does_not_raise,
        args = [],
        match = fun(Value) ->
            Fun = to_fun(Value),
            try Fun() of
                _ ->
                    ok
            catch T:R ->
                ?PR_FAIL({raised, Value, T, R})
            end
        end,
        reason = fun
            ({bad_fun, Value}) ->
                io_lib:format("~p is not a zero arity function", [Value]);
            ({raised, Value, T, R}) ->
                FunStr = format_fun(Value),
                Raised = format_exc(T, R),
                io_lib:format("~s raised ~s", [FunStr, Raised])
        end
    }.


raises() ->
    raises('_', '_').


raises(Type) ->
    raises(Type, '_').


raises(Type, Reason)
        when Type == throw; Type == error; Type == exit; Type == '_' ->
    ReasonMatcher = porkrind_util:maybe_wrap(Reason),
    #'porkrind.matcher'{
        name = raises,
        args = [Type, Reason],
        match = fun(Value) ->
            Fun = to_fun(Value),
            try Fun() of
                _ ->
                    ?PR_FAIL({did_not_raise, Value})
            catch
                T:R when (T == Type orelse Type == '_') ->
                    case porkrind:check(R, ReasonMatcher) of
                        ok ->
                            ok;
                        {assertion_failed, _} ->
                            ?PR_FAIL({raised, Value, T, R})
                    end;
                T:R ->
                    ?PR_FAIL({raised, Value, T, R})
            end
        end,
        reason = fun
            ({bad_fun, Value}) ->
                io_lib:format("~p is not a zero arity function", [Value]);
            ({did_not_raise, Value}) ->
                FunStr = format_fun(Value),
                ExcStr = format_exc(Type, Reason),
                io_lib:format("~s did not raise ~s", [FunStr, ExcStr]);
            ({raised, Value, T, R}) ->
                FunStr = format_fun(Value),
                Raised = format_exc(T, R),
                ExpStr = format_exc(Type, Reason),
                io_lib:format("~s raised ~s, not ~s", [FunStr, Raised, ExpStr])
        end
    }.


format_fun({apply, Mod, Fun, Args}) ->
    porkrind_util:mfa_to_string(Mod, Fun, Args);

format_fun(Fun) ->
    Info = erlang:fun_info(Fun),
    {module, Mod} = lists:keyfind(module, 1, Info),
    {name, Name} = lists:keyfind(name, 1, Info),
    {arity, Arity} = lists:keyfind(arity, 1, Info),
    porkrind_util:mfa_to_string(Mod, Name, Arity).


format_exc('_', '_') ->
    "any exception";

format_exc(Type, '_') ->
    io_lib:format("a ~w exception", [Type]);

format_exc(Type, Reason) when ?IS_MATCHER(Reason) ->
    Prefix = io_lib:format("~w with reason matching ", [Type]),
    [Prefix, porkrind:describe(Reason)];

format_exc(Type, Reason) ->
    io_lib:format("~w:~w", [Type, Reason]).


to_fun({apply, Mod, Fun, Args}) ->
    fun() -> erlang:apply(Mod, Fun, Args) end;

to_fun(Fun) when is_function(Fun, 0) ->
    Fun;

to_fun(Else) ->
    ?PR_FAIL({bad_fun, Else}).
