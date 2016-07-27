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

-module(chttpd_test_pt).


-export([
    parse_transform/2
]).


parse_transform(Forms, _Opts) ->
    TestFuns = collect_test_names(Forms, []),
    if length(TestFuns) == 0 -> Forms; true ->
        add_export(Forms, TestFuns)
    end.


collect_test_names([], Acc) ->
    lists:sort(Acc);

collect_test_names([{function, _L, Name, 0, _Cs} | RestForms], Acc) ->
    case atom_to_list(Name) of
        "t_" ++ _ ->
            collect_test_names(RestForms, [{Name, 0} | Acc]);
        _ ->
            collect_test_names(RestForms, Acc)
    end;

collect_test_names([_ | RestForms], Acc) ->
    collect_test_names(RestForms, Acc).


add_export([{attribute, _, module, _} = M | RestForms], Exports) ->
    ExportAttr = {attribute, 0, export, Exports},
    [M, ExportAttr | RestForms];

add_export([F | RestForms], Exports) ->
    [F | add_export(RestForms, Exports)];

add_export([], _) ->
    % No module export?
    [].
