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

-module(chttpd_api_test).

-include_lib("eunit/include/eunit.hrl").
-include("chttpd_test.hrl").


api_test_() ->
    {
        setup,
        fun chttpd_test_util:start_cluster/0,
        fun chttpd_test_util:stop_cluster/1,
        [
            fun check_nodes/0,
            fun check_mem3_nodes/0
        ] ++ chttpd_test_util:collect_tests(chttpd_api)
    }.


check_nodes() ->
    Expect = lists:sort([?CLUSTER_CTRL_NODE | ?CLUSTER_DB_NODES]),
    ?assertEqual(Expect, lists:sort([node() | nodes()])).


check_mem3_nodes() ->
    Expect = lists:sort(?CLUSTER_DB_NODES),
    lists:foreach(fun(Node) ->
        Resp = rpc:call(Node, mem3, nodes, []),
        ?assertEqual(Expect, lists:sort(Resp))
    end, ?CLUSTER_DB_NODES).
