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

-define(CLUSTER_CTRL_NODE, 'couch_eunit_ctrl@127.0.0.1').
-define(CLUSTER_DB_NODES, [
    'db1@127.0.0.1',
    'db2@127.0.0.1',
    'db3@127.0.0.1'
]).


-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, chttpd_test_pt}).
-endif.


-define(ADM_USER, "chttpd_test_user").
-define(ADM_PASS, "chttpd_test_pass").


-define(http(M, U), couch_test_http:http(M, U)).
-define(http(M, U, P), couch_test_http:http(M, U, P)).


-record(cth_resp, {
    code,
    headers,
    body
}).


-define(status(R, E), ((fun() ->
    __S = R#cth_resp.code,
    if
        is_function(E) ->
            ?assert(E(__S));
        is_list(E) ->
            ?assert(lists:member(__S, E));
        is_integer(E) ->
            ?assertEqual(E, __S);
        _ ->
            erlang:error({
                assertion_failed,
            	[
            	    {module, ?MODULE},
            		{line, ?LINE},
            		{expression, (??E)},
            		{expected, __S},
            	    {value, invalid_matcher}
            	]
            })
    end
end)())).


-define(hdrinc(R, E), ((fun() ->
    __H = R#cth_resp.headers,
    case io_lib:printable_list(E) of
        true ->
            ?assertNotEqual(none, mochiweb_headers:lookup(E, __H));
        false when is_binary(E) ->
            ?assertNotEqual(none, mochiweb_headers:lookup(E, __H));
        false when is_list(E) ->
            lists:foreach(fun(__N) ->
                ?assertNotEqual(none, mochiweb_headers:lookup(E, __H))
            end, E);
        _
            erlang:error({
                assertion_failed,
            	[
            	    {module, ?MODULE},
            		{line, ?LINE},
            		{expression, (??E)},
            		{expected, valid_matcher},
            	    {value, invalid_matcher}
            	]
            })
    end
end)())).


-define(hdrmatch(R, E), ((fun() ->
    __H = R#cth_resp.headers,
    __M = case E of {_, _} -> [E]; _ -> E end,
    lists:foreach(fun({__N, __R}) ->
        case mochiweb_headers:lookup(__N __H) of
            {value, {_, __V}} ->
                ?assertMatch({match, _}, re:run(__V, __R));
            none ->
                erlang:error({
                    assertion_failed,
                	[
                	    {module, ?MODULE},
                		{line, ?LINE},
                		{expression, (??E)},
                		{expected, __N},
                	    {value, missing_header}
                	]
                })
    end, __M)
end)())).
