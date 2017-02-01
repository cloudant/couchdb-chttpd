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

-module(chttpd_api_all_dbs).
-test_suite(chttpd_api).


-include_lib("chttpd/include/chttpd_test.hrl").


t_all_dbs() ->
    Resp = ?http(get, "_all_dbs"),
    ?assert_that(Resp, has_status(200)),
    ?assert_that(Resp, has_json_body_matching(only_contains(is_binary()))).
