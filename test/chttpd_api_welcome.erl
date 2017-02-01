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

-module(chttpd_api_welcome).
-test_suite(chttpd_api).


-include_lib("chttpd/include/chttpd_test.hrl").


t_welcome() ->
    Resp = ?http(get, "/"),
    ?assert_that(Resp, has_status(200)),
    ?assert_that(Resp, has_headers_matching([
        {"Server", '_'},
        {"Date", '_'}
    ])),
    ?assert_that(Resp, has_header("Content-Type", "application/json")),
    ?assert_that(Resp, has_json_body_matching(all_of([
        has_json_entry(couchdb, <<"Welcome">>),
        has_json_matching(vendor, is_json_object()),
        has_json_matching(version, is_binary())
    ]))).
