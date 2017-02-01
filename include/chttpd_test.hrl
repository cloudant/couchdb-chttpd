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

-include_lib("chttpd_test_data.hrl").

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("porkrind/include/porkrind.hrl").
-compile({parse_transform, chttpd_test_pt}).
-endif.

-define(http(M, U), chttpd_test_http:http(M, U)).
-define(http(M, U, P), chttpd_test_http:http(M, U, P)).

-import(chttpd_test_http, [
    is_http_response/0,

    has_status/1,
    has_status/2,

    has_header/2,
    has_headers/1,
    has_header_matching/2,
    has_headers_matching/1,
    has_header_matching_re/2,
    has_header_matching_re/3,

    has_json_body/0,
    has_json_body_matching/1
]).
