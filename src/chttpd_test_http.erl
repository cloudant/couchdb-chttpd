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

-module(chttpd_test_http).


-include("chttpd_test.hrl").


-export([
    http/2,
    http/3
]).


http(Method, Url) ->
    http(Method, Url, []).


http(Method, Url, Params) ->
    maybe_start_ibrowse(),
    Headers = couch_util:get_value(headers, Params, []),
    Body = couch_util:get_value(body, Params, []),
    Opts = couch_util:get_value(opts, Params, []),
    case ibrowse:send_req(url(Url), Headers, Method, Body, Opts) of
        {ok, Code, RespHeaders, RespBody} ->
            #cth_resp{
                code = list_to_integer(Code),
                headers = mochiweb_headers:from_list(RespHeaders),
                body = iolist_to_binary(RespBody)
            };
        Error ->
            erlang:error(Error)
    end.


url("http://" ++ _ = Url) ->
    Url;
url("https://" ++ _ = Url) ->
    Url;
url("/" ++ Url) ->
    url(Url);
url(Path) when is_binary(Path) ->
    url(binary_to_list(Path));
url(Path) when is_list(Path) ->
    GetAddr = fun() ->
        Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
        Port = mochiweb_socket_server:get(chttpd, port),
        {Addr, Port}
    end,
    DbNode = 'db1@127.0.0.1', % Hack for now. Need to rearrange the test header
    {Addr, Port} = rpc:call(DbNode, erlang, apply, [GetAddr, []]),
    lists:concat(["http://", Addr, ":", Port, "/", Path]).


maybe_start_ibrowse() ->
    {ok, _} = application:ensure_all_started(ibrowse).
