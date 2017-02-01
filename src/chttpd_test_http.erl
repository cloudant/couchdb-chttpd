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


-include_lib("porkrind/include/porkrind.hrl").
-include("chttpd_test_data.hrl").


-export([
    url/1,

    http/2,
    http/3,

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
    DbNode = hd(?CLUSTER_DB_NODES),
    {Addr, Port} = rpc:call(DbNode, erlang, apply, [GetAddr, []]),
    lists:concat(["http://", Addr, ":", Port, "/", Path]).


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
                headers = RespHeaders,
                body = iolist_to_binary(RespBody)
            };
        Error ->
            erlang:error(Error)
    end.


is_http_response() ->
    #'porkrind.matcher'{
        name = is_http_response,
        args = [],
        match = fun(Value) ->
            if is_record(Value, cth_resp) -> ok; true ->
                ?PR_FAIL({bad_value, Value})
            end
        end,
        reason = fun({bad_value, Value}) ->
            io_lib:format("~p is not an HTTP response", [Value])
        end
    }.


has_status(Code) when is_integer(Code) ->
    M = has_status(fun(Value) -> Value == Code end),
    M#'porkrind.matcher'{
        args = [Code]
    };

has_status(CodeFun) when is_function(CodeFun, 1) ->
    #'porkrind.matcher'{
        name = has_status,
        args = [CodeFun],
        match = fun(Value) ->
            #cth_resp{
                code = Code
            } = Value,
            case (catch CodeFun(Code)) of
                true -> ok;
                ok -> ok;
                Else -> ?PR_FAIL({bad_code, Value, Else})
            end
        end,
        reason = fun({bad_code, Value, BadReturn}) ->
            #cth_resp{
                code = Code
            } = Value,
            Args = [Value, Code, BadReturn],
            io_lib:format("~p has invalid status code ~p: ~p", Args)
        end
    }.


has_status(CodeMin, CodeMax) ->
    M = has_status(fun(Code) -> Code >= CodeMin andalso Code =< CodeMax end),
    M#'porkrind.matcher'{
        args = [CodeMin, CodeMax]
    }.


has_header(Name, Value) ->
    M1 = header_matcher(Name, equal_to_string(Value)),
    M2 = M1#'porkrind.matcher'{
        name = has_header,
        args = [Name, Value]
    },
    all_of([
        is_http_response(),
        M2
    ]).


has_headers(Pairs) ->
    Matchers = lists:map(fun({Name, Value}) ->
        M = header_matcher(Name, equal_to_string(Value)),
        M#'porkrind.matcher'{
            name = has_header,
            args = [Name, Value]
        }
    end, Pairs),
    all_of([is_http_response() | Matchers]).


has_header_matching(Name, Matcher) ->
    M1 = header_matcher(Name, porkrind_util:maybe_wrap(Matcher)),
    M2 = M1#'porkrind.matcher'{
        name = has_header_matching,
        args = [Name, Matcher]
    },
    all_of([
        is_http_response(),
        M2
    ]).


has_headers_matching(Pairs) ->
    Matchers = lists:map(fun({Name, Matcher}) ->
        M = header_matcher(Name, porkrind_util:maybe_wrap(Matcher)),
        M#'porkrind.matcher'{
            name = has_header_matching,
            args = [Name, Matcher]
        }
    end, Pairs),
    all_of([is_http_response() | Matchers]).


has_header_matching_re(Name, RegEx) ->
    M1 = header_matcher(Name, matches_re(RegEx)),
    M2 = M1#'porkrind.matcher'{
        name = has_header_matching_re,
        args = [Name, RegEx]
    },
    all_of([
        is_http_response(),
        M2
    ]).


has_header_matching_re(Name, RegEx, REOpts) ->
    M1 = header_matcher(Name, matches_re(RegEx, REOpts)),
    M2 = M1#'porkrind.matcher'{
        name = has_header_matching_re,
        args = [Name, RegEx, REOpts]
    },
    all_of([
        is_http_response(),
        M2
    ]).


has_json_body() ->
    M = #'porkrind.matcher'{
        name = has_json_body,
        args = [],
        match = fun(Value) ->
            #cth_resp{
                body = Body
            } = Value,
            try couch_util:json_decode(Body) of
                _ -> true
            catch _:_ ->
                ?PR_FAIL({not_json, Value})
            end
        end,
        reason = fun({not_json, Value}) ->
            io_lib:format("~p does not have a JSON body", [Value])
        end
    },
    all_of([is_http_response(), M]).


has_json_body_matching(Matcher) ->
    M = #'porkrind.matcher'{
        name = has_json_body_matching,
        args = [Matcher],
        match = fun(Value) ->
            #cth_resp{
                body = Body
            } = Value,
            EJson = couch_util:json_decode(Body),
            porkrind:match(EJson, Matcher)
        end
    },
    all_of([has_json_body(), M]).


header_matcher(Name, ValueMatcher) ->
    Matcher = has_item(matching({
        equal_ignoring_case(Name),
        ValueMatcher
    })),
    #'porkrind.matcher'{
        match = fun(Value) ->
            #cth_resp{
                headers = Headers
            } = Value,
            porkrind:match(Headers, Matcher)
        end
    }.


maybe_start_ibrowse() ->
    {ok, _} = application:ensure_all_started(ibrowse).
