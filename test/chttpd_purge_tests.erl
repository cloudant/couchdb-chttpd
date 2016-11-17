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

-module(chttpd_purge_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_db_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).

setup() ->
    ok = config:set("admins", ?USER, ?PASS, _Persist=false),
    TmpDb = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(TmpDb)]),
    create_db(Url),
    Url.

teardown(Url) ->
    delete_db(Url),
    ok = config:delete("admins", ?USER, _Persist=false).

create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).


create_doc(Url, Id) ->
    test_request:put(Url ++ "/" ++ Id,
        [?CONTENT_JSON, ?AUTH], "{\"mr\": \"rockoartischocko\"}").

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

purge_test_() ->
    {
        "chttpd db tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0, fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun test_ok_purge_request/1,
                    fun test_error_purge_request/1,
                    fun should_error_set_purged_docs_limit_to0/1
                ]
            }
        }
    }.


test_ok_purge_request(Url) ->
    ?_test(begin
        {ok, _, _, Body} = create_doc(Url, "doc1"),
        {Json} = ?JSON_DECODE(Body),
        Rev1 = couch_util:get_value(<<"rev">>, Json, undefined),
        {ok, _, _, Body2} = create_doc(Url, "doc2"),
        {Json2} = ?JSON_DECODE(Body2),
        Rev2 = couch_util:get_value(<<"rev">>, Json2, undefined),
        {ok, _, _, Body3} = create_doc(Url, "doc3"),
        {Json3} = ?JSON_DECODE(Body3),
        Rev3 = couch_util:get_value(<<"rev">>, Json3, undefined),
        IdsRevs = "{\"doc1\": [\"" ++ ?b2l(Rev1) ++ "\"], \"doc2\": [\"" ++
            ?b2l(Rev2) ++ "\"], \"doc3\": [\"" ++ ?b2l(Rev3) ++ "\"] }",

        {ok, Status, _, ResultBody} = test_request:post(Url ++ "/_purge/",
            [?CONTENT_JSON, ?AUTH], IdsRevs),
        ?assert(Status =:= 201 orelse Status =:= 202),
        ResultJson = ?JSON_DECODE(ResultBody),
        ?assertMatch(
            {[
                {<<"purged">>, [
                    {[{<<"ok">>,true}, {<<"id">>,<<"doc1">>}, {<<"revs">>,[Rev1]}]},
                    {[{<<"ok">>,true}, {<<"id">>,<<"doc2">>}, {<<"revs">>,[Rev2]}]},
                    {[{<<"ok">>,true}, {<<"id">>,<<"doc3">>}, {<<"revs">>,[Rev3]}]}
                ]},
                {<<"purge_seq">>, _}
            ]},
            ResultJson
        )
    end).


test_error_purge_request(Url) ->
    ?_test(begin
        % set purged_docs_limit to 1
        PDocsLimit = 1,
        {ok, Status, _, _} = test_request:put(Url ++ "/_purged_docs_limit/",
           [?CONTENT_JSON, ?AUTH], integer_to_list(PDocsLimit)),
        ?assert(Status =:= 200),

        % test db purged_docs_limit is set to PDocsLimit
        {ok, Status2, _, ResultBody2} = test_request:get(Url ++
            "/_purged_docs_limit/", [?AUTH]),
        ?assert(Status2 =:= 200),
        ResultJson2 = ?JSON_DECODE(ResultBody2),
        ?assertEqual(PDocsLimit, ResultJson2),

        % doc1 and doc5 fall on the same shard,
        % and exceed purged_docs_limit for this shard
        {ok,_,_,Body} = create_doc(Url, "doc1"), {Json} = ?JSON_DECODE(Body),
        Rev1 = couch_util:get_value(<<"rev">>, Json),
        {ok,_,_,Body2} = create_doc(Url, "doc2"), {Json2} = ?JSON_DECODE(Body2),
        Rev2 = couch_util:get_value(<<"rev">>, Json2),
        {ok,_,_,Body3} = create_doc(Url, "doc3"), {Json3} = ?JSON_DECODE(Body3),
        Rev3 = couch_util:get_value(<<"rev">>, Json3),
        {ok,_,_,Body4} = create_doc(Url, "doc4"), {Json4} = ?JSON_DECODE(Body4),
        Rev4 = couch_util:get_value(<<"rev">>, Json4),
        {ok,_,_,Body5} = create_doc(Url, "doc5"), {Json5} = ?JSON_DECODE(Body5),
        Rev5 = couch_util:get_value(<<"rev">>, Json5),
        IdsRevs = "{\"doc1\": [\"" ++ ?b2l(Rev1) ++ "\"], \"doc2\": [\"" ++
            ?b2l(Rev2) ++ "\"], \"doc3\": [\"" ++ ?b2l(Rev3) ++
            "\"], \"doc4\": [\"" ++ ?b2l(Rev4) ++ "\"], \"doc5\": [\"" ++
            ?b2l(Rev5) ++ "\"] }",

        {ok, Status3, _, ResultBody3} = test_request:post(Url ++ "/_purge/",
            [?CONTENT_JSON, ?AUTH], IdsRevs),
        ?assert(Status3 =:= 201 orelse Status3 =:= 202),
        ResultJson3 = ?JSON_DECODE(ResultBody3),
        ?assertMatch(
            {[
                {<<"purged">>, [
                    {[{<<"id">>,<<"doc1">>}, {<<"error">>,<<"error">>}, {<<"reason">>,<<"purged_docs_limit_exceeded">>}]},
                    {[{<<"ok">>,true}, {<<"id">>,<<"doc2">>}, {<<"revs">>,[Rev2]}]},
                    {[{<<"ok">>,true}, {<<"id">>,<<"doc3">>}, {<<"revs">>,[Rev3]}]},
                    {[{<<"ok">>,true}, {<<"id">>,<<"doc4">>}, {<<"revs">>,[Rev4]}]},
                    {[{<<"id">>,<<"doc5">>}, {<<"error">>,<<"error">>}, {<<"reason">>,<<"purged_docs_limit_exceeded">>}]}
                ]},
                {<<"purge_seq">>, _}
            ]},
            ResultJson3
        )
    end).


should_error_set_purged_docs_limit_to0(Url) ->
    ?_test(begin
        % set purged_docs_limit to 0
        {ok, Status, _, _} = test_request:put(Url ++ "/_purged_docs_limit/",
            [?CONTENT_JSON, ?AUTH], "0"),
        ?assert(Status =:= 400)
    end).