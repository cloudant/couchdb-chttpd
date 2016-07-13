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

-module(chttpd_shard).
-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-export([
    handle_shard_req/2
]).


handle_shard_req(#httpd{} = Req, Db) ->
    #httpd{
        path_parts = [_, _ | RestParts]
    } = Req,
    case RestParts of
        [] ->
            throw({not_found, <<"No shard range specified">>});
        [Range | _] ->
            Shard = get_shard(Db, Range),
            redispatch(Req, Shard, Db)
    end.


get_shard(Db, Range) ->
    {Start, End} = parse_range(Range),
    Shards = mem3:shards(mem3:dbname(Db#db.name)),
    FiltFun = fun(#shard{node = N, range = R}) ->
        N == node() andalso R == [Start, End]
    end,
    case lists:filter(FiltFun, Shards) of
        [] ->
            throw({not_found, <<"Range does not exist on this node.">>});
        [Shard] ->
            Shard;
        [_ | _] ->
            erlang:error({kaboom, multiple_shard_ranges_on_node})
    end.


parse_range(<<StartBin:8/binary, "-", EndBin:8/binary>>) ->
    Start = try
        httpd_util:hexlist_to_integer(binary_to_list(StartBin))
    catch _T:_R ->
        throw({bad_request, <<"Invalid range start">>})
    end,
    End = try
        httpd_util:hexlist_to_integer(binary_to_list(EndBin))
    catch _:_ ->
        throw({bad_request, <<"Invalid range end">>})
    end,
    {Start, End};

parse_range(_) ->
    throw({bad_request, <<"Invalid range specification">>}).


redispatch(ReqIn, Shard, Db) ->
    #httpd{
        method = Method,
        path_parts = [_, _, _ | RestParts]
    } = ReqIn,
    ReqOut = ReqIn#httpd{
        path_parts = [Shard#shard.name | RestParts],
        api_module = fabric_local
    },
    case {Method, RestParts} of
        {'PUT', []} ->
            chttpd:send_method_not_allowed(ReqIn, "GET,HEAD,POST");
        {'DELETE', []} ->
            chttpd:send_method_not_allowed(ReqIn, "GET,HEAD,POST");
        {_, []} ->
            redispatch(ReqOut, Shard, Db, fun chttpd_db:db_req/2);
        {_, [Part | _]} ->
            Handler = chttpd_handlers:db_handler(Part, fun chttpd_db:db_req/2),
            redispatch(ReqOut, Shard, Db, Handler)
        end.


redispatch(Req, Shard, DbIn, Handler) ->
    case couch_db:open(Shard#shard.name, [{user_ctx, DbIn#db.user_ctx}]) of
        {ok, Db} ->
            try
                Handler(Req, Db)
            after
                couch_db:close(Db)
            end;
        Error ->
            throw(Error)
    end.
