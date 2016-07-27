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

-module(chttpd_test_util).


-export([
    start_couch/0,
    start_couch/1,
    stop_couch/1,

    start_cluster/0,
    stop_cluster/1,

    collect_tests/1
]).

-export([
    init_cluster_node/0
]).


-include_lib("couch/include/couch_eunit.hrl").
-include("chttpd_test.hrl").


start_couch() ->
    start_couch(?CONFIG_CHAIN).

start_couch(IniFiles) ->
    test_util:start_couch(IniFiles, [chttpd]).

stop_couch(Ctx) ->
    test_util:stop_couch(Ctx).


start_cluster() ->
    maybe_start_networking(),
    lists:map(fun(Node) ->
        [Name, Host] = split_node(Node),
        {ok, Node} = slave:start_link(Host, Name, path_arg()),
        {ok, Ctx} = rpc:call(Node, ?MODULE, init_cluster_node, []),
        {Node, Ctx}
    end, ?CLUSTER_DB_NODES).


stop_cluster(Nodes) ->
    lists:foreach(fun({Node, Ctx}) ->
        ok = rpc:call(Node, ?MODULE, stop_couch, [Ctx]),
        ok = slave:stop(Node)
    end, Nodes).


collect_tests(SuiteName) ->
    {ok, [[Root]]} = init:get_argument(root),
    lists:flatmap(fun(Path) ->
        case lists:prefix(Root, Path) of
            true ->
                [];
            false ->
                collect_tests(SuiteName, Path)
        end
    end, code:get_path()).


collect_tests(SuiteName, Path) ->
    FileNames = filelib:wildcard(filename:join(Path, "*.beam")),
    lists:flatmap(fun(FileName) ->
        ModName = list_to_atom(filename:rootname(filename:basename(FileName))),
        Attrs = ModName:module_info(attributes),
        case lists:keyfind(test_suite, 1, Attrs) of
            {test_suite, Suites} ->
                case lists:member(SuiteName, Suites) of
                    true ->
                        collect_tests_int(ModName);
                    false ->
                        []
                end;
            false ->
                []
        end
    end, FileNames).


collect_tests_int(ModName) ->
    Funs = ModName:module_info(exports),
    lists:flatmap(fun({Name, Arity}) ->
        case atom_to_list(Name) of
            "t_" ++ _ when Arity == 0 ->
                [fun ModName:Name/Arity];
            _ ->
                []
        end
    end, Funs).


init_cluster_node() ->
    [Name, _] = split_node(node()),
    NodeCfg = filename:join([?BUILDDIR(), "tmp", "etc", Name ++ ".ini"]),
    Chain = ?CONFIG_CHAIN ++ [NodeCfg],
    {ok, start_couch(Chain)}.


maybe_start_networking() ->
    case node() of
        'nonode@nohost' ->
            net_kernel:start([?CLUSTER_CTRL_NODE]);
        ?CLUSTER_CTRL_NODE ->
            ok;
        Other ->
            erlang:error({unexpected_test_node, Other})
    end.


split_node(Node) ->
    re:split(atom_to_list(Node), "@", [{return, list}]).


path_arg() ->
    Paths = code:get_path(),
    {ok, [[Root]]} = init:get_argument(root),
    {Prefix, Suffix} = lists:foldl(fun(Path, {Prepend, Append}) ->
        case lists:prefix(Root, Path) of
            true ->
                {Prepend, [" -pz " ++ Path | Append]};
            false ->
                {[" -pa " ++ Path | Prepend], Append}
        end
    end, {[], []}, Paths),
    lists:flatten(["-pa . -pz ../ebin", Prefix, Suffix]).
