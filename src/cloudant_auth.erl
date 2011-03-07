%%% @author Adam Kocoloski
%%% @doc custom auth/auth handler for Cloudant-flavored CouchDB
%%% @copyright 2009 26 Solutions, Inc. All rights reserved.

-module(cloudant_auth).
-author('Adam Kocoloski <adam@cloudant.com>').

-export([authorize_request/1]).

-include_lib("couch/include/couch_db.hrl").

authorize_request(#httpd{auth=Auth, user_ctx=Ctx} = Req) ->
    try authorize_request_int(Req)
    catch throw:{forbidden, Msg} ->
        case {Auth, Ctx} of
        {{cookie_auth_failed, {Error, Reason}}, _} ->
            throw({forbidden, Error, Reason});
        {_, #user_ctx{name=null}} ->
            throw({unauthorized, Msg});
        {_, _} ->
            throw({forbidden, Msg})
        end
    end.

authorize_request_int(#httpd{path_parts=[]}=Req) ->
    Req;

authorize_request_int(#httpd{path_parts=[<<"favicon.ico">>|_]}=Req) ->
    Req;

authorize_request_int(#httpd{path_parts=[<<"_", _/binary>>|_]}=Req) ->
    server_authorization_check(Req, (Req#httpd.user_ctx)#user_ctx.roles),
    Req;

authorize_request_int(#httpd{path_parts=[_DbName, <<"_compact">> | _]}=Req) ->
    try require_role(server_admin, (Req#httpd.user_ctx)#user_ctx.roles),
        Req
    catch throw:{forbidden, _} ->
        throw({forbidden, <<"compaction is automatic on Cloudant">>})
    end;

authorize_request_int(#httpd{path_parts=[_DbName, <<"_temp_view">> | _]}=Req) ->
    try require_role(server_admin, (Req#httpd.user_ctx)#user_ctx.roles),
        Req
    catch throw:{forbidden, _} ->
        throw({forbidden, <<"temp views are disabled on Cloudant">>})
    end;

authorize_request_int(#httpd{path_parts=[_DbName], method='PUT'}=Req) ->
    case is_request_by_owner(Req) of
    true ->
        Req#httpd{user_ctx = #user_ctx{roles = [<<"_admin">>]}};
    false ->
        require_role(server_admin, (Req#httpd.user_ctx)#user_ctx.roles),
        Req
    end;

authorize_request_int(#httpd{path_parts=[DbName|_]}=Req) ->
    User = Req#httpd.user_ctx,
    NewUser = add_roles(User, assign_roles(DbName, User#user_ctx.name, Req)),
    db_authorization_check(Req, NewUser#user_ctx.roles),
    Req#httpd{user_ctx=NewUser}.

%%=============================================================================
%% internal functions
%%=============================================================================

add_roles(#user_ctx{} = Ctx, Roles) ->
    NewRoles = Roles ++ Ctx#user_ctx.roles,
    Ctx#user_ctx{roles=NewRoles}.

is_request_by_owner(#httpd{user_ctx=#user_ctx{name=null}}) ->
    false;
is_request_by_owner(Req) ->
    User = (Req#httpd.user_ctx)#user_ctx.name,
    UserAsList = ?b2l(User),
    case cloudant_util:customer_name(Req) of
    UserAsList ->
        true;
    _Else ->
        UserPath = ?l2b(cloudant_util:customer_path(Req)),
        Size = byte_size(UserPath),
        case Req#httpd.path_parts of
        [<<UserPath:Size/binary, _/binary>>|_] ->
            true;
        _ ->
            false
        end
    end.

assign_roles(DbName, User, Req) ->
    SecProps = try fabric:get_security(DbName) of
        {Props} -> Props
    catch error:database_does_not_exist ->
        []
    end,
    {CloudantProps} = couch_util:get_value(<<"cloudant">>, SecProps, {[]}),
    case couch_util:get_value(User, CloudantProps) of
    undefined ->
        case is_request_by_owner(Req) of
        true ->
            [<<"_reader">>, <<"_writer">>, <<"_admin">>];
        false ->
            nobody_roles(Req, CloudantProps)
        end;
    Roles ->
        Roles
    end.

nobody_roles(Req, Props) ->
    Host = chttpd:header_value(Req, "Host"),
    case couch_util:get_value(?l2b(Host), Props) of
    undefined ->
        couch_util:get_value(<<"nobody">>, Props, []);
    Else ->
        Else
    end.

require_localhost(Req, Msg) ->
    case string:tokens(couch_httpd:header_value(Req, "Host", ""), ":") of
    [Host|_] when Host =:= "localhost"; Host =:= "127.0.0.1" ->
        ok;
    _Else ->
        throw({forbidden, Msg})
    end.

require_role(Role, Roles) ->
    case lists:member(Role, Roles) of
    true ->
        ok;
    false ->
        Msg = [couch_util:to_list(Role), " access is required for this request"],
        throw({forbidden, Msg})
    end.

require_any_role(Required, Roles) ->
    Results = [catch require_role(R, Roles) || R <- Required],
    case lists:member(ok, Results) of
    true ->
        ok;
    false ->
        List = lists:foldl(fun(X, A) -> [couch_util:to_list(X), ", "| A] end,
            couch_util:to_list(hd(Required)), tl(Required)),
        throw({forbidden, ["one of ", List, " is required for this request"]})
    end.

server_authorization_check(#httpd{path_parts=[<<"_all_dbs">>|_]}=Req, Roles) ->
    case is_request_by_owner(Req) of
    true -> ok;
    false ->
        require_any_role([server_admin, <<"_admin">>], Roles)
    end;

server_authorization_check(#httpd{path_parts=[<<"_up">>|_]}, _Roles) ->
    ok;

server_authorization_check(#httpd{path_parts=[<<"_uuids">>|_]}, _Roles) ->
    ok;

server_authorization_check(#httpd{path_parts=[<<"_cloudant">>,<<"status">>|_]}, _) ->
    ok;

server_authorization_check(#httpd{path_parts=[<<"_cluster">>|_]}, _Roles) ->
    ok;

server_authorization_check(#httpd{path_parts=[<<"_session">>|_]}, _Roles) ->
    ok;

server_authorization_check(#httpd{path_parts=[<<"_replicate">>]}, _Roles) ->
    ok;

server_authorization_check(#httpd{method=Method, path_parts=[<<"_utils">>,
        <<"script">>|_]}, _) when Method =:= 'HEAD' orelse Method =:= 'GET' ->
    ok;

server_authorization_check(#httpd{path_parts=[<<"_stats">>|_]} = Req, Roles) ->
    try require_role(server_admin, Roles)
    catch throw:{forbidden, Msg} -> require_localhost(Req, Msg) end;

server_authorization_check(#httpd{path_parts=[<<"_metrics">>|_]} = Req, Roles) ->
    try require_role(server_admin, Roles)
    catch throw:{forbidden, Msg} -> require_localhost(Req, Msg) end;

server_authorization_check(#httpd{path_parts=[<<"_active_tasks">>]} = Req,
        Roles) ->
    case is_request_by_owner(Req) of
    true -> ok;
    false ->
        require_any_role([server_admin, <<"_admin">>], Roles)
    end;

server_authorization_check(#httpd{path_parts=[<<"_", _/binary>>|_]}, Roles) ->
    require_role(server_admin, Roles).

db_authorization_check(#httpd{path_parts=[_DbName], method='DELETE'}, Roles) ->
    require_role(<<"_admin">>, Roles);

db_authorization_check(#httpd{method='POST', path_parts=[_DbName]}, Roles) ->
    require_any_role([<<"_creator">>, <<"_writer">>], Roles);

db_authorization_check(#httpd{path_parts=[_,<<"_all_docs">>]}, Roles) ->
    require_role(<<"_reader">>, Roles);

db_authorization_check(#httpd{path_parts=[_,<<"_all_docs_by_seq">>]}, Roles) ->
    require_role(<<"_reader">>, Roles);

db_authorization_check(#httpd{path_parts=[_,<<"_local">>,<<"authorization">>]},
        Roles) ->
    require_role(<<"_admin">>, Roles);

db_authorization_check(#httpd{path_parts=[_,<<"_security">>]},
        Roles) ->
    require_role(<<"_admin">>, Roles);

db_authorization_check(#httpd{path_parts=[_,<<"_ensure_full_commit">>]},
        Roles) ->
    require_role(<<"_writer">>, Roles);

db_authorization_check(#httpd{path_parts=[_,<<"_design">>,_,<<"_view">>|_]},
        Roles) ->
    require_role(<<"_reader">>, Roles);

db_authorization_check(#httpd{path_parts=[_,<<"_design">>,_,<<"_show">>|_]},
        Roles) ->
    require_role(<<"_reader">>, Roles);

db_authorization_check(#httpd{path_parts=[_,<<"_design">>,_,<<"_list">>|_]},
        Roles) ->
    require_role(<<"_reader">>, Roles);

db_authorization_check(#httpd{path_parts=[_,<<"_design">>,_,<<"_update">>|_]},
        Roles) ->
    require_role(<<"_writer">>, Roles);

db_authorization_check(#httpd{path_parts=[_,<<"_design">>,_,<<"_rewrite">>|_]},
        _Roles) ->
    % we'll apply the authz checks on the rewritten request
    ok;

db_authorization_check(#httpd{method=Method, path_parts=[_,<<"_design", _/binary>>|_]},
        Roles) when Method =:= 'HEAD' orelse Method =:= 'GET' ->
    require_role(<<"_reader">>, Roles);


db_authorization_check(#httpd{path_parts=[_,<<"_design", _/binary>>|_]}, Roles) ->
    require_role(<<"_admin">>, Roles);

db_authorization_check(#httpd{method=Method}, Roles) when
        Method =:= 'HEAD' orelse Method =:= 'GET' ->
    require_role(<<"_reader">>, Roles);

db_authorization_check(_Req, Roles) ->
    require_role(<<"_writer">>, Roles).

