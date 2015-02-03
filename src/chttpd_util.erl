-module(chttpd_util).
-export([customer_name/1, customer_db_info/2, customer_path/1, customer_path/2]).

-define(RESERVED_NAMESPACE, "default").

customer_db_info(Req, Info) ->
    FullName = couch_util:get_value(db_name, Info),
    Customer = customer_path(Req),
    DbName = re:replace(FullName, [$^,Customer,$/], "", [{return,binary}]),
    lists:keyreplace(db_name, 1, Info, {db_name,DbName}).

customer_name(Req) ->
    case chttpd:header_value(Req, "X-Couch-User") of
    undefined ->
        customer_from_host_header(chttpd:header_value(Req, "Host"), config:get("chttpd", "domain", ""));
    CouchUser ->
        lists:takewhile(fun (C) -> C =/= $, end, CouchUser)
    end.

customer_path(Req) ->
    customer_path(Req, true).

customer_path(Req, Validate) ->
    DefaultNamespace = config:get("chttpd", "reserved_namespace", ?RESERVED_NAMESPACE),
    CustomerName = case {customer_name(Req), Validate} of
    {DefaultNamespace, true} ->
        throw({bad_request, <<"Attempted to use reserved namespace">>});
    {undefined, _} ->
        DefaultNamespace;
    {Else, _} ->
        Else
    end,
    Labels = string:tokens(CustomerName, "."),
    string:join(lists:reverse(Labels), "/").

%% internal

customer_from_host_header(Host, DomainStr) when is_list(Host), is_list(DomainStr), length(Host) > 0, length(DomainStr) > 0 ->
    Domain = lists:reverse(string:tokens(DomainStr, ".")),
    [Hostname|_Port] = string:tokens(Host, ":"),
    Hostname1 = lists:reverse(string:tokens(Hostname, ".")),
    case lists:prefix(Domain, Hostname1) of
    true ->
        Rest = lists:nthtail(length(Domain), Hostname1),
        string:join(lists:reverse(Rest), ".");
    _ ->
        undefined
    end;
customer_from_host_header(_Host, _DomainStr) ->
    undefined.
