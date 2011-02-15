-module(cloudant_util).
-export([customer_name/1, customer_db_info/2]).

customer_db_info(Req, Info) ->
    FullName = couch_util:get_value(db_name, Info),
    Customer = customer_name(Req),
    DbName = re:replace(FullName, [$^,Customer,$/], "", [{return,binary}]),
    lists:keyreplace(db_name, 1, Info, {db_name,DbName}).

customer_name(Req) ->
    case chttpd:header_value(Req, "X-Cloudant-User") of
    undefined ->
        customer_from_host_header(chttpd:header_value(Req, "Host"));
    CloudantUser ->
        lists:takewhile(fun (C) -> C =/= $, end, CloudantUser)
    end.

%% internal

customer_from_host_header(undefined) ->
    "";
customer_from_host_header("") ->
    "";
customer_from_host_header(Host) ->
    [Hostname|_Port] = string:tokens(Host, ":"),
    case lists:reverse(string:tokens(Hostname, ".")) of
    ["com", "cloudant", "us-east-1a" | Rest] ->
        string:join(lists:reverse(Rest), ".");
    ["com", "cloudant", "us-east-1b" | Rest] ->
        string:join(lists:reverse(Rest), ".");
    ["com", "cloudant", "seti" | Rest] ->
        string:join(lists:reverse(Rest), ".");
    ["com", "cloudant", "cloudenvi" | Rest] ->
        string:join(lists:reverse(Rest), ".");
    ["com", "cloudant" | Rest] ->
        string:join(lists:reverse(Rest), ".");
    _ ->
        ""
    end.
