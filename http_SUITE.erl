-module(http_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([basic/1,
         echo_body/1]).

%% ct.
all() ->
    [{group, http}].

groups() ->
    Tests = [basic, echo_body],
    [{http, [parallel], Tests}].

init_per_suite(Config) ->
    application:start(crypto),
    application:start(cowlib),
    application:start(ranch),
    application:start(cowboy),
    Dir = ?config(priv_dir, Config) ++ "/static",
    ct_helper:create_static_dir(Dir),
    [{static_dir, Dir}|Config].

end_per_suite(Config) ->
    Dir = ?config(static_dir, Config),
    ct_helper:delete_static_dir(Dir),
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(cowlib),
    application:stop(crypto),
    ok.

init_per_group(http, Config) ->
    Transport = ranch_tcp,
    {ok, _} = cowboy:start_http(http, 100, [{port, 0}], [
            {env, [{dispatch, init_dispatch(Config)}]},
            {max_keepalive, 50},
            {timeout, 500}]),
    Port = ranch:get_port(http),
    {ok, Client} = cowboy_client:init([]),
    [{scheme, <<"http">>},
        {port, Port}, {opts, []},
        {transport, Transport}, {client, Client}|Config].

end_per_group(http, _Config) ->
    ok.

%% Dispatch configuration.
init_dispatch(Config) ->
    cowboy_router:compile([{"localhost", [{"/", toppage_handler, []}]}]).

basic(_Config) ->
    1 = 1.

echo_body(Config) ->
    Client = ?config(client, Config),
    MTU = ct_helper:get_loopback_mtu(),
    _ = [begin
            Body = list_to_binary(lists:duplicate(Size, $a)),
            {ok, Client2} = cowboy_client:request(<<"PUT">>,
                build_url("/", Config),
                [{<<"connection">>, <<"close">>}],
                Body, Client),
            {ok, 200, _, Client3} = cowboy_client:response(Client2),
            {ok, Body, _} = cowboy_client:response_body(Client3)
         end || Size <- lists:seq(MTU - 500, MTU)],
    ok.

build_url(Path, Config) ->
    {scheme, Scheme} = lists:keyfind(scheme, 1, Config),
    {port, Port} = lists:keyfind(port, 1, Config),
    PortBin = list_to_binary(integer_to_list(Port)),
    PathBin = list_to_binary(Path),
    << Scheme/binary, "://localhost:", PortBin/binary, PathBin/binary >>.
