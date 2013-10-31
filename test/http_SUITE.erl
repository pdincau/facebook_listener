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
-export([callback_with_expected_params/1,
         callback_with_missing_params/1]).

%% ct.
all() ->
    [{group, http}].

groups() ->
    Tests = [callback_with_expected_params, callback_with_missing_params],
    [{http, [parallel], Tests}].

init_per_suite(Config) ->
    application:start(crypto),
    application:start(cowlib),
    application:start(ranch),
    application:start(cowboy),
    application:start(inets),
    [].

end_per_suite(_Config) ->
    application:stop(inets),
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(cowlib),
    application:stop(crypto),
    ok.

init_per_group(http, Config) ->
    Transport = ranch_tcp,
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
            {env, [{dispatch, init_dispatch(Config)}]},
            {max_keepalive, 50},
            {timeout, 500}]),
    Port = ranch:get_port(http),
    [Config].

end_per_group(http, _Config) ->
    ok.

%% Dispatch configuration.
init_dispatch(Config) ->
    cowboy_router:compile([{"localhost", [{"/", toppage_handler, []}]}]).

callback_with_missing_params(_Config) ->
    {ok, {{_, 404, _}, _, _}} = httpc:request(get, {"http://localhost:8080", []}, [], []),
    ok.

callback_with_expected_params(_Config) ->
    {ok, {{_, 200, _}, _, "mychallenge"}} = httpc:request(get, {"http://localhost:8080/?hub.mode=subscribe&hub.verify_token=token&hub.challenge=mychallenge", []}, [], []),
    ok.
