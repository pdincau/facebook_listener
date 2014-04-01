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
         callback_with_missing_params/1,
         update_without_body/1,
         update_with_valid_signature/1,
         update_with_invalid_signature/1]).

-define(BASE_URL, "http://localhost:8080").
-define(SIGNATURE, "sha1=534985d2be5f2df69cae7cc5e23be204add4f499"). % for key "test"
-define(JSON_UPDATE, "{\"object\":\"user\",\"entry\":[{\"uid\":1335845740,\"changed_fields\":[\"name\",\"picture\"],\"time\":232323},{\"uid\":1234,\"changed_fields\":[\"friends\"],\"time\":232325}]}").
-define(CALLBACK_PARAMS, "?hub.mode=subscribe&hub.verify_token=token&hub.challenge=mychallenge").

%% ct.
all() ->
    [{group, http}].

groups() ->
    Tests = [callback_with_expected_params,
             callback_with_missing_params,
             update_without_body,
             update_with_valid_signature,
             update_with_invalid_signature],
    [{http, [parallel], Tests}].

init_per_suite(_Config) ->
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
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
            {env, [{dispatch, init_dispatch(Config)}]},
            {max_keepalive, 50},
            {timeout, 500}]),
    [Config].

end_per_group(http, _Config) ->
    ok.

%% Dispatch configuration.
init_dispatch(_Config) ->
    cowboy_router:compile([{"localhost", [{"/", handler, []}]}]).

callback_with_missing_params(_Config) ->
    {ok, {{_, 400, _}, _, _}} = httpc:request(get, {?BASE_URL, []}, [], []),
    ok.

callback_with_expected_params(_Config) ->
    {ok, {{_, 200, _}, _, "mychallenge"}} = httpc:request(get, {?BASE_URL ++ ?CALLBACK_PARAMS, []}, [], []),
    ok.

update_without_body(_Config) ->
    {ok, {{_, 400, _}, _, _}} = httpc:request(post, {?BASE_URL, [], "", ""}, [], []),
    ok.

update_with_valid_signature(_Config) ->
    {ok, {{_, 200, _}, _, _}} = httpc:request(post, {?BASE_URL, [{"x-hub-signature", ?SIGNATURE}], "application/json", ?JSON_UPDATE}, [], []),
    ok.

update_with_invalid_signature(_Config) ->
    {ok, {{_, 400, _}, _, _}} = httpc:request(post, {?BASE_URL, [{"x-hub-signature", "invalid"}], "application/json", ?JSON_UPDATE}, [], []),
    ok.
