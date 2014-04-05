-module(eunit_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

%% Tests.
-export([eunit/1]).

%% ct.

all() ->
    [eunit].

init_per_testcase(eunit, _Config) ->
    application:start(facebook_listener),
    [].

end_per_testcase(eunit, _Config) ->
    application:stop(facebook_listener),
    ok.


eunit(_) ->
    ok = eunit:test({application, facebook_listener}).
