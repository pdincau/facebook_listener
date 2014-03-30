-module(eunit_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).

%% Tests.
-export([eunit/1]).

%% ct.

all() ->
    [eunit].

eunit(_) ->
    ok = eunit:test({application, facebook_listener}).
