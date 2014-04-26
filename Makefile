PROJECT = facebook_listener

CT_SUITES = eunit #http

DEPS = cowboy jsx
dep_cowboy = pkg://cowboy master
dep_jsx = pkg://jsx master

TEST_DEPS = ct_helper
dep_ct_helper = https://github.com/extend/ct_helper.git master

# Standard targets.
include erlang.mk

