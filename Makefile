PROJECT = facebook_listener

CT_SUITES = eunit http

DEPS = cowboy jsx lager
dep_cowboy = pkg://cowboy master
dep_jsx = pkg://jsx master
dep_lager = https://github.com/basho/lager.git master

# Standard targets.
#include erlang.mk

release: clean-release all
	relx

clean-release:
	rm -rf _rel

# Extra targets.
autobahn: clean clean-deps deps app build-tests
	@mkdir -p logs/
	@$(CT_RUN) -suite autobahn_SUITE

include erlang.mk
