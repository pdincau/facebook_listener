PROJECT = facebook_listener

DEPS = cowboy jsx lager
dep_cowboy = pkg://cowboy master
dep_jsx = pkg://jsx master
dep_lager = https://github.com/basho/lager.git master

.PHONY: release clean-release

release: clean-release all
	relx

clean-release:
	rm -rf _rel

include erlang.mk
