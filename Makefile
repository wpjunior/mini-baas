.PHONY: test
PROJECT = mini_baas
DEPS = cowboy eredis mongodb jiffy uuid

dep_jiffy = git https://github.com/davisp/jiffy master
dep_cowboy = git https://github.com/ninenines/cowboy master
dep_eredis = git https://github.com/wooga/eredis master
dep_mongodb = git https://github.com/comtihon/mongodb-erlang master
dep_uuid = git https://github.com/avtobiff/erlang-uuid master

include erlang.mk

run: all
	./_rel/mini_baas/bin/mini_baas foreground

setup:
	./rebar g-d
	./rebar co

test:
	./rebar eu

.virtualenv:
	@virtualenv .virtualenv
	@. .virtualenv/bin/activate && pip install -r acceptance/requirements.txt

test_acceptance: .virtualenv
	./.virtualenv/bin/nosetests -s acceptance
