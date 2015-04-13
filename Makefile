.PHONY: test
PROJECT = mini_baas
DEPS = cowboy eredis mongodb jiffy

dep_jiffy = git https://github.com/davisp/jiffy master
dep_cowboy = git https://github.com/ninenines/cowboy master
dep_eredis = git https://github.com/wooga/eredis master
dep_mongodb = git https://github.com/comtihon/mongodb-erlang master

include erlang.mk

run: all
	./_rel/mini_baas/bin/mini_baas foreground

setup:
	./rebar g-d
	./rebar co

test:
	./rebar eu
