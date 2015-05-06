.PHONY: test

RELX ?= $(CURDIR)/relx
RELX_CONFIG ?= $(CURDIR)/relx.config
BUILD_BIN = ./_rel/mini_baas_release/bin/mini_baas_release
export RELX

run-dev:
	erl -pa ebin/ -pa deps/*/ebin -eval "application:ensure_all_started(mini_baas)" -eval "sync:go()"

run: build relx-rel
	$(BUILD_BIN) foreground

shell-dev:
	erl -pa ebin/ -pa deps/*/ebin

build:
	./rebar co

setup:
	./rebar g-d
	make build

test:
	./rebar eu

.virtualenv:
	@virtualenv .virtualenv
	@. .virtualenv/bin/activate && pip install -r acceptance/requirements.txt

test_acceptance: .virtualenv
	./.virtualenv/bin/nosetests -s acceptance

relx-rel: $(RELX)
	@$(RELX) -c $(RELX_CONFIG) $(RELX_OPTS)
