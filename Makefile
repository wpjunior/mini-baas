.PHONY: test

RELX ?= $(CURDIR)/relx
RELX_CONFIG ?= $(CURDIR)/relx.config
BUILD_BIN = ./_rel/mini_baas_release/bin/mini_baas_release
export RELX

run: build relx-rel
	$(BUILD_BIN) foreground

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
