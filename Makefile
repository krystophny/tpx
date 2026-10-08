LAZBUILD ?= lazbuild
WIDGETSET ?= gtk2
LAZARUS_DIR ?= $(patsubst %/lcl,%,$(firstword $(wildcard /usr/lib/lazarus/lcl /usr/lib/lazarus/*/lcl /usr/share/lazarus/lcl /usr/share/lazarus/*/lcl)))
LAZARUS_CONFIG ?= $(CURDIR)/.lazarus
LAZBUILD_FLAGS ?=
FPC ?= fpc
TPX_BINARY = obj/$(shell $(FPC) -iTP)-$(shell $(FPC) -iTO)/TpX
RUNTIME_BINARY = obj/$(shell $(FPC) -iTP)-$(shell $(FPC) -iTO)/RuntimeTests
BUILD_OPTIONS = --pcp="$(LAZARUS_CONFIG)" $(if $(LAZARUS_DIR),--lazarusdir="$(LAZARUS_DIR)") --ws="$(WIDGETSET)" $(LAZBUILD_FLAGS)

WEB_PORT ?= 8791

.PHONY: all build run test web web-check web-serve

all: build

build:
	$(LAZBUILD) $(BUILD_OPTIONS) TpX.lpi

run: build
	"$(TPX_BINARY)"

test: build
	TPX_BINARY="$(CURDIR)/$(TPX_BINARY)" python3 tests/test_exports.py
	TPX_BINARY="$(CURDIR)/$(TPX_BINARY)" python3 tests/test_tex.py
	$(LAZBUILD) $(BUILD_OPTIONS) tests/RuntimeTests.lpi
	RUNTIME_BINARY="$(CURDIR)/$(RUNTIME_BINARY)" python3 tests/test_runtime.py

# Browser (WASI) target. Toolchain pins and locations: web/pins.env, web/README.md.
web:
	web/build.sh

web-check:
	web/build.sh check

web-serve: web
	@echo "serving $(CURDIR)/web/dist on http://127.0.0.1:$(WEB_PORT)/"
	python3 -m http.server "$(WEB_PORT)" --bind 127.0.0.1 --directory "$(CURDIR)/web/dist"
