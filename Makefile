LAZBUILD ?= lazbuild
WIDGETSET ?= gtk2
LAZARUS_DIR ?= $(patsubst %/lcl,%,$(firstword $(wildcard /usr/lib/lazarus/lcl /usr/lib/lazarus/*/lcl /usr/share/lazarus/lcl /usr/share/lazarus/*/lcl)))
LAZARUS_CONFIG ?= $(CURDIR)/.lazarus
LAZBUILD_FLAGS ?=
FPC ?= fpc
TPX_BINARY = obj/$(shell $(FPC) -iTP)-$(shell $(FPC) -iTO)/TpX
RUNTIME_BINARY = obj/$(shell $(FPC) -iTP)-$(shell $(FPC) -iTO)/RuntimeTests
CORE_BUILD_DIR = obj/$(shell $(FPC) -iTP)-$(shell $(FPC) -iTO)/core-tests
BUILD_OPTIONS = --pcp="$(LAZARUS_CONFIG)" $(if $(LAZARUS_DIR),--lazarusdir="$(LAZARUS_DIR)") --ws="$(WIDGETSET)" $(LAZBUILD_FLAGS)

WEB_PORT ?= 8791

.PHONY: all build run test test-core test-gui test-tex test-watch web web-check web-serve

all: build

build:
	$(LAZBUILD) $(BUILD_OPTIONS) TpX.lpi

run: build
	"$(TPX_BINARY)"

test: test-core test-gui test-tex

test-core:
	python3 tests/test_core.py --compiler "$(FPC)" --build-dir "$(CORE_BUILD_DIR)"

test-gui: build
	TPX_BINARY="$(CURDIR)/$(TPX_BINARY)" python3 tests/test_exports.py
	$(LAZBUILD) $(BUILD_OPTIONS) tests/RuntimeTests.lpi
	RUNTIME_BINARY="$(CURDIR)/$(RUNTIME_BINARY)" python3 tests/test_runtime.py

test-tex: build
	python3 tests/check_tex_tools.py
	TPX_BINARY="$(CURDIR)/$(TPX_BINARY)" python3 tests/test_tex.py

# Run the common watcher contract case and the native inotify conformance suite.
test-watch:
	python3 tests/test_core.py --compiler "$(FPC)" --build-dir "$(CORE_BUILD_DIR)" --filter=watch
	python3 tests/test_filewatch_linux.py -v

# Browser (WASI) target. Toolchain pins and locations: web/pins.env, web/README.md.
web:
	web/build.sh

web-check:
	web/build.sh check

web-serve: web
	@echo "serving $(CURDIR)/web/dist on http://127.0.0.1:$(WEB_PORT)/"
	python3 -m http.server "$(WEB_PORT)" --bind 127.0.0.1 --directory "$(CURDIR)/web/dist"
