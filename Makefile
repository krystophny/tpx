LAZBUILD ?= lazbuild
WIDGETSET ?= gtk2
LAZARUS_DIR ?= $(patsubst %/lcl,%,$(firstword $(wildcard /usr/lib/lazarus/lcl /usr/lib/lazarus/*/lcl /usr/share/lazarus/lcl /usr/share/lazarus/*/lcl)))
LAZARUS_CONFIG ?= $(CURDIR)/.lazarus
LAZBUILD_FLAGS ?=
FPC ?= fpc
TPX_BINARY = obj/$(shell $(FPC) -iTP)-$(shell $(FPC) -iTO)/TpX
RUNTIME_BINARY = obj/$(shell $(FPC) -iTP)-$(shell $(FPC) -iTO)/RuntimeTests
BUILD_OPTIONS = --pcp="$(LAZARUS_CONFIG)" $(if $(LAZARUS_DIR),--lazarusdir="$(LAZARUS_DIR)") --ws="$(WIDGETSET)" $(LAZBUILD_FLAGS)

.PHONY: all build run test

all: build

build:
	$(LAZBUILD) $(BUILD_OPTIONS) TpX.lpi

run: build
	"$(TPX_BINARY)"

test: build
	TPX_BINARY="$(CURDIR)/$(TPX_BINARY)" python3 tests/test_exports.py
	$(LAZBUILD) $(BUILD_OPTIONS) tests/RuntimeTests.lpi
	RUNTIME_BINARY="$(CURDIR)/$(RUNTIME_BINARY)" python3 tests/test_runtime.py
