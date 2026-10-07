LAZBUILD ?= lazbuild
WIDGETSET ?= gtk2
LAZARUS_DIR ?= $(patsubst %/lcl,%,$(firstword $(wildcard /usr/lib/lazarus/lcl /usr/lib/lazarus/*/lcl /usr/share/lazarus/lcl /usr/share/lazarus/*/lcl)))
LAZARUS_CONFIG ?= $(CURDIR)/.lazarus
LAZBUILD_FLAGS ?=
FPC ?= fpc
TPX_BINARY = obj/$(shell $(FPC) -iTP)-$(shell $(FPC) -iTO)/TpX

.PHONY: all build run

all: build

build:
	$(LAZBUILD) --pcp="$(LAZARUS_CONFIG)" $(if $(LAZARUS_DIR),--lazarusdir="$(LAZARUS_DIR)") --ws="$(WIDGETSET)" $(LAZBUILD_FLAGS) TpX.lpi

run: build
	"$(TPX_BINARY)"
