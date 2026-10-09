#!/bin/sh
# Build the browser (WASI) toolchain for TpX from the pins in web/pins.env.
#
#   ci/web-toolchain.sh          build whatever is missing, then report
#   ci/web-toolchain.sh check    only report what is present
#
# This recreates the layout that web/pins.env expects, from the pinned
# revisions, so a fresh machine or CI runner can run `make web`.
#
# It is deliberately long (a full FPC cross-compiler plus the Lazarus fork with
# the CustomDrawn/WASM widgetset) and idempotent: every stage is skipped when
# its output already exists, so caching the three target directories makes a
# second run cheap. Point WASM_LCL / WASM_FPC / WASM_PAS2JS / WASM_BIN at your
# own checkout to reuse work.
#
# Not verified end-to-end in CI yet; the local layout it produces is the one
# web/pins.env documents and `ci/web-toolchain.sh check` is what CI asserts.
set -eu
ROOT=$(cd "$(dirname "$0")/.." && pwd)
. "$ROOT/web/pins.env"

log() { printf 'web-toolchain: %s\n' "$*"; }
have() { [ -e "$1" ]; }

check() {
  rc=0
  for pair in "ppcrosswasm32:$PPCWASM32" "fpcres:$FPCRES" "Lazarus fork lcl:$WASM_LCL/lcl" \
              "FPC wasm32-wasip1 RTL:$WASM_FPC/rtl/units/wasm32-wasip1" "pas2js:$PAS2JS" \
              "esbuild:$ESBUILD"; do
    name=${pair%%:*}; path=${pair#*:}
    if have "$path"; then log "ok      $name"
    else log "MISSING $name  ($path)"; rc=1; fi
  done
  return $rc
}

clone_at() { # url rev dir
  url=$1; rev=$2; dir=$3
  if have "$dir/.git"; then
    cd "$dir"
    if [ "$(git rev-parse HEAD)" = "$rev" ]; then log "at pin   $(basename "$dir")"; return 0; fi
    # A branch pin may move; a sha pin must match exactly.
    git fetch --quiet origin "$rev" 2>/dev/null || git fetch --quiet origin
    git checkout --quiet "$rev"
  else
    git clone --quiet "$url" "$dir"
    cd "$dir"
    git checkout --quiet "$rev"
  fi
  git submodule update --init --recursive --quiet || true
  log "checked  $(basename "$dir") @ $(git rev-parse --short HEAD)"
}

build_fpc() {
  # The pinned GitLab source tree is flat: compiler/, rtl/, packages/ at the root.
  cd "$WASM_FPC"
  if have "$WASM_FPC/rtl/units/wasm32-wasip1" && have "$PPCWASM32"; then
    log "ok       wasm32-wasip1 RTL and cross compiler already present"
    return 0
  fi
  # Cross compiler for wasm32-wasip1 only; no native reinstall, no docs.
  sh ./configure --prefix="$WASM_BIN" --target=wasm32-wasip1 --build=x86_64-linux \
    --with-compiler-name=ppcrosswasm32
  make build FPCMAKE= NOPP= LCL= OPT=-O2 -j"$(nproc)"
  make install-compiler install-cross -j"$(nproc)"
  log "built    ppcrosswasm32"
}

build_lazarus_lcl() {
  cd "$WASM_LCL/lcl"
  make clean || true
  make LCL_PLATFORM=wasm32 CPU_TARGET=wasm32 OS_TARGET=wasip1 \
    FPC=$(basename "$PPCWASM32") FPCDIR="$WASM_FPC/fpc" \
    INSTALL_PREFIX="$WASM_BIN" -j"$(nproc)"
  log "built    CustomDrawn/WASM LCL"
}

build_pas2js() {
  cd "$WASM_PAS2JS"
  make clean || true
  make -j"$(nproc)"
  log "built    pas2js"
}

case ${1:-build} in
  check) check ;;
  build)
    if have "$PPCWASM32" && have "$WASM_LCL/lcl/units/wasm32-wasip1" && have "$PAS2JS"; then
      log "toolchain already complete"; exit 0
    fi
    [ -n "${GITHUB_ACTIONS:-}" ] || [ "${ALLOW_LONG_BUILD:-0}" = 1 ] || {
      echo "This builds FPC + Lazarus + pas2js from source (tens of minutes)." >&2
      echo "Re-run with ALLOW_LONG_BUILD=1, or point the pins at an existing toolchain." >&2
      exit 3
    }
    clone_at "$WASM_FPC_URL" "$WASM_FPC_REV" "$WASM_FPC"
    build_fpc
    clone_at "$WASM_LCL_URL" "$WASM_LCL_REV" "$WASM_LCL"
    build_lazarus_lcl
    clone_at "$WASM_PAS2JS_URL" "$WASM_PAS2JS_REV" "$WASM_PAS2JS"
    build_pas2js
    check
    ;;
  *) echo "usage: $0 [build|check]" >&2; exit 64 ;;
esac
