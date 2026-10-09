#!/bin/sh
# Build the browser (WASI) target of TpX: ppcrosswasm32 + the CustomDrawn/WASM
# widgetset from the Lazarus fork, then the pas2js browser host.
#
#   web/build.sh            compile everything into web/dist
#   web/build.sh check      only report whether the toolchain is present
#
# Toolchain locations and pinned revisions live in web/pins.env.
set -eu
ROOT=$(cd "$(dirname "$0")/.." && pwd)
. "$ROOT/web/pins.env"

BUILD=$ROOT/web/build
DIST=$ROOT/web/dist
BIN=$ROOT/obj/wasm32-wasip1/TpXWasm
LRS=$BUILD/lrs

need() { [ -e "$2" ] || { echo "missing $1: $2" >&2; echo "see web/README.md (or ci/web-toolchain.sh) to build it" >&2; exit 2; }; }

check() {
  need ppcrosswasm32 "$PPCWASM32"
  need fpcres "$FPCRES"
  need "Lazarus fork ($WASM_LCL_REV)" "$WASM_LCL/lcl"
  need "FPC wasm32-wasip1 RTL" "$WASM_FPC/rtl/units/wasm32-wasip1"
  need pas2js "$PAS2JS"
  need lazres "$LAZRES"
  need esbuild "$ESBUILD"
  echo "toolchain ok"
}

if [ "${1:-}" = check ]; then check; exit 0; fi

mkdir -p "$BUILD/units" "$DIST" "$LRS" "$(dirname "$BIN")"

# The browser program is generated from the desktop TpX.lpr so that the unit
# list can never drift apart: `library` instead of `program`, source and
# resource paths anchored at the repository root, and no native TpX.res.
sed -e '1s/^program TpX;/library TpXWasm;/' -e 's/\\/\//g' -e '/TpX\.res/d' \
    -e "s#'src/#'$ROOT/src/#g" -e "s#R src/#R $ROOT/src/#g" "$ROOT/TpX.lpr" > "$BUILD/TpXWasm.lpr"
grep -q '^library TpXWasm;' "$BUILD/TpXWasm.lpr"

# Form resources. lazres produces the same .lrs bytes that lazbuild produces for
# the desktop build; those bytes are the oracle for this step.
for unit in MainUnit Propert Table; do
  "$LAZRES" "$LRS/$unit.lrs" "$ROOT/src/$unit.lfm" >/dev/null
done

CFG=$BUILD/compiler.cfg
{
  printf '%s\n' -n -Twasip1 -Pwasm32 -O2 -Mdelphi \
    -dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL \
    -dLCL -dLCLcustomdrawn -dCPUWASM32 \
    "-Fi$ROOT" "-Fu$ROOT" "-Fisrc" "-Fusrc" "-Fusrc/lib/PowerPdf" "-Fusrc/lib/XML" \
    "-FU$BUILD/units" "-FE$(dirname "$BIN")" "-Fi$LRS" \
    "-Fi$WASM_LCL/lcl/include" "-Fi$WASM_LCL/lcl/interfaces/customdrawn" \
    "-Fu$WASM_FPC/rtl/units/wasm32-wasip1"
  for pkg in "$WASM_FPC"/packages/*/units/wasm32-wasip1; do
    [ -d "$pkg" ] && printf '%s\n' "-Fu$pkg"
  done
  printf '%s\n' "-Fu$WASM_LCL/lcl" "-Fu$WASM_LCL/lcl/widgetset" "-Fu$WASM_LCL/lcl/forms" \
    "-Fu$WASM_LCL/lcl/interfaces/customdrawn" "-Fu$WASM_LCL/lcl/nonwin32" \
    "-Fu$WASM_LCL/components/lazutils" "-Fu$WASM_LCL/components/printers" \
    "-Fi$WASM_LCL/components/printers/wasi"
} > "$CFG"

PATH="$(dirname "$FPCRES"):$PATH" "$PPCWASM32" "@$CFG" "$BUILD/TpXWasm.lpr" > "$BUILD/compile.log" 2>&1 || {
  grep -E 'Error|Fatal' "$BUILD/compile.log" | head -30 >&2; exit 1; }
cp "$BIN" "$DIST/tpx.wasm"

# Browser host: the JOB bridge is pas2js, the host loop is plain JavaScript.
PASFLAGS="-n -Jc -Jirtl.js -Fu$WASM_FPC/utils/pas2js/dist"
for pkg in "$WASM_PAS2JS"/packages/*/src; do
  [ -d "$pkg" ] && PASFLAGS="$PASFLAGS -Fu$pkg"
done
# shellcheck disable=SC2086
"$PAS2JS" $PASFLAGS "$ROOT/web/jobhost.lpr" -o"$DIST/jobhost.js" > "$BUILD/pas2js.log" 2>&1 || {
  cat "$BUILD/pas2js.log" >&2; exit 1; }
"$ESBUILD" "$DIST/jobhost.js" --minify --outfile="$DIST/jobhost.js" --allow-overwrite --log-level=warning
cp "$ROOT/web/index.html" "$ROOT/web/host.js" "$DIST/"
"$ESBUILD" "$DIST/host.js" --bundle --format=esm --outfile="$DIST/host.bundle.js" \
  --alias:@lcl/browser-host="$WASM_LCL/examples/customdrawnwasm/web/lcl-host.js" \
  --alias:@bjorn3/browser_wasi_shim="$(dirname "$ESBUILD")/../@bjorn3/browser_wasi_shim" \
  --log-level=warning

printf 'web ready: %s (%s bytes) -> %s\n' "$BIN" "$(wc -c < "$BIN")" "$DIST"
printf 'serve it with:  make web-serve\n'
