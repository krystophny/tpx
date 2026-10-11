#!/bin/sh
set -eu

package_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
data_home=${XDG_DATA_HOME:-"$HOME/.local/share"}
case "$data_home" in
  /*) ;;
  *) echo "XDG_DATA_HOME must be an absolute path" >&2; exit 1 ;;
esac

applications_dir=$data_home/applications
mime_packages_dir=$data_home/mime/packages
icons_dir=$data_home/icons/hicolor/scalable/apps
mkdir -p "$applications_dir" "$mime_packages_dir" "$icons_dir"

# Desktop Exec uses its own quoting and field-code syntax, not shell quoting.
case "$package_dir" in
  *'
'*) echo "Package path cannot contain a newline" >&2; exit 1 ;;
esac
desktop_exec_path=$(printf '%s' "$package_dir/TpX" | sed \
  -e 's/\\/\\\\/g' -e 's/"/\\"/g' \
  -e 's/\$/\\$/g' -e 's/`/\\`/g' -e 's/%/%%/g')
desktop_exec_path=$(printf '%s' "$desktop_exec_path" | sed 's/\\/\\\\/g')
case "$package_dir" in
  *%*) desktop_exec_path="\"/usr/bin/env\" \"$desktop_exec_path\"" ;;
  *) desktop_exec_path="\"$desktop_exec_path\"" ;;
esac
desktop_string_path=$(printf '%s' "$package_dir" | sed 's/\\/\\\\/g')

# Escape sed replacement metacharacters after desktop-entry encoding.
escaped_exec_path=$(printf '%s' "$desktop_exec_path" | sed 's/[\\&|]/\\&/g')
escaped_string_path=$(printf '%s' "$desktop_string_path" | sed 's/[\\&|]/\\&/g')
sed -e "s|@TPX_PACKAGE_DIR@|$escaped_string_path|g" \
  -e "s|@TPX_EXECUTABLE@|$escaped_exec_path|g" \
  "$package_dir/org.tpx.TpX.desktop.in" \
  > "$applications_dir/org.tpx.TpX.desktop"
cp "$package_dir/org.tpx.TpX.xml" "$mime_packages_dir/org.tpx.TpX.xml"
cp "$package_dir/tpx.svg" "$icons_dir/org.tpx.TpX.svg"

if command -v update-mime-database >/dev/null 2>&1; then
  update-mime-database "$data_home/mime"
else
  echo "Install shared-mime-info to register application/x-tpx" >&2
fi
if command -v xdg-mime >/dev/null 2>&1; then
  xdg-mime default org.tpx.TpX.desktop application/x-tpx
else
  echo "Run xdg-mime default org.tpx.TpX.desktop application/x-tpx to set the default opener" >&2
fi
echo "Registered TpX for native .tpx drawings in $data_home"
