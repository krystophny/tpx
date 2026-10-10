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

# Path is a desktop-entry string value. Escape sed replacement metacharacters
# so package directories containing spaces or punctuation remain valid.
escaped_package_dir=$(printf '%s' "$package_dir" | sed 's/[\\&|]/\\&/g')
sed "s|@TPX_PACKAGE_DIR@|$escaped_package_dir|g" \
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
