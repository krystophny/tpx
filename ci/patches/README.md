# Lazarus 4.8 OpenDocument repair

`lazarus-4.8-opendocument.patch` fixes a dependency defect: Unix `OpenDocument`
passed shell quote characters to the desktop opener as part of the document
path. The repair creates the process with an argument vector so paths with
spaces and quote characters arrive unchanged. TpX's `default-opener` native
scenario checks the received argument independently.

The patch comes from the owning Lazarus repository, commit
`24da86bf6a04b56ab7832da067ba1232ea5c3bb1`, based on the dereferenced
`lazarus_4_8` tag commit `62c14a4d18c81f222127d42ce9b89b922c63fcbf`.
Its SHA-256 is
`e803579552bf5d22fb3f51782e3a1bad5482c7e17453c936f7053ef54528f452`.
The owner regression runner checks the received path byte for byte.
The repair is retained locally and has not yet been merged upstream.
This versioned bootstrap patch is required until an upstream release contains
the repair; update the pinned Lazarus revision and remove the patch together
after the same behavioral test passes without it. Windows uses the original
upstream implementation because this defect affects Unix only.

# Lazarus 4.8 Cocoa text-shortcut repair

`lazarus-4.8-cocoa-text-shortcuts.patch` restricts Cocoa's undo/redo branch to
the `z` and `Z` key cases. On the pinned stock provider, the native regression
showed Shift+Command+S incorrectly consumed the pending text redo action. The
patched regression confirms Shift+Command+S stays unhandled and preserves redo,
while Command+Z undo, Shift+Command+Z redo, Command+A selection, and their Caps
Lock controls pass. The macOS candidate job runs
`test/lcltests/testcocoatextshortcuts.sh` from the patched Lazarus tree.

The patch comes from the owning Lazarus repository, commit
`c155ae4c8be30bec073a69df38197176ea2c40d0`, based on the pinned Lazarus 4.8
commit `62c14a4d18c81f222127d42ce9b89b922c63fcbf`. Its SHA-256 is
`0bff9606d28894c667706ddff1c8e2e78293d75f7ad292429bb5e20deb13216f`. The
owner regression runner fails on the stock base and passes on the repaired
owner commit. The repair is not yet merged upstream. This versioned patch is applied by the Unix bootstrap until an
upstream Lazarus revision contains the repair and passes the same native
regression.
