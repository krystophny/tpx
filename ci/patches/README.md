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

# Lazarus 4.8 native text-composition forwarding

`lazarus-4.8-gtk2-text-composition.patch` forwards the GTK2 memo's native IM
context lifecycle through `LM_IM_COMPOSITION`. It tracks generations so a
queued END from an abandoned composition cannot release a newer composition.
The patch is the GTK2 provider owner's commit
`b8356ab0fc201b47f4ef95e0f99d128b2bc58635`, tested against the pinned 4.8
source; its SHA-256 is
`db7174e55e69ef88172c4d5bee8a4c9f0654e37ff1e09eb55a6f9139097fbde2`.

`lazarus-4.8-cocoa-text-composition.patch` forwards marked-text start, replace,
commit and cancel/end events from Cocoa text views. A generation-tagged queued
END is delivered after the final text notification and is canceled if a newer
composition starts. The 4.8 backport is owner commit
`937501906f5261c66c0f1462a29f83f9ce730e4d`, derived from the reviewed upstream
provider change `f7a6b515ceaa3c79f8fb4d02743c8c0c7433d5e5`; its SHA-256 is
`b8f71505a24d573795c3baa71dc5b7ecac6e5647e3c7cb0310ab7b5a5a9ac7e1`.

The Unix bootstrap applies both versioned provider patches to Lazarus 4.8, and
candidate/release manifests record both patch hashes. The native Cocoa inspector
scenario drives marked text through the real `NSTextView`, cancels without a
text-change notification, and verifies that the inspector's composition gate
releases. This checks the consumer boundary; it does not claim a manual or
physical-device IME session.
