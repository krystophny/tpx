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

Cocoa composition handling lives in TpX's custom edit and memo controls
(`src/CompositionAwareControls.pas` and `src/CocoaComposition.pas`). Their
native subclasses observe marked-text start, insertion and cancellation only
for TpX-owned editors, retaining LCL's native text and undo handling. They add
no native instance fields; attachment checks the original class and instance
size. Detachment restores that class while preserving any observer subclass
installed by AppKit. Shared field-editor bindings follow their owner.
Deferred completion is canceled on restart or destruction.
No Cocoa composition forwarding patch is applied to Lazarus.

The native inspector regression exercises both memo and field-editor
cancellation without a text-change notification. These tests invoke Cocoa's
text-input APIs; they do not claim a physical-device IME session.

`lazarus-4.8-cocoa-focus-handoff.patch` prevents an older queued Cocoa first-
responder request from overriding a newer focus choice, and cancels pending
requests when the window closes. This Lazarus 4.8 backport is commit
`75bb9453a768a79c4a998fb3ed6b488d6f87451d` on the pinned
`lazarus_4_8` commit `62c14a4d18c81f222127d42ce9b89b922c63fcbf`; it carries
the behavior of merged owning fix `7756423568b59f61be14acfac40c8e6f8f9dd687`.
Its SHA-256 is
`ec4daefa0ef36fda258086e246ef5e350be98941a0ecca626709b840b2fb0f75`. Native
regressions check deferred focus, newer-request supersession, and cancellation
when the target is destroyed before delivery.
