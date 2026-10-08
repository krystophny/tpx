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
