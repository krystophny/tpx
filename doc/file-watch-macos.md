# macOS file-change source

TpX uses a small Free Pascal binding to the native `kqueue`/`EVFILT_VNODE`
interface. The watcher owns a worker thread and waits indefinitely for vnode
events. An `EVFILT_USER` event wakes it for shutdown; it does not poll paths or
use a repeating timer. The worker publishes immutable records through the
shared bounded file-watch queue and never calls Cocoa or LCL drawing code.

Each subscription watches the named file and its lexical parent directory.
The file watch reports in-place writes and metadata changes; the directory
watch lets the source reopen and re-arm after atomic replacement, deletion,
recreation, or rename. A canonical target-directory watch is added when the
source is a symlink whose target lives in a different directory. The reported
path remains the absolute lexical path supplied to the shared API. Symlink
aliases are followed by this backend, and native tests exercise an alias with a
target in a separate directory. Callers can reject final-component symlinks
before subscribing if their document policy requires that.
Parent-directory symlinks are traversed by the native directory open; tests
confirm that a stable parent alias keeps its lexical event path through edits
and replacement. If an ancestor symlink itself is retargeted, callers should
resubscribe to reconcile the path.

The backend retries a changing pathname a bounded number of times while
re-arming. If a parent cannot be reopened, a native watch is lost, or an
operation fails, the source reports a degraded or error status and a backend
error/rescan event; it does not fall back to filesystem polling. The backend
has been behaviorally tested on the native macOS CI filesystem with in-place
writes, same-size writes with restored modification time, repeated atomic
replacements, deletion/recreation, rename-away/back, alias-target replacement,
Unicode and spaced names, and case-sensitive or case-insensitive filename
behavior according to the volume. kqueue is a notification hint, not a
cross-filesystem completeness guarantee. Network, cloud-synchronized, and
other unusual filesystems may provide weaker notifications; users can reload
manually when watching is unavailable or degraded.

Run the headless native backend tests with:

```sh
python3 tests/test_filewatch_macos.py
```

The test starts a separate Pascal watcher process and performs mutations from
an independent helper process. It does not require a logged-in GUI session.
