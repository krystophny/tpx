# File change notifications

Desktop source watching is event-driven. The Windows backend watches each
source's parent directory with overlapped `ReadDirectoryChangesW` requests and
re-arms the request after every completion. It watches file-name, attribute,
size and last-write changes so atomic replacement, rename, deletion and
recreation of a document remain observable. If the operating system reports a
lost or malformed notification batch, the backend requests a source rescan.
It does not fall back to periodic polling.

The Windows integration job runs on a local Windows Server 2025 NTFS workspace.
That verifies the native backend on NTFS; it does not establish equivalent
behavior on SMB or other network filesystems. Remote filesystem providers can
coalesce, delay or lose notifications, and notification-buffer capacity and
semantics depend on the provider. A failed or unsupported watch is reported as
degraded capability so editing can continue without implying that external
changes are being tracked reliably.
