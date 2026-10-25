"""Give Linux GUI test drivers a private Xvfb display.

Test drivers launch the real application, which must never appear on the
developer's desktop. On Linux every driver therefore starts its own Xvfb
server, points DISPLAY at it for all child processes and removes the Wayland
socket, so nothing can fall back to the session display. A missing Xvfb is an
error rather than a reason to use the session display.
"""
import atexit
import os
import shutil
import subprocess
import sys

_MARKER = "TPX_PRIVATE_DISPLAY"


def _stop(process):
    process.terminate()
    try:
        process.wait(timeout=5)
    except subprocess.TimeoutExpired:
        process.kill()
        process.wait()


def ensure_private_display():
    if not sys.platform.startswith("linux"):
        return
    # A parent driver already owns a private server for this process tree.
    display = os.environ.get(_MARKER)
    if display and os.environ.get("DISPLAY") == display:
        return
    xvfb = shutil.which("Xvfb")
    if not xvfb:
        raise SystemExit("GUI tests need Xvfb so they never use the session "
                         "display; install xvfb (Debian/Ubuntu) or "
                         "xorg-server-xvfb (Arch).")
    read_fd, write_fd = os.pipe()
    process = subprocess.Popen(
        [xvfb, "-displayfd", str(write_fd), "-screen", "0", "1600x1000x24",
         "-nolisten", "tcp"],
        pass_fds=(write_fd,), stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    os.close(write_fd)
    with os.fdopen(read_fd) as pipe:
        number = pipe.readline().strip()
    if not number.isdigit():
        _stop(process)
        raise SystemExit("Xvfb did not report a display number.")
    atexit.register(_stop, process)
    display = ":" + number
    os.environ["DISPLAY"] = display
    os.environ[_MARKER] = display
    os.environ.pop("WAYLAND_DISPLAY", None)
    os.environ["GDK_BACKEND"] = "x11"
    os.environ["QT_QPA_PLATFORM"] = "xcb"
