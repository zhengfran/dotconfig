#!/usr/bin/env python3
"""Give every pane in a tab an equal share along each split axis.

Runs as a herdr plugin event hook (pane.created/closed/moved, startup) or
action. Each split's ratio
is set to weight(first) / (weight(first) + weight(second)), where weight counts
the panes a subtree lines up along that split's axis: same-direction splits add
up, perpendicular splits take the larger side. Three side-by-side columns end
up at 1/3 each, not 1/2 + 1/4 + 1/4.

pane.closed only reports the pane and workspace, and the pane is already gone
by then, so every run caches pane -> tab from session.snapshot and the close
hook looks the tab up there.
"""
import json
import os
import socket
import sys


def call(method, params):
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
        s.connect(os.environ["HERDR_SOCKET_PATH"])
        s.sendall((json.dumps({"id": "balance", "method": method, "params": params}) + "\n").encode())
        buf = b""
        while not buf.endswith(b"\n"):
            chunk = s.recv(65536)
            if not chunk:
                break
            buf += chunk
    resp = json.loads(buf)
    if "error" in resp:
        raise SystemExit(f"{method}: {resp['error']}")
    return resp["result"]


def weight(node, direction):
    if node["type"] != "split":
        return 1
    a, b = weight(node["first"], direction), weight(node["second"], direction)
    return a + b if node["direction"] == direction else max(a, b)


def walk(node, path):
    if node["type"] != "split":
        return
    d = node["direction"]
    a, b = weight(node["first"], d), weight(node["second"], d)
    yield path, a / (a + b), node["ratio"]
    yield from walk(node["first"], path + [False])
    yield from walk(node["second"], path + [True])


def cache_path():
    return os.path.join(os.environ.get("HERDR_PLUGIN_STATE_DIR", "/tmp"), "pane-tabs.json")


def load_cache():
    try:
        with open(cache_path()) as f:
            return json.load(f)
    except (OSError, ValueError):
        return {}


def save_cache():
    snap = call("session.snapshot", {})
    snap = snap.get("snapshot", snap)
    mapping = {p["pane_id"]: p["tab_id"] for p in snap["panes"]}
    tmp = f"{cache_path()}.{os.getpid()}"
    with open(tmp, "w") as f:
        json.dump(mapping, f)
    os.replace(tmp, cache_path())


def target():
    """Return layout.export params for the tab to balance, or None to skip."""
    name = os.environ.get("HERDR_PLUGIN_EVENT")
    data = json.loads(os.environ.get("HERDR_PLUGIN_EVENT_JSON") or "{}").get("data", {})
    pane = data.get("pane_id") or (data.get("pane") or {}).get("pane_id")
    if name == "pane.closed":
        tab = load_cache().get(pane)
        return {"tab_id": tab} if tab else None
    if name == "pane.created" and pane:
        return {"pane_id": pane}
    if name is None:  # manual action: the focused tab
        return {}
    return None


def main():
    params = target()
    save_cache()
    if params is None:
        return
    try:
        layout = call("layout.export", params)
    except SystemExit:  # tab closed along with its last pane
        return
    if "layout" in layout:
        layout = layout["layout"]
    tab_id = layout["tab_id"]
    for path, ratio, current in walk(layout["root"], []):
        if abs(ratio - current) > 1e-3:
            call("layout.set_split_ratio", {"tab_id": tab_id, "path": path, "ratio": ratio})


if __name__ == "__main__":
    sys.exit(main())
