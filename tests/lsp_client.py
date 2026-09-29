"""Minimal Language Server Protocol client for testing the R language server.

It speaks to the server over stdin/stdout the way Claude Code does: send
initialize/initialized, open files, collect published diagnostics, and make
requests such as hover or references.
"""

import json
import os
import subprocess
import threading
import time


class LspClient:
    def __init__(self, command, root, cwd=None, env=None, stderr_path=os.devnull):
        self.root = os.path.abspath(root)
        self._stderr = open(stderr_path, "w")
        self.proc = subprocess.Popen(
            command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=self._stderr, cwd=cwd or self.root, env=env,
        )
        self._lock = threading.Lock()
        self._responses = {}
        self.diagnostics = {}   # uri -> latest diagnostics list
        self._diag_times = {}   # uri -> time of latest publish
        self._next_id = 0
        threading.Thread(target=self._read, daemon=True).start()

    # --- transport ---------------------------------------------------------
    def _read(self):
        out = self.proc.stdout
        while True:
            headers = {}
            while True:
                line = out.readline()
                if not line:
                    return
                line = line.decode().strip()
                if not line:
                    break
                key, value = line.split(":", 1)
                headers[key.lower()] = value.strip()
            msg = json.loads(out.read(int(headers["content-length"])))
            if "method" in msg and "id" in msg:
                # Server-to-client request: answer so the server is not blocked.
                result = None
                if msg["method"] == "workspace/configuration":
                    result = [None] * len(msg["params"]["items"])
                self._send({"jsonrpc": "2.0", "id": msg["id"], "result": result})
            elif msg.get("method") == "textDocument/publishDiagnostics":
                with self._lock:
                    uri = msg["params"]["uri"]
                    self.diagnostics[uri] = msg["params"]["diagnostics"]
                    self._diag_times[uri] = time.time()
            elif "id" in msg:
                with self._lock:
                    self._responses[msg["id"]] = msg

    def _send(self, msg):
        body = json.dumps(msg).encode()
        self.proc.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        self.proc.stdin.flush()

    def request(self, method, params, timeout=60):
        self._next_id += 1
        rid = self._next_id
        self._send({"jsonrpc": "2.0", "id": rid, "method": method, "params": params})
        end = time.time() + timeout
        while time.time() < end:
            with self._lock:
                if rid in self._responses:
                    return self._responses.pop(rid)
            time.sleep(0.05)
        raise TimeoutError(f"{method} timed out after {timeout}s")

    def notify(self, method, params):
        self._send({"jsonrpc": "2.0", "method": method, "params": params})

    # --- protocol helpers ----------------------------------------------------
    def uri(self, rel):
        return "file://" + os.path.join(self.root, rel)

    def initialize(self, timeout=120):
        root_uri = "file://" + self.root
        self.request("initialize", {
            "processId": os.getpid(), "rootUri": root_uri, "rootPath": self.root,
            "workspaceFolders": [{"uri": root_uri, "name": os.path.basename(self.root)}],
            "capabilities": {
                "textDocument": {"hover": {"contentFormat": ["markdown", "plaintext"]},
                                 "publishDiagnostics": {}},
                "workspace": {"workspaceFolders": True},
            },
        }, timeout=timeout)
        self.notify("initialized", {})

    def open(self, rel):
        with open(os.path.join(self.root, rel)) as f:
            text = f.read()
        self.notify("textDocument/didOpen", {"textDocument": {
            "uri": self.uri(rel), "languageId": "r", "version": 1, "text": text}})

    def wait_diagnostics(self, rel, settle=4.0, timeout=120):
        """Latest diagnostics for a file, once no new publish arrived for
        `settle` seconds. Returns None if nothing was published."""
        uri = self.uri(rel)
        end = time.time() + timeout
        while time.time() < end:
            with self._lock:
                last = self._diag_times.get(uri)
            if last is not None and time.time() - last >= settle:
                break
            time.sleep(0.1)
        with self._lock:
            return self.diagnostics.get(uri)

    def hover(self, rel, line, char):
        res = self.request("textDocument/hover", {
            "textDocument": {"uri": self.uri(rel)},
            "position": {"line": line, "character": char}})
        result = res.get("result") or {}
        contents = result.get("contents", "")
        if isinstance(contents, dict):
            contents = contents.get("value", "")
        return contents if isinstance(contents, str) else json.dumps(contents)

    def close(self):
        try:
            self.request("shutdown", None, timeout=5)
            self.notify("exit", None)
            self.proc.wait(timeout=5)
        except Exception:
            self.proc.kill()
        self._stderr.close()
