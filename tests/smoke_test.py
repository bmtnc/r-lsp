#!/usr/bin/env python3
"""End-to-end smoke test for the r-lsp plugin.

Starts plugins/r-lsp/bin/r-lsp-wrapper on scratch projects, the way Claude
Code does, and checks diagnostics and hover. The wrapper patches internals of
the `languageserver` R package, so this is also the early warning when a new
languageserver release breaks those patches.

Needs R on PATH with lintr. Scenarios that start the language server need
languageserver, the renv scenarios need renv, the tidyverse scenario needs
tidyverse; a scenario whose packages are missing is skipped, not failed,
unless --no-skip makes a skip a failure (CI runs with it, so a package that
fails to install cannot turn the run green).

    python3 tests/smoke_test.py            # run everything
    python3 tests/smoke_test.py -k renv    # scenarios whose name contains "renv"
    python3 tests/smoke_test.py --keep     # keep the scratch dir for debugging
    python3 tests/smoke_test.py --no-skip  # a skipped scenario is a failure
"""

import argparse
import datetime
import json
import os
import shutil
import signal
import subprocess
import sys
import tempfile
import time
import traceback

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from lsp_client import LspClient  # noqa: E402

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
WRAPPER = os.environ.get("R_LSP_WRAPPER") or os.path.join(REPO, "plugins", "r-lsp", "bin", "r-lsp-wrapper")
FIXTURES = os.path.join(REPO, "tests", "fixtures")
STYLE_LINTERS = {
    "assignment_linter", "brace_linter", "infix_spaces_linter", "object_name_linter",
    "line_length_linter", "pipe_consistency_linter", "quotes_linter",
    "indentation_linter", "semicolon_linter", "commas_linter", "whitespace_linter",
}


class Skip(Exception):
    pass


def rscript(code, cwd=None):
    out = subprocess.run(["Rscript", "--vanilla", "-e", code], cwd=cwd,
                         capture_output=True, text=True)
    if out.returncode != 0:
        raise RuntimeError(f"Rscript failed:\n{code}\n{out.stderr}")
    return out.stdout


def require_packages(*pkgs):
    names = ", ".join(f'"{p}"' for p in pkgs)
    found = rscript(f"cat(vapply(c({names}), function(p) nzchar(system.file(package = p)), TRUE))").split()
    missing = [p for p, ok in zip(pkgs, found) if ok != "TRUE"]
    if missing:
        raise Skip("not installed: " + ", ".join(missing))


def install_into(lib, pkg_dir):
    out = subprocess.run(["R", "CMD", "INSTALL", "--no-test-load", "-l", lib, pkg_dir],
                         capture_output=True, text=True)
    if out.returncode != 0:
        raise RuntimeError(f"R CMD INSTALL {pkg_dir} failed:\n{out.stderr}")


def lines_between(path, start_marker, end_marker=None):
    """1-based line numbers after the start marker, up to the end marker."""
    with open(path) as f:
        lines = f.read().splitlines()
    start = next(i for i, l in enumerate(lines) if l.startswith(start_marker)) + 1
    end = len(lines)
    if end_marker:
        end = next(i for i, l in enumerate(lines) if l.startswith(end_marker))
    return set(range(start + 1, end + 1))


def line_of(path, text):
    with open(path) as f:
        for i, line in enumerate(f.read().splitlines(), 1):
            if text in line:
                return i
    raise ValueError(f"{text!r} not in {path}")


def by_line(diags):
    out = {}
    for d in diags or []:
        out.setdefault(d["range"]["start"]["line"] + 1, []).append(d)
    return out


def describe(diags):
    return "\n".join(f"    L{d['range']['start']['line'] + 1} [{d.get('code')}] {d['message'][:100]}"
                     for d in diags or []) or "    (none)"


class Scenario:
    def __init__(self, scratch):
        self.scratch = scratch

    def server(self, root, cwd=None, env=None):
        require_packages("languageserver")
        log = os.path.join(self.scratch, os.path.basename(root) + ".stderr.log")
        full_env = dict(os.environ, **(env or {}))
        client = LspClient([WRAPPER], root, cwd=cwd, env=full_env, stderr_path=log)
        client.initialize()
        return client

    def make_package(self, name, extra_files=0):
        """Copy the pkg2 fixture and add `extra_files` generated R files, each
        calling add_tax(), so that indexing takes long enough to observe."""
        root = os.path.join(self.scratch, name)
        shutil.copytree(os.path.join(FIXTURES, "pkg2"), root)
        for i in range(extra_files):
            with open(os.path.join(root, "R", f"gen_{i:03d}.R"), "w") as f:
                f.write(f"gen_fun_{i} <- function(x) {{\n  y <- add_tax(x) + {i}\n  y * 2\n}}\n")
        return root

    def make_project(self, name, files):
        root = os.path.join(self.scratch, name)
        os.makedirs(root, exist_ok=True)
        # A project marker, so the project scan stays inside this directory.
        open(os.path.join(root, ".here"), "w").close()
        for rel, src in files.items():
            dest = os.path.join(root, rel)
            os.makedirs(os.path.dirname(dest), exist_ok=True)
            shutil.copy(src, dest)
        return root

    def make_renv_project(self, name, files, extra_packages=()):
        require_packages("renv")
        root = self.make_project(name, files)
        rscript("options(renv.consent = TRUE); "
                "renv::init(bare = TRUE, restart = FALSE, load = FALSE)", cwd=root)
        lib = rscript(f'cat(renv::paths$library(project = "{root}"))').strip()
        os.makedirs(lib, exist_ok=True)
        for pkg in ("renvonly",) + tuple(extra_packages):
            install_into(lib, os.path.join(FIXTURES, "packages", pkg))
        return root


def check(cond, message, diags=None):
    if not cond:
        raise AssertionError(message + ("\n  diagnostics:\n" + describe(diags) if diags is not None else ""))


# --- scenarios ----------------------------------------------------------------

def scenario_bugs(s):
    """Planted bugs in plain R: caught, without style noise or column false alarms."""
    require_packages("dplyr")
    src = os.path.join(FIXTURES, "bugs", "bugs.R")
    root = s.make_project("bugs", {"bugs.R": src})
    lsp = s.server(root)
    try:
        lsp.open("bugs.R")
        diags = lsp.wait_diagnostics("bugs.R")
    finally:
        lsp.close()
    check(diags is not None, "no diagnostics published")
    lines = by_line(diags)
    with open(src) as f:
        tagged = f.read().splitlines()
    for i, line in enumerate(tagged, 1):
        if line.lstrip().startswith("#"):
            continue
        if "# bug" in line:
            check(i in lines, f"line {i} not flagged: {line.strip()}", diags)
        if "# ok" in line:
            check(i not in lines, f"line {i} wrongly flagged: {line.strip()}", diags)
    style = [d for d in diags if d.get("code") in STYLE_LINTERS]
    check(not style, "style lints reported", style)


def scenario_tidyverse(s):
    """library(tidyverse) code: correct code is clean, old APIs and bugs flagged."""
    require_packages("tidyverse")
    src = os.path.join(FIXTURES, "tidyverse", "analysis.R")
    root = s.make_project("tidyverse", {"analysis.R": src})
    lsp = s.server(root)
    try:
        lsp.open("analysis.R")
        diags = lsp.wait_diagnostics("analysis.R")
        hover = lsp.hover("analysis.R", line_of(src, "mutate(share") - 1, 6)
    finally:
        lsp.close()
    check(diags is not None, "no diagnostics published")
    lines = by_line(diags)
    clean = lines_between(src, "# == correct", "# == outdated")
    wrong = [d for n in sorted(clean) for d in lines.get(n, [])]
    check(not wrong, "diagnostics on correct tidyverse code", wrong)
    for text in ("funs(", "gather(", "do(head", "top_n(", "cur_data()"):
        n = line_of(src, text)
        check(any(d.get("code") == "lifecycle_linter" for d in lines.get(n, [])),
              f"outdated call not flagged on line {n}: {text}", diags)
    for text in ("filterr(", "summarize_everything(", "undefined_helper("):
        n = line_of(src, text)
        check(n in lines, f"bug not flagged on line {n}: {text}", diags)
    check("dplyr" in hover and "mutate" in hover, f"hover on mutate() failed: {hover[:200]!r}")


def scenario_renv(s):
    """A package only in the renv library: hover works, no false 'undefined'."""
    root = s.make_renv_project("renv_basic", {"R/use.R": os.path.join(FIXTURES, "renv", "use.R")})
    lsp = s.server(root)
    try:
        lsp.open("R/use.R")
        diags = lsp.wait_diagnostics("R/use.R")
        qualified = lsp.hover("R/use.R", 1, 20)
        attached = lsp.hover("R/use.R", 2, 20)
    finally:
        lsp.close()
    check(diags is not None, "no diagnostics published")
    check(diags == [], "renv-only package reported as missing/undefined", diags)
    check("RENVONLY_DOC_MARKER" in qualified, f"hover on renvonly::greet failed: {qualified[:200]!r}")
    check("RENVONLY_DOC_MARKER" in attached, f"hover on attached greet failed: {attached[:200]!r}")


def scenario_renv_old_lintr(s):
    """A project pinning an old lintr in renv must not break diagnostics."""
    root = s.make_renv_project("renv_old_lintr",
                               {"R/use.R": os.path.join(FIXTURES, "renv", "use.R")},
                               extra_packages=("lintr_old",))
    lsp = s.server(root)
    try:
        lsp.open("R/use.R")
        diags = lsp.wait_diagnostics("R/use.R")
    finally:
        lsp.close()
    check(diags is not None, "no diagnostics published")
    failed = [d for d in diags if "Failed to run diagnostics" in d["message"]]
    check(not failed, "diagnostics crashed", diags)
    check(diags == [], "unexpected diagnostics", diags)


def scenario_renv_old_purrr(s):
    """Version checks use the renv-pinned copy, even for packages the server loads."""
    require_packages("purrr")
    src = os.path.join(FIXTURES, "renv", "use_purrr.R")
    root = s.make_renv_project("renv_old_purrr", {"R/use.R": src}, extra_packages=("purrr_old",))
    lsp = s.server(root)
    try:
        lsp.open("R/use.R")
        diags = lsp.wait_diagnostics("R/use.R")
    finally:
        lsp.close()
    check(diags is not None, "no diagnostics published")
    lines = by_line(diags)
    check(line_of(src, "map_dfr(") not in lines,
          "map_dfr() flagged although the pinned purrr 0.3.5 does not supersede it", diags)
    for text in ("stacked <- list_rbind", "purrr::list_rbind", "h <- function(xs) list_rbind"):
        n = line_of(src, text)
        check(any("purrr 0.3.5" in d["message"] for d in lines.get(n, [])),
              f"list_rbind() on line {n} not reported as missing from purrr 0.3.5", diags)


def hook_timeouts():
    """basename -> the timeout hooks.json gives the hook, in seconds."""
    with open(os.path.join(REPO, "plugins", "r-lsp", "hooks", "hooks.json")) as f:
        hooks = json.load(f)["hooks"]
    out = {}
    for entries in hooks.values():
        for entry in entries:
            for h in entry["hooks"]:
                out[os.path.basename(h["command"].strip('"'))] = h.get("timeout", 60)
    return out


HOOK_TIMEOUTS = hook_timeouts()


def run_hook(script, payload, env=None, stdin_text=None, allow_stderr=False):
    """Run a plugin hook/bin script with a JSON payload on stdin; return stdout.
    A hook that exits non-zero, writes to stderr, or runs past the timeout
    hooks.json gives it fails the scenario: a hook that crashes is silent
    too, and silence means checked and clean."""
    full_env = dict(os.environ, **(env or {}))
    name = os.path.basename(script)
    out = subprocess.run([script], input=stdin_text if stdin_text is not None else json.dumps(payload),
                         capture_output=True, text=True, env=full_env, timeout=HOOK_TIMEOUTS.get(name, 60))
    check(out.returncode == 0, f"{name} exited {out.returncode}:\n{out.stderr}")
    if not allow_stderr:
        check(not out.stderr.strip(), f"{name} wrote to stderr:\n{out.stderr}")
    return out.stdout


def hook_context(stdout):
    if not stdout.strip():
        return ""
    return json.loads(stdout)["hookSpecificOutput"]["additionalContext"]


def references(lsp, rel, line, char):
    res = lsp.request("textDocument/references", {
        "textDocument": {"uri": lsp.uri(rel)}, "position": {"line": line, "character": char},
        "context": {"includeDeclaration": True}})
    return [loc["uri"] for loc in (res.get("result") or [])]


def scenario_complete_answers(s):
    """References asked right after opening a file wait for the index and are complete."""
    root = s.make_package("complete_answers", extra_files=80)
    status_file = os.path.join(s.scratch, "complete_answers.status.log")
    lsp = s.server(root, env={"R_LSP_STATUS_FILE": status_file, "CLAUDE_PROJECT_DIR": root})
    try:
        lsp.open("R/tax.R")
        t0 = time.time()
        uris = references(lsp, "R/tax.R", 4, 0)
        elapsed = time.time() - t0
    finally:
        lsp.close()
    files = {u.rsplit("/", 1)[-1] for u in uris}
    generated = {f for f in files if f.startswith("gen_")}
    check(len(generated) == 80, f"references found {len(generated)}/80 generated callers after {elapsed:.1f}s: {sorted(files)[:5]}")
    check("other.R" in files, "references missed R/other.R")
    with open(status_file) as f:
        status = f.read()
    check("indexing" in status and "index ready" in status, f"status events missing:\n{status}")


def scenario_cold_hover(s):
    """Hover and documentSymbol asked right after opening a file wait for the
    parse queue and the index like references do: an empty answer during
    warm-up looked like a real "nothing here"."""
    root = s.make_package("cold_hover", extra_files=80)
    lsp = s.server(root, env={"CLAUDE_PROJECT_DIR": root})
    try:
        lsp.open("R/gen_040.R")
        t0 = time.time()
        hover = lsp.request("textDocument/hover", {
            "textDocument": {"uri": lsp.uri("R/gen_040.R")}, "position": {"line": 1, "character": 8}})
        symbols = lsp.request("textDocument/documentSymbol", {"textDocument": {"uri": lsp.uri("R/gen_040.R")}})
        elapsed = time.time() - t0
    finally:
        lsp.close()
    contents = json.dumps((hover.get("result") or {}).get("contents") or "")
    check("add_tax" in contents, f"cold hover on add_tax() came back empty after {elapsed:.1f}s: {hover.get('result')!r}")
    names = {sym.get("name") for sym in (symbols.get("result") or [])}
    check("gen_fun_40" in names, f"cold documentSymbol came back empty after {elapsed:.1f}s: {sorted(names)}")


def scenario_shared_scope(s):
    """Since languageserver 0.3.19 a script or test is scoped away from the
    package's R/; the plugin restores one shared scope and complete outgoing calls."""
    root = os.path.join(s.scratch, "pkgscope")
    shutil.copytree(os.path.join(FIXTURES, "pkgscope"), root)
    status_file = os.path.join(s.scratch, "pkgscope.status.log")
    lsp = s.server(root, env={"CLAUDE_PROJECT_DIR": root, "R_LSP_STATUS_FILE": status_file})
    try:
        # Callers in a script and a test, asked before anything opens those files.
        lsp.open("R/flags.R")
        items = []
        for _ in range(10):
            items = lsp.request("textDocument/prepareCallHierarchy", {
                "textDocument": {"uri": lsp.uri("R/flags.R")},
                "position": {"line": 3, "character": 1}}).get("result") or []
            if items:
                break
            time.sleep(1)
        incoming = []
        for _ in range(15):
            incoming = lsp.request("callHierarchy/incomingCalls", {"item": items[0]}).get("result") or [] if items else []
            if len({i["from"]["uri"].rsplit("/", 1)[-1] for i in incoming}) >= 3:
                break
            time.sleep(1)
        outgoing = lsp.request("callHierarchy/outgoingCalls", {"item": items[0]}).get("result") or [] if items else []
        ref_locs = lsp.request("textDocument/references", {
            "textDocument": {"uri": lsp.uri("R/flags.R")}, "position": {"line": 3, "character": 1},
            "context": {"includeDeclaration": True}}).get("result") or []
        refs = [loc["uri"] for loc in ref_locs]
        # Hover and definition from the script into R/.
        lsp.open("scripts/run.R")
        hover = ""
        for _ in range(15):
            hover = lsp.hover("scripts/run.R", 2, 12)   # 0-based: line 3 `result <- flag_missing(`
            if hover:
                break
            time.sleep(1)
        defn = lsp.request("textDocument/definition", {
            "textDocument": {"uri": lsp.uri("scripts/run.R")},
            "position": {"line": 2, "character": 12}}).get("result")
    finally:
        lsp.close()
    callers = {i["from"]["uri"].rsplit("/", 1)[-1] for i in incoming}
    check({"run.R", "test-flags.R", "usage.Rmd"} <= callers,
          f"incomingCalls missed callers in scripts/, tests/ or vignettes/ that nothing had opened: {sorted(callers)}")
    ref_files = {u.rsplit("/", 1)[-1] for u in refs}
    check({"run.R", "test-flags.R", "usage.Rmd"} <= ref_files, f"references missed scripts/, tests/ or vignettes/: {sorted(ref_files)}")
    # the vignette's chunk is referenced at its own line; its prose is not
    rmd_lines = {loc["range"]["start"]["line"] + 1 for loc in ref_locs if loc["uri"].endswith("usage.Rmd")}
    check(rmd_lines == {line_of(os.path.join(root, "vignettes", "usage.Rmd"), "flagged <- flag_missing")},
          f"references in the vignette are not the chunk line only: {sorted(rmd_lines)}")
    called = {o["to"]["name"] for o in outgoing}
    check({"is.na", "sum", "as.character"} <= called,
          f"outgoingCalls dropped calls inside `!`, unary minus or a subscript: {sorted(called)}")
    check("flag_missing" in hover, f"hover from a script into R/ is empty (document scope not shared): {hover[:80]!r}")
    check(bool(defn) and "flags.R" in json.dumps(defn), f"definition from a script did not reach R/flags.R: {defn}")
    with open(status_file) as f:
        log = f.read()
    check("loaded 2 test/script files as documents, plus 1 Rmd/qmd" in log,
          "status log has no 'loaded ... test/script files ..., plus 1 Rmd/qmd' event")
    # On a languageserver whose discover() fills the file list later from a
    # worker (git master), the server walks the tree itself with the index's
    # rules; here discover() is synchronous, so the walk must not have run,
    # and run by hand it must list what the index lists (R/ excepted).
    check("walked" not in log, f"the fallback walk ran although the index had its file list:\n{log}")
    server_r = os.path.join(os.path.dirname(WRAPPER), "r-lsp-server.R")
    same = rscript(f"""
        ns <- asNamespace("languageserver")
        exprs <- parse("{server_r}")
        def <- Filter(function(e) is.call(e) && identical(e[[1L]], as.name("<-")) && identical(e[[2L]], as.name("walk_index_files")), as.list(exprs))
        eval(def[[1L]])
        root <- normalizePath("{root}")
        idx <- ns$WorkspaceIndex$new(root)
        walked <- sort(walk_index_files(idx, root))
        idx$discover()
        listed <- sort(unname(vapply(setdiff(idx$files$keys(), idx$package_source_uris()), ns$path_from_uri, "")))
        cat(identical(walked, listed), basename(walked))
    """)
    check(same.split()[0] == "TRUE" and set(same.split()[1:]) == {"run.R", "test-flags.R"},
          f"the fallback walk and the index disagree on the files to load: {same}")


def scenario_stale_files(s):
    """Files changed on disk outside Edit/Write (git checkout, sed, styler, a
    heredoc) are re-read before a cross-file request. Claude Code never sends
    didChangeWatchedFiles and re-opens only the file it asks about, so the
    server answered from the old text of every other file: a caller that no
    longer existed, a deleted vignette, the old signature on hover."""
    root = os.path.join(s.scratch, "stale_files")
    shutil.copytree(os.path.join(FIXTURES, "pkgscope"), root)
    status_file = os.path.join(s.scratch, "stale_files.status.log")
    test_file = os.path.join(root, "tests", "testthat", "test-flags.R")
    flags = os.path.join(root, "R", "flags.R")

    def located(lsp):
        locs = lsp.request("textDocument/references", {
            "textDocument": {"uri": lsp.uri("scripts/run.R")}, "position": {"line": 2, "character": 12},
            "context": {"includeDeclaration": True}}).get("result") or []
        out = {}
        for loc in locs:
            out.setdefault(loc["uri"].rsplit("/", 1)[-1], set()).add(loc["range"]["start"]["line"] + 1)
        return out

    lsp = s.server(root, env={"CLAUDE_PROJECT_DIR": root, "R_LSP_STATUS_FILE": status_file})
    try:
        lsp.open("scripts/run.R")
        before = {}
        for _ in range(15):
            before = located(lsp)
            if {"flags.R", "test-flags.R", "usage.Rmd"} <= set(before):
                break
            time.sleep(1)
        # Changed on disk as a shell would: a second test caller, the
        # vignette gone, a new signature and definition line in R/.
        time.sleep(1.1)   # a filesystem with whole-second mtimes
        with open(test_file, "a") as f:
            f.write("test_that('again', {\n  expect_equal(unname(flag_missing(c(NA, 2))), -2)\n})\n")
        os.remove(os.path.join(root, "vignettes", "usage.Rmd"))
        with open(flags, "w") as f:
            f.write("#' Flag missing values\n#' @param x numeric\n#' @param na_value used when the first value is missing\n#' @export\n"
                    "flag_missing <- function(x, na_value = 0) {\n  if (!is.na(x[1])) y <- sum(x) else y <- na_value - sum(x, na.rm = TRUE)\n"
                    "  names(y) <- as.character(length(x))\n  y\n}\n")
        lsp.open("scripts/run.R")   # the LSP tool re-opens the file it asks about, nothing else
        after = located(lsp)
        hover = lsp.hover("scripts/run.R", 2, 12)
        defn = lsp.request("textDocument/definition", {
            "textDocument": {"uri": lsp.uri("scripts/run.R")}, "position": {"line": 2, "character": 12}}).get("result")
    finally:
        lsp.close()
    check({"flags.R", "test-flags.R", "usage.Rmd"} <= set(before) and before.get("test-flags.R") == {2},
          f"references before the change are not the fixture's: {before}")
    check("usage.Rmd" not in after, f"references still list the deleted vignette: {after}")
    check(after.get("test-flags.R") == {2, 5}, f"references missed the caller appended to the test file on disk: {after}")
    check(after.get("run.R") == {3} and after.get("flags.R") == {5}, f"references after the change unexpected: {after}")
    check("na_value" in hover, f"hover gives the old signature after R/flags.R changed on disk: {hover[:120]!r}")
    defn = defn if isinstance(defn, list) else [defn] if defn else []
    check(any(d.get("uri", "").endswith("flags.R") and d["range"]["start"]["line"] + 1 == 5 for d in defn),
          f"definition did not move to the new line of R/flags.R: {defn}")
    with open(status_file) as f:
        log = f.read()
    check("re-read 2 file(s) changed on disk outside the session, dropped 1 deleted" in log,
          f"status log has no 're-read ... dropped ...' event:\n{log}")


def scenario_other_repository(s):
    """A file opened from another repository gets that repository indexed."""
    other = s.make_package("other_repo")
    root = s.make_project("scripts_root", {"analysis.R": os.path.join(FIXTURES, "bugs", "bugs.R")})
    status_file = os.path.join(s.scratch, "other_repo.status.log")
    lsp = s.server(root, env={"R_LSP_STATUS_FILE": status_file, "CLAUDE_PROJECT_DIR": root})
    try:
        tax = os.path.join(other, "R", "tax.R")
        lsp.open(tax)
        time.sleep(3)
        uris = references(lsp, tax, 4, 0)
        sym = lsp.request("workspace/symbol", {"query": "gross"}).get("result") or []
        # the other repository's R/ is parsed and its tests loaded, as for the session's own
    finally:
        lsp.close()
    files = {u.rsplit("/", 1)[-1] for u in uris}
    check("other.R" in files, f"references in the other repository missed R/other.R: {sorted(files)}")
    check("test-tax.R" in files, f"references in the other repository missed tests/testthat/test-tax.R: {sorted(files)}")
    check(any(x.get("name") == "gross" for x in sym), "workspace symbol did not find gross() in the other repository")
    with open(status_file) as f:
        log = f.read()
    check("added workspace" in log, "status log has no 'added workspace' event")
    check("loaded 1 test/script files as documents" in log, f"the other repository's tests were not loaded:\n{log}")
    hook = os.path.join(REPO, "plugins", "r-lsp", "hooks", "post-lsp.sh")
    payload = {"tool_name": "LSP", "cwd": root,
               "tool_input": {"operation": "findReferences", "filePath": tax, "line": 5, "character": 1},
               "tool_response": "Found 1 references across 1 files:\n\nR/tax.R:\n  Line 5:1"}
    ctx = hook_context(run_hook(hook, payload, env={"CLAUDE_PROJECT_DIR": root}))
    check("R/other.R:1:" in ctx and "tests/testthat/test-tax.R:2:" in ctx,
          f"post-lsp note did not grep the other repository:\n{ctx}")


def scenario_colcheck(s):
    """Column names in tidyverse code are checked against the data the code reads."""
    root = os.path.join(s.scratch, "columns")
    shutil.copytree(os.path.join(FIXTURES, "columns"), root)
    rscript('saveRDS(data.frame(id = 1:3, score = c(1, 2, 3)), "data/scores.rds")', cwd=root)
    tool = os.path.join(REPO, "plugins", "r-lsp", "bin", "r-colcheck")
    hook = os.path.join(REPO, "plugins", "r-lsp", "hooks", "post-edit.sh")
    analysis = os.path.join(root, "R", "analysis.R")
    out = subprocess.run([tool, "--root", root, analysis], capture_output=True, text=True, timeout=120).stdout
    reported = {int(line.split(":")[1]) for line in out.splitlines()
                if line.startswith("R/analysis.R:") and ("is not a column" in line or "no argument" in line)}
    check("'ok7' is known up to pivot_wider(" in out and "'ok9' is known up to" not in out,
          f"stop note missing for pivot_wider(), or across(where()) in mutate() still stops tracking:\n{out}")
    # --steps: the column set after every verb, as text and as JSON
    steps = subprocess.run([tool, "--steps", "--root", root, analysis], capture_output=True, text=True, timeout=120).stdout
    check("R/analysis.R:18: select(region) => region" in steps and "=> (unknown)" in steps,
          f"--steps listing unexpected:\n{steps}")
    sj = json.loads(subprocess.run([tool, "--json", "--steps", "--root", root, analysis],
                                   capture_output=True, text=True, timeout=120).stdout)
    check(set(sj) == {"findings", "steps"} and any(st["line"] == 18 and st["columns"] == ["region"] for st in sj["steps"]),
          f"--json --steps shape unexpected: {list(sj)}")
    with open(analysis) as f:
        src = f.read().splitlines()
    bad = {i for i, line in enumerate(src, 1) if line.rstrip().endswith("# bad")}
    check(bad <= reported, f"r-colcheck missed lines {sorted(bad - reported)}:\n{out}")
    check(reported <= bad, f"r-colcheck flagged correct lines {sorted(reported - bad)}:\n{out}")
    # Call shapes a parser walker gets wrong if it assumes a call's head is a
    # symbol or `pkg::name`: both tools must survive every one of them.
    shapes = os.path.join(root, "R", "shapes.R")
    with open(shapes, "w") as f:
        f.write("library(magrittr)\n"
                "shapes_fn <- function(x) x\n"
                "a <- purrr::insistently(shapes_fn)(1)\n"
                "b <- (function(x) x)(2)\n"
                "c2 <- shapes_fn(shapes_fn)(3)\n"
                "d <- list(f = shapes_fn)$f(4)\n"
                "e <- list(shapes_fn)[[1]](5)\n"
                "g <- base::identity(shapes_fn)(base::identity(shapes_fn)(6))\n"
                "h <- `::`(base, identity)(7)\n"
                "df <- tibble::tibble(a = 1)\n"
                "df %>% dplyr::select(zz) # bad\n")
    out = subprocess.run([tool, "--root", root, shapes], capture_output=True, text=True, timeout=120)
    check(out.returncode == 1 and "R/shapes.R:11: 'zz'" in out.stdout and "error" not in out.stdout
          and "checker error" not in out.stderr, f"odd call shapes broke r-colcheck:\n{out.stdout}{out.stderr}")
    cm = subprocess.run([os.path.join(REPO, "plugins", "r-lsp", "bin", "r-callmap"), "--root", root],
                        capture_output=True, text=True, timeout=120)
    check(cm.returncode == 0 and "shapes_fn" in cm.stdout, f"odd call shapes broke r-callmap:\n{cm.stdout}{cm.stderr}")
    # A frame reassigned from itself: every step reads the old frame, not the result.
    again = os.path.join(root, "R", "again.R")
    with open(again, "w") as f:
        f.write("library(magrittr)\n"
                "x <- tibble::tibble(a = 1, b = 2)\n"
                "x <- x %>% dplyr::filter(a > 0) %>% dplyr::select(b)\n"
                "x <- dplyr::select(dplyr::filter(x, b > 0), b)\n"
                "x %>% dplyr::select(a) # bad\n"
                "n <- nrow(x %>% dplyr::filter(zz > 1)) # bad\n")
    out = subprocess.run([tool, "--root", root, again], capture_output=True, text=True, timeout=120)
    got = {int(l.split(":")[1]) for l in out.stdout.splitlines() if l.startswith("R/again.R:")}
    check(got == {5, 6}, f"reassignment from itself mis-checked (lines {sorted(got)}):\n{out.stdout}")
    # Verbs that change the column set: a:b ranges, join suffixes and renamed
    # keys, the groups summarise() leaves, pivot_longer() with several
    # names_to, set-keeping verbs, %<>% and assignment inside a branch.
    verbs_r = os.path.join(root, "R", "verbs.R")
    out = subprocess.run([tool, "--root", root, verbs_r], capture_output=True, text=True, timeout=120)
    reported = {int(line.split(":")[1]) for line in out.stdout.splitlines()
                if line.startswith("R/verbs.R:") and ("is not a column" in line or "no argument" in line)}
    with open(verbs_r) as f:
        vsrc = f.read().splitlines()
    bad = {i for i, line in enumerate(vsrc, 1) if line.rstrip().endswith("# bad")}
    check(bad <= reported, f"verb tracking missed lines {sorted(bad - reported)}:\n{out.stdout}")
    check(reported <= bad, f"verb tracking flagged correct lines {sorted(reported - bad)}:\n{out.stdout}")
    check("hit an error" not in out.stdout and "checker error" not in out.stderr, f"checker errors on R/verbs.R:\n{out.stdout}{out.stderr}")
    vsteps = subprocess.run([tool, "--steps", "--root", root, verbs_r], capture_output=True, text=True, timeout=120).stdout
    check('select(sales, region:amount) => region, year, amount' in vsteps
          and 'left_join(sales, other, by = "region") => region, year, amount.x, customer, amount.y, pop' in vsteps
          and 'left_join(sales, lookup, by = c(customer = "id")) => region, year, amount, customer, score' in vsteps
          and 'summarise(., m = mean(amount)) => region, year, m [grouped by region]' in vsteps
          and 'names_sep = "_") => region, year, amount, customer, set, a, b' in vsteps
          and 'unite(sales, "key", region, year) => key, amount, customer' in vsteps,
          f"verb steps unexpected:\n{vsteps}")
    tricky = subprocess.run([tool, "--root", root, os.path.join(root, "R", "tricky_ok.R")],
                            capture_output=True, text=True, timeout=120).stdout
    tricky_findings = [l for l in tricky.splitlines() if "is not a column" in l or "no argument" in l]
    check(not tricky_findings, f"r-colcheck flagged correct tricky code:\n{tricky}")
    check("'s5' is known up to purrr::map(~.x %>% select(all_of(unknown_cols)))" in tricky and "inside its lambda at select(all_of(unknown_cols))" in tricky
          and "'s7' is known up to" not in tricky, f"lambda notes unexpected:\n{tricky}")
    # one unknown input leaves the binding partly known; all unknown was never known: no note either way
    check("'b4' is known up to" not in tricky and "'b5' is known up to" not in tricky
          and "'b9' is known up to" not in tricky, f"row-binding notes unexpected:\n{tricky}")
    tsteps = subprocess.run([tool, "--steps", "--root", root, os.path.join(root, "R", "tricky_ok.R")],
                            capture_output=True, text=True, timeout=120)
    check('select(starts_with("am"), region) => amount, region' in tsteps.stdout
          and 'select(-contains("OUNT")) => region, year, customer' in tsteps.stdout
          and 'select(matches("^(reg|cust)")) => region, customer' in tsteps.stdout
          and 'bind_rows(sales, unknown_frame) => at least region, year, amount, customer' in tsteps.stdout,
          f"tidyselect helpers or partial binding not expanded in --steps:\n{tsteps.stdout}")
    check(" of " in tsteps.stderr and "on partly known columns" in tsteps.stderr, f"summary line lacks coverage:\n{tsteps.stderr}")
    # usage, not an empty check, for --help, an unknown flag and a subcommand that does not exist
    hp = subprocess.run([tool, "--help"], capture_output=True, text=True, timeout=60)
    check(hp.returncode == 0 and hp.stdout.startswith("usage:") and "refresh" in hp.stdout, f"--help unexpected:\n{hp.stdout}{hp.stderr}")
    for bad_args in (["list"], ["--nope", os.path.join(root, "R", "tricky_ok.R")]):
        ua = subprocess.run([tool] + bad_args, capture_output=True, text=True, timeout=60)
        check(ua.returncode == 2 and "usage:" in ua.stderr and ("no such file" in ua.stderr or "unknown option" in ua.stderr),
              f"bad arguments {bad_args} did not print usage:\n{ua.stdout}{ua.stderr}")
    check("'k' is known up to" not in tricky
          and "'k24' is known up to summarise(across(where(is.numeric), mean), .by = region)" in tricky
          and "cannot be worked out statically" in tricky, f"stop notes for summarise(across()) unexpected:\n{tricky}")
    # a native-pipe step through a call the checker does not know gets the note the %>% form gets
    check("'n3' is known up to unknown_wrapper_fn(sales)" in tricky, f"|> through an unknown function must stop tracking with a note:\n{tricky}")
    # janitor::clean_names() is computed for plain headers, in both pipe shapes, so it no longer ends tracking
    check("'m' is known up to" not in tricky and "'m2' is known up to" not in tricky
          and "janitor::clean_names(sales) => region, year, amount, customer" in tsteps.stdout
          and "janitor::clean_names() => region, year, amount, customer" in tsteps.stdout,
          f"clean_names() should be followed:\n{tricky}\n{tsteps.stdout}")
    # a finding names the known column the typo is closest to
    an = subprocess.run([tool, "--root", root, analysis], capture_output=True, text=True, timeout=120).stdout
    check("R/analysis.R:23: 'regoin' is not a column at this point (columns: region, year, amount, customer; data from sales.csv); did you mean region?" in an
          and "'mpgg' is not a column" in an and "did you mean mpg?" in an, f"findings lack the nearest column name:\n{an}")
    fj = json.loads(subprocess.run([tool, "--json", "--root", root, analysis], capture_output=True, text=True, timeout=120).stdout)
    check(any(f.get("name") == "regoin" and f.get("nearest") == ["region"] for f in fj), f"--json findings lack the nearest names:\n{json.dumps(fj)[:600]}")

    # Fixes pinned: a TSV read by fread()/vroom() (which guess the separator),
    # header = T, and a helper calling its sibling in the same file.
    with open(os.path.join(root, "data", "sales.tsv"), "w") as f:
        f.write("region\tyear\tamount\tcustomer\nNorth\t2020\t10.5\tA\n")
    fixes = os.path.join(root, "R", "fixes.R")
    with open(fixes, "w") as f:
        f.write("library(dplyr)\n"
                "t1 <- data.table::fread('data/sales.tsv') |> select(region, nope1) # bad\n"
                "t2 <- vroom::vroom('data/sales.tsv') |> select(amount, nope2) # bad\n"
                "t3 <- read.csv('data/sales.csv', header = T) |> select(year, nope3) # bad\n"
                "tidy <- function(d) d %>% select(a)\n"
                "clean <- function(d) tidy(d) %>% mutate(z = 1)\n"
                "x <- clean(tibble::tibble(a = 1, b = 2))\n"
                "x %>% select(z, nope4) # bad\n"
                "flag <- a | b\n"
                "flag %>% select(ok)\n")
    out = subprocess.run([tool, "--root", root, fixes], capture_output=True, text=True, timeout=120)
    got = {(int(l.split(":")[1]), l.split("'")[1]) for l in out.stdout.splitlines() if l.startswith("R/fixes.R:") and "is not a column" in l}
    check(got == {(2, "nope1"), (3, "nope2"), (4, "nope3"), (8, "nope4")}, f"pinned fixes mis-checked: {sorted(got)}\n{out.stdout}")
    check("columns: region, year, amount, customer; data from sales.tsv" in out.stdout, f"a TSV header was not guessed:\n{out.stdout}")
    # Logic: a name assigned twice in one call, mutate(x = NULL), and an
    # aggregate run per group that summarise() left behind (said, not a finding).
    logic = os.path.join(root, "R", "logic.R")
    with open(logic, "w") as f:
        f.write("library(dplyr)\nsales <- read_csv('data/sales.csv')\n"
                "g1 <- sales |> group_by(region, year) |> summarise(total = sum(amount)) |> mutate(share = total / sum(total))\n"
                "g2 <- sales |> group_by(region, year) |> summarise(total = sum(amount), .groups = 'drop') |> mutate(share = total / sum(total))\n"
                "g3 <- sales |> group_by(region, year) |> summarise(total = sum(amount)) |> ungroup() |> mutate(share = total / sum(total))\n"
                "g4 <- sales |> group_by(region, year) |> summarise(total = sum(amount)) |> filter(total == max(total))\n"
                "g5 <- sales |> group_by(region, year) |> summarise(total = sum(amount)) |> mutate(pct = total * 100)\n"
                "d1 <- sales |> mutate(amount = amount * 2, year = NULL) |> select(year) # bad\n"
                "d2 <- sales |> summarise(n = n(), n = sum(amount)) # bad\n"
                "d3 <- sales |> mutate(x = amount + 1, x = round(x))\n"
                "d4 <- sales |> mutate(x = amount + 1, x = year) # bad\n")
    with open(logic, "a") as f:
        f.write("s1 <- sales |> filter(amount > amount) # bad\ns2 <- sales |> mutate(z = amount == amount) # bad\ns3 <- sales |> filter(amount > year)\n")
    lg = subprocess.run([tool, logic, "--root", root], capture_output=True, text=True, timeout=120)   # --root anywhere
    lines = [l for l in lg.stdout.splitlines() if l.startswith("R/logic.R:")]
    grouped = {int(l.split(":")[1]) for l in lines if "runs per group" in l}
    findings = {int(l.split(":")[1]) for l in lines if "runs per group" not in l}
    check(grouped == {3, 6} and "left by summarise() on line 3" in lg.stdout and "so sum() is computed within each group" in lg.stdout,
          f"grouped-aggregate notes unexpected (lines {sorted(grouped)}):\n{lg.stdout}")
    check(findings == {8, 9, 11, 12, 13} and "R/logic.R:9: summarise() assigns 'n' twice" in lg.stdout and "R/logic.R:8: 'year' is not a column" in lg.stdout
          and "R/logic.R:12: 'amount > amount' compares the column with itself (always FALSE" in lg.stdout
          and "R/logic.R:13: 'amount == amount' compares the column with itself (always TRUE" in lg.stdout,
          f"logic findings unexpected (lines {sorted(findings)}):\n{lg.stdout}")
    check(lg.returncode == 1 and "5 finding(s), 2 note(s)" in lg.stderr, f"logic exit status or summary unexpected: {lg.returncode}\n{lg.stderr}")
    lsteps = subprocess.run([tool, "--steps", "--root", root, logic], capture_output=True, text=True, timeout=120).stdout
    check("mutate(sales, amount = amount * 2, year = NULL) => region, amount, customer (amount overwritten)" in lsteps,
          f"--steps does not show the overwrite and the NULL drop:\n{lsteps}")
    ctx = hook_context(run_hook(hook, {"tool_name": "Edit", "tool_input": {"file_path": logic}, "cwd": root}, env={"CLAUDE_PROJECT_DIR": root}))
    check("Steps whose result cannot be what the code means" in ctx and "assigns 'n' twice" in ctx
          and "Steps that run per group because an earlier summarise() left the frame grouped" in ctx, f"hook note lacks the logic sections:\n{ctx}")

    # Reader arguments that change the column names (col_select, col_names,
    # header=, id=, skip=, make.names repair, fread select/drop) and base-R
    # column access (df$x, df[["x"]], df[, "x"], columns written in place).
    def marked_file(name):
        path = os.path.join(root, "R", name + ".R")
        out = subprocess.run([tool, "--root", root, path], capture_output=True, text=True, timeout=120)
        check("checker error" not in out.stderr and "hit an error" not in out.stdout, f"{name}.R: checker error:\n{out.stdout}{out.stderr}")
        reported = {int(line.split(":")[1]) for line in out.stdout.splitlines()
                    if line.startswith(f"R/{name}.R:") and "is not a column" in line}
        with open(path) as f:
            marked = {i for i, line in enumerate(f.read().splitlines(), 1) if line.rstrip().endswith("# bad")}
        check(marked <= reported, f"{name}.R: r-colcheck missed lines {sorted(marked - reported)}:\n{out.stdout}")
        check(reported <= marked, f"{name}.R: r-colcheck flagged correct lines {sorted(reported - marked)}:\n{out.stdout}")
        return out.stdout
    out = marked_file("readers")
    check("'r45' is known up to read_csv(" in out and "its col_select argument cannot be read statically" in out
          and "'r46' is known up to read.csv(" in out and "its header argument cannot be read statically" in out,
          f"an unreadable reader argument must stop tracking with a note:\n{out}")
    marked_file("access")
    # readxl and arrow arguments, and an arrow dataset directory, when both packages are installed
    if rscript('cat(requireNamespace("readxl", quietly = TRUE) && requireNamespace("arrow", quietly = TRUE))').strip() == "TRUE":
        rscript('invisible(file.copy(readxl::readxl_example("datasets.xlsx"), "data/datasets.xlsx")); '
                'arrow::write_parquet(read.csv("data/sales.csv"), "data/sales.parquet"); '
                'arrow::write_dataset(read.csv("data/sales.csv"), "data/sales_ds", partitioning = "year")', cwd=root)
        out = marked_file("readers_pkgs")
        check("'x11' is known up to readxl::read_excel(" in out and "its sheet argument cannot be read statically" in out,
              f"an unreadable sheet argument must stop tracking with a note:\n{out}")
        check("data from sales_ds" in out, f"an arrow dataset directory was not read:\n{out}")

    # Frames passed through project functions (same file, source()d file), both pipes.
    funcs = os.path.join(root, "R", "funcs.R")
    out = subprocess.run([tool, "--root", root, funcs], capture_output=True, text=True, timeout=120).stdout
    reported = {int(line.split(":")[1]) for line in out.splitlines()
                if line.startswith("R/funcs.R:") and ("is not a column" in line or "no argument" in line)}
    with open(funcs) as f:
        fsrc = f.read().splitlines()
    bad = {i for i, line in enumerate(fsrc, 1) if line.rstrip().endswith("# bad")}
    check(bad <= reported, f"function tracking missed lines {sorted(bad - reported)}:\n{out}")
    check(reported <= bad, f"function tracking flagged correct lines {sorted(reported - bad)}:\n{out}")
    check("helpers_src.R" not in [l.split(":")[0] for l in out.splitlines()], f"findings attributed to the sourced helper file:\n{out}")
    # a stop inside a followed project function is reported at the call site, with the step inside
    check("'wd' is known up to widen(sales)" in out
          and "inside widen() at its pivot_wider(names_from = year, values_from = amount) (R/funcs.R:" in out
          and "r-colcheck learn \"widen\"" in out,
          f"stop inside a followed function not reported at the call site:\n{out}")
    check("'op' is known up to opaque_fn()" in out and "following its code gave no column set" in out,
          f"direct call of an opaque project function got no note:\n{out}")
    srcs = {int(l.split(":")[1]): l for l in out.splitlines() if l.startswith("R/funcs.R:") and "is not a column" in l}
    a1 = [l for n, l in srcs.items() if "asserted_select(unknown_input)" in fsrc[n - 1]]
    check(a1 and "the columns require_columns() requires on line" in a1[0], f"assertion-backed finding lacks its source:\n{out}")
    steps = subprocess.run([tool, "--steps", "--root", root, funcs], capture_output=True, text=True, timeout=120).stdout
    check("=> at least region, amount, amt2" in steps, f"open column set not shown in --steps:\n{steps}")
    tsteps = subprocess.run([tool, "--steps", "--root", root, os.path.join(root, "R", "tricky_ok.R")], capture_output=True, text=True, timeout=120).stdout
    check("split(.$region) => each: region, year, amount, customer" in tsteps and "purrr::map_dfr(~.x %>% summarise(total = sum(amount))) => total" in tsteps,
          f"list-of-frames steps not shown:\n{tsteps}")
    ws_note = [l for l in out.splitlines() if "'ws' is known up to widen_src(sales)" in l]
    check(ws_note and "inside widen_src() at its pivot_wider(" in ws_note[0] and "(R/helpers_src.R:6)" in ws_note[0],
          f"a one-expression helper in another file must get the note with the helper's own line:\n{out}")
    steps = subprocess.run([tool, "--steps", "--root", root, funcs], capture_output=True, text=True, timeout=120).stdout
    check("R/funcs.R:11: mutate(., big = amount > 100) =>" in steps, f"|> chain steps not shown as verb(., ...):\n{steps}")

    # In a package, functions from every file under R/ are known without source().
    pkg = os.path.join(s.scratch, "pkgcols")
    shutil.copytree(os.path.join(FIXTURES, "pkgcols"), pkg)
    out = subprocess.run([tool, "--root", pkg, os.path.join(pkg, "R", "pipeline.R")],
                         capture_output=True, text=True, timeout=120).stdout
    check("R/pipeline.R:5: 'amout'" in out and out.count("is not a column") == 1,
          f"package-wide function lookup unexpected:\n{out}")
    # Argument names against the definition, the reported line, exit code and summary.
    with open(os.path.join(pkg, "R", "calls.R"), "w") as f:
        f.write("totals <- function(df, min_rows = 1) df\n"
                "x <- readr::read_csv('data/sales.csv') %>%\n  dplyr::mutate(z = 1) %>%\n  totals(min_row = 2)\n"
                "y <- totals(x, rows = 3)\nz <- totals(x, 1, 2)\nok <- totals(x, min_rows = 2)\n"
                "testthat::expect_error(totals(x, nope = 1))\n")
    run = subprocess.run([tool, "--root", pkg, os.path.join(pkg, "R", "calls.R")], capture_output=True, text=True, timeout=120)
    lines = [l for l in run.stdout.splitlines() if l.startswith("R/calls.R:")]
    check(any(l.startswith("R/calls.R:4:") and "partial-matches it to 'min_rows'" in l for l in lines) and
          any(l.startswith("R/calls.R:5:") and "no argument 'rows'" in l for l in lines) and
          any(l.startswith("R/calls.R:6:") and "takes 2 argument(s)" in l for l in lines) and len(lines) == 3,
          f"argument check unexpected:\n{run.stdout}")
    check(run.returncode == 1, f"r-colcheck exit code with findings was {run.returncode}")
    check("r-colcheck: 1 of " in run.stderr and "3 finding(s)" in run.stderr, f"summary line unexpected:\n{run.stderr}")
    clean = subprocess.run([tool, "--root", pkg, os.path.join(pkg, "R", "load.R")], capture_output=True, text=True, timeout=120)
    check(clean.returncode == 0 and not clean.stdout.strip() and "r-colcheck:" in clean.stderr,
          f"clean file: exit {clean.returncode}, stdout {clean.stdout!r}, stderr {clean.stderr!r}")
    ctx = hook_context(run_hook(hook, {"tool_name": "Edit", "tool_input": {"file_path": analysis}, "cwd": root},
                                env={"CLAUDE_PROJECT_DIR": root}))
    check("'amt'" in ctx and "regoin" in ctx, f"post-edit hook context missing findings:\n{ctx[:400]}")
    # a name wrong on several lines is one line of the note; the explanation comes once per session
    check("R/analysis.R: 'regoin' is not a column at lines 23, 31, 34" in ctx and "did you mean region?" in ctx, f"repeated name not collapsed in the note:\n{ctx[:1500]}")
    sdata = os.path.join(s.scratch, "colcheck_hook_data")
    first = hook_context(run_hook(hook, {"session_id": "note-session", "tool_name": "Edit", "tool_input": {"file_path": analysis}, "cwd": root},
                                  env={"CLAUDE_PROJECT_DIR": root, "CLAUDE_PLUGIN_DATA": sdata}))
    second = hook_context(run_hook(hook, {"session_id": "note-session", "tool_name": "Edit", "tool_input": {"file_path": analysis}, "cwd": root},
                                   env={"CLAUDE_PROJECT_DIR": root, "CLAUDE_PLUGIN_DATA": sdata}))
    check("If the data really has that column" in first and "If the data really has that column" not in second and len(second) < len(first),
          f"the note's explanation should be said once per session:\n{second[:800]}")
    ctx = hook_context(run_hook(hook, {"tool_name": "Edit", "tool_input": {"file_path": os.path.join(pkg, "R", "calls.R")},
                                       "cwd": pkg}, env={"CLAUDE_PROJECT_DIR": pkg}))
    check("argument names do not match" in ctx and "no argument 'rows'" in ctx, f"post-edit hook missed argument findings:\n{ctx[:600]}")
    # An R Markdown file: its chunks are checked at the file's own line
    # numbers, prose and other engines are not, a broken chunk costs itself.
    report = os.path.join(root, "report.Rmd")
    with open(report, "w") as f:
        f.write("---\ntitle: report\n---\n\nProse: sales %>% select(nonsense) is not code.\n\n"
                "```{r setup}\nlibrary(tidyverse)\nsales <- read_csv(\"data/sales.csv\")\n```\n\n"
                "```{python}\nsales.select(amt)\n```\n\n"
                "```{r broken}\nsales %>% filter(\n```\n\n"
                "```{r}\nsales %>% filter(amt > 0) # bad\nsales %>% select(region)\n```\n")
    out = subprocess.run([tool, "--root", root, report], capture_output=True, text=True, timeout=120)
    got = {int(l.split(":")[1]) for l in out.stdout.splitlines() if l.startswith("report.Rmd:") and "is not a column" in l}
    chunk = line_of(report, "sales %>% filter(")
    check(out.returncode == 1 and got == {line_of(report, "# bad")} and "nonsense" not in out.stdout,
          f"Rmd chunks not checked at their own lines:\n{out.stdout}{out.stderr}")
    check(f"report.Rmd:{chunk + 1}: the R chunk starting at line {chunk} does not parse ({chunk + 1}:0: unexpected end of input), so nothing in that chunk was checked" in out.stdout
          and "did not parse" not in out.stderr, f"broken chunk not reported on its own:\n{out.stdout}{out.stderr}")
    ctx = hook_context(run_hook(hook, {"tool_name": "Edit", "tool_input": {"file_path": report}, "cwd": root},
                                env={"CLAUDE_PROJECT_DIR": root}))
    check("'amt'" in ctx and "the R chunk starting at line" in ctx and "fix this first" in ctx,
          f"post-edit hook context for an Rmd unexpected:\n{ctx[:600]}")
    # A file that does not parse is said so, by the CLI and by the hook.
    broken = os.path.join(root, "R", "broken.R")
    with open(broken, "w") as f:
        f.write("library(magrittr)\nx <- tibble::tibble(a = 1)\nx %>% dplyr::select(a))\ny <- x %>% dplyr::select(zz)\n")
    out = subprocess.run([tool, "--root", root, broken], capture_output=True, text=True, timeout=120)
    check(out.returncode == 1 and "R/broken.R:3: this file does not parse (3:23: unexpected ')')" in out.stdout
          and "1 file(s) did not parse" in out.stderr, f"syntax error not reported:\n{out.stdout}{out.stderr}")
    ctx = hook_context(run_hook(hook, {"tool_name": "Edit", "tool_input": {"file_path": broken}, "cwd": root},
                                env={"CLAUDE_PROJECT_DIR": root}))
    check("does not parse" in ctx and "unexpected ')'" in ctx and "fix this first" in ctx,
          f"post-edit hook silent on a syntax error:\n{ctx[:400]}")
    silent = run_hook(hook, {"tool_name": "Edit", "tool_input": {"file_path": "/tmp/x.py"}})
    check(not silent.strip(), "post-edit hook spoke for a non-R file")


def scenario_callmap_cli(s):
    """r-callmap's argument conventions and two --diff false positives: an
    unknown flag exits 2 (not a function named --jsn), a trailing --depth is
    an error, `help` prints usage, a deletion above the first definition does
    not change it, and a prose edit in an Rmd is not top-level code."""
    tool = os.path.join(REPO, "plugins", "r-lsp", "bin", "r-callmap")
    root = os.path.join(s.scratch, "callmap_cli")
    shutil.copytree(os.path.join(FIXTURES, "pkgscope"), root)
    for bad in (["--jsn"], ["f", "--depth"], ["--root"]):
        r = subprocess.run([tool, "--root", root, *bad], capture_output=True, text=True, timeout=60)
        check(r.returncode == 2 and "usage:" in r.stderr and "not defined in the project" not in r.stdout, f"r-callmap {bad} should exit 2 with usage:\n{r.stdout}{r.stderr}")
    r = subprocess.run([tool, "help"], cwd=root, capture_output=True, text=True, timeout=60)
    check(r.returncode == 0 and r.stdout.startswith("usage:"), f"r-callmap help unexpected:\n{r.stdout}")
    git = lambda *a: subprocess.run(["git", "-c", "user.email=t@t", "-c", "user.name=t", *a], cwd=root, capture_output=True, text=True, check=True)  # noqa: E731
    git("init", "-q")
    git("add", "-A")
    git("commit", "-qm", "init")
    flags = os.path.join(root, "R", "flags.R")
    with open(flags) as f:
        src = f.read()
    with open(flags, "w") as f:
        f.write("# a comment above the first definition\n# and another\n" + src)
    git("commit", "-qam", "comments")
    with open(flags, "w") as f:
        f.write(src)
    d = subprocess.run([tool, "--root", root, "--diff"], capture_output=True, text=True, timeout=120).stdout
    check("No definition changed" in d and "[body changed]" not in d, f"a deletion above the first definition was reported as a change:\n{d}")
    git("commit", "-qam", "restore")
    vig = os.path.join(root, "vignettes", "usage.Rmd")
    with open(vig, "a") as f:
        f.write("\nSome more prose about the package.\n")
    d = subprocess.run([tool, "--root", root, "--diff"], capture_output=True, text=True, timeout=120).stdout
    check("Top-level code changed" not in d, f"a prose edit in an Rmd was reported as top-level code:\n{d}")


def scenario_colmap(s):
    """r-colmap: the data map (sources, their columns, where each is used,
    dropped or created), built from r-colcheck's step records."""
    tool = os.path.join(REPO, "plugins", "r-lsp", "bin", "r-colmap")
    root = os.path.join(FIXTURES, "columns")

    def run(*args, cwd=root):
        out = subprocess.run([tool, *args], cwd=cwd, capture_output=True, text=True, timeout=180)
        check(out.returncode == 0, f"r-colmap {' '.join(args)} exited {out.returncode}:\n{out.stderr}")
        return out.stdout

    full = run("--file", "R/analysis.R")
    # sales.csv, mtcars, and each tibble()/tribble() literal as a source of its
    # own (the two with the same text on lines 47 and 48 told apart; the
    # unnest() of one no longer splits off a sixth, line-less source)
    check("r-colmap: 5 data sources read by 1 file" in full.splitlines()[0], f"colmap header unexpected:\n{full[:300]}")
    check("tibble() literal at R/analysis.R:47   first used at R/analysis.R:47   2 columns: id, data" in full
          and "tibble() literal at R/analysis.R:48" in full and full.count("tibble() literal   first used") == 0, f"literal sources unexpected:\n{full}")
    check("sales.csv   first used at R/analysis.R:10   4 columns: region, year, amount, customer" in full,
          f"colmap source line unexpected:\n{full[:1500]}")
    check("  customer             used 1 time: R/analysis.R:73 (select)" in full, f"last_col() use of customer missing:\n{full[:2500]}")
    check("  region               used " in full and "dropped at R/analysis.R:23 (summarise)" in full, f"region line unexpected:\n{full[:2500]}")
    check("created by the code:" in full and "  total                at R/analysis.R:10 (summarise), from amount; used 1 time" in full
          and "  big                  at R/analysis.R:12 (mutate), from amt; no use seen in the steps followed; no later step carries it" in full
          and "  amt                  at R/analysis.R:12 (rename), from amount; used 1 time" in full, f"created columns (with their lineage) unexpected:\n{full}")
    check("never used afterwards" not in full, "the map must not claim a column is never used; it says where the frame was last seen")
    for wrong in ("regoin", "amuont", "yaer"):
        check(f"  {wrong} " not in full, f"a reported mistake ({wrong}) is listed as a column the code creates:\n{full}")
    check("datasets::mtcars" in full and "tibble() literal at R/analysis.R:" in full, f"other sources missing:\n{full}")

    one = run("--file", "R/analysis.R", "amount", "regoin")
    check("amount:\n  sales.csv\n    used at R/analysis.R:10 (filter(amount > threshold, year >= 2020)), R/analysis.R:10 (summarise(total = sum(amount), n = n()))" in one,
          f"column lookup unexpected:\n{one}")
    check("ggplot(sales, aes(x = year, y = amount, colour = region))" in one, f"ggplot use of amount missing:\n{one}")
    check("regoin:\n  not a column of any source the map knows. Nearest: region" in one, f"unknown column lookup unexpected:\n{one}")
    # lineage: what a column is computed from, and what is computed from it, directly and through those
    check("    feeds (by name within this source): total (R/analysis.R:10), amt (R/analysis.R:12), t (R/analysis.R:23), m (R/analysis.R:30); and through those: share (R/analysis.R:11), big (R/analysis.R:12)" in one,
          f"lineage of amount unexpected:\n{one}")
    tot = run("--file", "R/analysis.R", "total")
    check("total:\n  sales.csv   (not in the data: created by the code)\n    created at R/analysis.R:10 (summarise(total = sum(amount), n = n()))" in tot
          and "    derived from: amount (R/analysis.R:10)\n    feeds (by name within this source): share (R/analysis.R:11)" in tot, f"lineage of a created column unexpected:\n{tot}")

    data = json.loads(run("--json", "--file", "R/analysis.R"))
    by_src = {d["source"]: d for d in data}
    check(by_src["sales.csv"]["columns"] == ["region", "year", "amount", "customer"] and by_src["sales.csv"]["unused"] == []
          and any(u["column"] == "total" and not u["in_data"] and u["sites"][0]["kind"] == "created" and u["sites"][0]["verb"] == "summarise"
                  and u["sites"][0]["from"] == ["amount"] and u["derived_from"] == ["amount"] and u["feeds"] == ["share"]
                  for u in by_src["sales.csv"]["uses"]), f"colmap JSON unexpected:\n{json.dumps(data)[:1200]}")

    s3 = run(cwd=os.path.join(FIXTURES, "s3columns"))
    check('cached schema "returns/monthly"   first used at R/report.R:10   3 columns: date, fund_id, ret' in s3
          and "Not mapped: 2 frames from sources the checker cannot open" in s3, f"colmap on the cache fixture unexpected:\n{s3}")
    empty = s.make_project("colmap_empty", {"R/x.R": os.path.join(FIXTURES, "bugs", "bugs.R")})
    none = run("--root", empty, cwd=empty)
    check("No data source with known columns: nothing to map from the data." in none and "parameter df of g()   at R/x.R:17   reads 1 column: x" in none,
          f"colmap with no data source should say so and still list the parameters:\n{none}")

    # Where a frame was last seen, on a package whose functions take frames as
    # parameters and whose callers are not all followed.
    fates_root = os.path.join(FIXTURES, "colmap")
    fates = run(cwd=fates_root)
    head = fates.splitlines()[0]
    check("r-colmap: 10 data sources read by 9 files" in head and "function parameters used as frames" in head
          and "\"no use seen\" means none in the steps followed" in fates.splitlines()[1], f"colmap fates header unexpected:\n{fates[:600]}")
    # created in a callee, read in a caller the checker followed: a use, across files
    check("  rolling_12m_value_add at R/panel.R:3 (dplyr::mutate), from value; used 1 time" in fates, f"cross-function use missing:\n{fates}")
    # a sink, and a return to callers the checker never reached
    check("  trailing_value_add   at R/panel.R:8 (dplyr::mutate), from value; no use seen in the steps followed; written by saveRDS() at R/panel.R:9" in fates
          and "  self_loaded_created  at R/selfload.R:4 (dplyr::mutate), from value; no use seen in the steps followed; returned by build_panel() at R/selfload.R:4" in fates,
          f"fates unexpected:\n{fates}")
    # a join's other frame keeps its own source; its columns are carried in, not created
    check("users.csv   first used at R/load.R:5   3 columns: id, userLastName, region" in fates
          and "  userLastName         carried into parameter df of with_users() and panel.csv at R/panel.R:13 (dplyr::left_join)" in fates
          and "  id                   used 1 time: R/panel.R:13 (dplyr::left_join)" in fates
          and "  userLastName         at R/panel.R:13" not in fates, f"join source unexpected:\n{fates}")
    # a parameter used as a frame: what the body reads of it and adds to it
    check("parameter df of with_users()   at R/panel.R:13   reads 1 column: id" in fates
          and "  joined from users.csv at R/panel.R:13 (dplyr::left_join):\n  userLastName         no use seen in the steps followed; returned by with_users() at R/panel.R:13" in fates
          and "parameter panel of assemble_report()   at R/report.R:2   reads 1 column: rolling_12m_value_add" in fates,
          f"parameter sources unexpected:\n{fates}")
    # deframe() reads two columns; nest() consumes what it nests and is followed; a step the checker cannot follow ends tracking
    check("R/panel.R:17 (tibble::deframe)" in fates and "  nst                  at R/more.R:25 (dplyr::mutate), from value; used 1 time" in fates
          and "tracking ended at R/more.R:25 (tidyr::nest)" in fates, f"verb handling unexpected:\n{fates}")
    # (issue 1) a join of two parameters where y went through a verb first: no crash, the carried column under its own source
    check("parameter orders of f()   at R/params_join.R:4   reads 1 column: user_id" in fates
          and "  joined from parameter users of f() at R/params_join.R:4 (dplyr::left_join):\n  status               no use seen in the steps followed; returned by f() at R/params_join.R:4" in fates
          and "could not be listed" not in fates, f"join of two parameters unexpected:\n{fates}")
    # (issue 2) a literal keeps its line past the first verb: no location-less source, the sink and the return land on the literal
    check("tribble() literal   first used" not in fates
          and "  total                at R/literals.R:2 (dplyr::mutate), from a, b; no use seen in the steps followed; written by saveRDS() at R/literals.R:7" in fates
          and "  total                at R/literals.R:11 (dplyr::mutate), from a, b; no use seen in the steps followed; returned by return_totals() at R/literals.R:11" in fates
          and "  amount               never named by the code; returned by join_users() at R/literal_join.R:10" in fates
          and "  joined from tribble() literal at R/literal_join.R:6 at R/literal_join.R:10 (dplyr::left_join):\n  user_name            used 1 time" in fates,
          f"literal sources unexpected:\n{fates}")
    # (issue 3) a column made on the y side before a join is carried, not dead; a test's literal is named by the test's line
    check("  score                at R/yside.R:2 (dplyr::mutate), from nested; carried into tribble() literal at R/yside.R:4 at R/yside.R:5 (dplyr::left_join)" in fates
          and "tribble() literal at tests/testthat/test-rename_join.R:1   first used at R/rename_join.R:2   2 columns: user_id, name" in fates
          and "  user_name            at R/rename_join.R:2 (dplyr::rename), from name; carried into tribble() literal at tests/testthat/test-rename_join.R:9 at R/rename_join.R:4 (dplyr::left_join)" in fates
          and "tribble() literal at R/rename_join.R" not in fates, f"y-side and test-literal handling unexpected:\n{fates}")
    one = run("userLastName", cwd=fates_root)
    check("carried into another frame at R/panel.R:13" in one and "then returned by with_users() at R/panel.R:13" in one,
          f"column lookup with fates unexpected:\n{one}")
    data = json.loads(run("--json", cwd=fates_root))
    kinds = {d["source"]: d["kind"] for d in data}
    panel = next(d for d in data if d["source"] == "panel.csv")
    tva = next(u for u in panel["uses"] if u["column"] == "trailing_value_add")
    check(kinds.get("panel.csv") == "data" and kinds.get("parameter df of make_panel()") == "parameter"
          and tva["fates"] and tva["fates"][0]["kind"] == "written" and tva["fates"][0]["fn"] == "saveRDS",
          f"colmap JSON fates unexpected:\n{json.dumps(data)[:1500]}")
    colcheck = os.path.join(REPO, "plugins", "r-lsp", "bin", "r-colcheck")
    steps = subprocess.run([colcheck, "--steps", "R/selfload.R", "R/panel.R"], cwd=fates_root, capture_output=True, text=True, timeout=180)
    check("R/selfload.R:4: return from build_panel() => returned by build_panel(): id, value, date, self_loaded_created" in steps.stdout
          and 'R/panel.R:9: saveRDS(result, "output.rds") => written by saveRDS(): trailing_value_add' in steps.stdout,
          f"--steps fate lines unexpected:\n{steps.stdout}")

    # --diff: the data side of a change. A copy of the fixture under git; the
    # working tree stops creating `big` (which another file still reads and
    # also makes), adds `pct` from `share`, misspells `share`, and a new file
    # selects a column its summarise() dropped.
    repo = os.path.join(s.scratch, "colmap_diff")
    shutil.copytree(os.path.join(FIXTURES, "columns"), repo)
    os.remove(os.path.join(repo, ".here"))
    git = lambda *a: subprocess.run(["git", "-c", "user.email=t@t", "-c", "user.name=t", *a], cwd=repo, capture_output=True, text=True, check=True)  # noqa: E731
    git("init", "-q")
    git("add", "-A")
    git("commit", "-qm", "init")
    check("No R file differs from HEAD" in run("--diff", cwd=repo), "colmap --diff with no change should say so")
    ana = os.path.join(repo, "R", "analysis.R")
    with open(ana) as f:
        src = f.read()
    src = src.replace("ok3 <- sales |> select(region, year, amount) |> rename(amt = amount) |> mutate(big = amt > 5)",
                      "ok3 <- sales |> select(region, year, amount) |> rename(amt = amount)")
    src = src.replace("ok2 <- ok1 %>% mutate(share = total / sum(total)) %>% arrange(desc(share))",
                      "ok2 <- ok1 %>% mutate(share = total / sum(total), pct = share * 100) %>% arrange(desc(shar))")
    with open(ana, "w") as f:
        f.write(src)
    with open(os.path.join(repo, "R", "newfile.R"), "w") as f:
        f.write('library(dplyr)\nsales <- readr::read_csv("data/sales.csv")\n'
                'out <- sales |> group_by(region) |> summarise(n = n(), avg = mean(amount)) |> select(region, avg, customer)\n')
    diff = run("--diff", cwd=repo)
    check("changes in the working tree against HEAD" in diff.splitlines()[0]
          and "The diff falls in 7 pipeline steps in R/analysis.R, R/newfile.R, touching 10 columns of 1 source." in diff, f"colmap --diff header unexpected:\n{diff}")
    check("  pct                  created at R/analysis.R:11 (mutate), from share   not used anywhere else" in diff
          and "  share                created at R/analysis.R:11 (mutate), from total; used at R/analysis.R:11 (mutate)   also used at R/funcs.R:54 (filter)   feeds (by name): pct" in diff
          and "  avg                  created at R/newfile.R:3 (summarise), from amount; used at R/newfile.R:3 (select)   not used anywhere else" in diff,
          f"colmap --diff touched columns unexpected:\n{diff}")
    check("Columns the changed files created at HEAD and no longer create:\n  big                  was made at R/analysis.R:12 (mutate) of sales.csv   still used at R/funcs.R:31 (filter), R/funcs.R:39 (select)   also made at R/funcs.R:11 (mutate)" in diff,
          f"colmap --diff missed the column the diff stopped creating:\n{diff}")
    check("Findings on the changed lines:\n  R/analysis.R:11: 'shar' is not a column at this point" in diff and "did you mean share?" in diff
          and "R/newfile.R:3: 'customer' is not a column at this point" in diff, f"colmap --diff findings unexpected:\n{diff}")
    check("  customer             created at" not in diff, f"a reported mistake listed as a created column in --diff:\n{diff}")
    check('Sources the checker cannot open, used in the changed files: "data/scores.rds" (R/analysis.R:7)' in diff, f"colmap --diff unresolved sources unexpected:\n{diff}")
    dj = json.loads(run("--json", "--diff", cwd=repo))
    check(dj["base"] == "HEAD" and dj["steps"] == 7 and [g["column"] for g in dj["no_longer_created"]] == ["big"]
          and dj["no_longer_created"][0]["still_used_at"][0]["file"] == "R/funcs.R" and dj["no_longer_created"][0]["also_made_at"][0]["line"] == 11
          and any(c["column"] == "pct" and c["from"] == ["share"] for c in dj["sources"][0]["columns"])
          and [f["name"] for f in dj["findings"]] == ["shar", "customer"], f"colmap --diff JSON unexpected:\n{json.dumps(dj)[:800]}")
    against = run("--diff", "HEAD", cwd=repo)
    check("against HEAD" in against.splitlines()[0] and "big" in against, "colmap --diff REF unexpected")
    # --source NAME: one dataset; an overwrite is a site of its own
    one_src = run("--source", "sales.csv", "--file", "R/verbs.R")
    check("source sales.csv" in one_src.splitlines()[0] and "sales.csv   first used at R/verbs.R" in one_src and one_src.count("   first used at ") == 1,
          f"colmap --source unexpected:\n{one_src}")
    check("No source matches nothing." in run("--source", "nothing", "--file", "R/verbs.R"), "colmap --source with no match should say so")
    with open(os.path.join(repo, "R", "over.R"), "w") as f:
        f.write('library(dplyr)\nsales <- readr::read_csv("data/sales.csv")\nsales |> mutate(amount = amount * 2) |> select(amount)\n')
    ov = run("--file", "R/over.R", cwd=repo)
    check("  amount               used 2 times: R/over.R:3 (mutate), R/over.R:3 (select); overwritten at R/over.R:3 (mutate)" in ov,
          f"an overwrite is not listed as a site:\n{ov}")


def scenario_colcheck_cache(s):
    """Data the checker cannot open (an S3-style loader) is checked through the
    schema cache; unresolved frames are noted; `learn` fills the cache."""
    root = os.path.join(s.scratch, "s3columns")
    shutil.copytree(os.path.join(FIXTURES, "s3columns"), root)
    tool = os.path.join(REPO, "plugins", "r-lsp", "bin", "r-colcheck")
    report = os.path.join(root, "R", "report.R")
    with open(report) as f:
        src = f.read().splitlines()
    marked = lambda tag: {i for i, line in enumerate(src, 1) if line.rstrip().endswith(tag)}  # noqa: E731
    bad, later, noted = marked("# bad"), marked("# bad-after-learn"), marked("# note")

    def run_check():
        out = subprocess.run([tool, "--root", root, report], capture_output=True, text=True, timeout=120).stdout
        lines = [l for l in out.splitlines() if l.startswith("R/report.R:")]
        columns = {int(l.split(":")[1]) for l in lines if "is not a column" in l}
        notes = {int(l.split(":")[1]) for l in lines if "cannot open" in l}
        return out, columns, notes

    out, columns, notes = run_check()
    check(bad <= columns, f"cache-backed check missed lines {sorted(bad - columns)}:\n{out}")
    check(columns <= bad, f"cache-backed check flagged correct lines {sorted(columns - bad)}:\n{out}")
    check(notes == noted, f"unresolved-source notes expected on {sorted(noted)}, got {sorted(notes)}:\n{out}")
    check("days old" in out, f"no staleness note for the old cache entry:\n{out}")
    # the loader is a project function: the learn command brings its file along, and runs as printed
    check("Learn it once: r-colcheck learn \"holdings/latest\" 's3_read(\"holdings/latest\")' --source R/loaders.R" in out,
          f"the learn command must carry --source for a project loader:\n{out}")
    la = subprocess.run([tool, "learn", "--all", "--root", root], capture_output=True, text=True, timeout=180)
    check(la.returncode == 0 and 'learned  "holdings/latest": 3 columns' in la.stdout and "learned 1 of 1" in la.stdout,
          f"learn --all with the derived --source flag failed:\n{la.stdout}{la.stderr}")
    out, columns, notes = run_check()
    check(columns == bad | later and not notes, f"after learn --all expected {sorted(bad | later)}, got {sorted(columns)}:\n{out}")

    learned = subprocess.run([tool, "learn", "holdings/latest", 's3_read("holdings/latest")',
                              "--source", "R/loaders.R", "--root", root],
                             capture_output=True, text=True, timeout=120)
    check(learned.returncode == 0 and 'learned "holdings/latest": 3 columns' in learned.stdout,
          f"learn failed:\n{learned.stdout}\n{learned.stderr}")
    with open(os.path.join(root, ".colcheck", "schemas.json")) as f:
        cache = json.load(f)
    check([c["name"] for c in cache["holdings/latest"]["columns"]] == ["fund_id", "security", "weight"],
          f"cache entry wrong: {cache.get('holdings/latest')}")
    check(list(cache) == sorted(cache), "cache keys are not sorted")
    out, columns, notes = run_check()
    check(columns == bad | later, f"after learn expected {sorted(bad | later)}, got {sorted(columns)}:\n{out}")
    check(not notes, f"unresolved note persists after learn:\n{out}")
    nested = subprocess.run([tool, "learn", "nested/sample",
                             '{ x <- data.frame(id = 1); x$data <- list(data.frame(d = 1, r = 2)); x }'],
                            cwd=root, capture_output=True, text=True, timeout=120)
    check(nested.returncode == 0 and "inner columns recorded for list-column data" in nested.stdout,
          f"learn did not record inner columns:\n{nested.stdout}\n{nested.stderr}")
    with open(os.path.join(root, ".colcheck", "schemas.json")) as f:
        entry_cols = {c["name"]: c for c in json.load(f)["nested/sample"]["columns"]}
    check([c["name"] for c in entry_cols["data"].get("inner", [])] == ["d", "r"],
          f"cache entry lacks inner columns: {entry_cols}")
    listing = subprocess.run([tool, "schemas", "--root", root], capture_output=True, text=True, timeout=120).stdout
    check("holdings/latest  3 columns" in listing, f"schemas listing unexpected:\n{listing}")
    # learn on an arrow dplyr query records the query's output columns, not
    # the schema of the dataset it reads; a bare Schema keeps its types; an
    # arrow object that is not a table is refused
    have_arrow = rscript('cat(all(vapply(c("arrow", "dplyr"), function(p) nzchar(system.file(package = p)), TRUE)))').strip() == "TRUE"
    if have_arrow:
        def learn_arrow(key, expr):
            return subprocess.run([tool, "learn", key, expr, "--root", root], capture_output=True, text=True, timeout=180)
        aq = learn_arrow("arrow/query", 'arrow::arrow_table(id = 1:2, amount = c(1, 2), status = "a") |> dplyr::select(id, amount) |> dplyr::mutate(amount2 = amount * 2)')
        rn = learn_arrow("arrow/renamed", 'arrow::arrow_table(id = 1:2, amount = c(1, 2)) |> dplyr::rename(amt = amount)')
        sc = learn_arrow("arrow/schema", 'arrow::arrow_table(id = 1:2, amount = c(1, 2))$schema')
        check(aq.returncode == 0 and rn.returncode == 0 and sc.returncode == 0,
              f"learn on arrow expressions failed:\n{aq.stderr}{rn.stderr}{sc.stderr}")
        with open(os.path.join(root, ".colcheck", "schemas.json")) as f:
            arrow_cache = json.load(f)
        check([c["name"] for c in arrow_cache["arrow/query"]["columns"]] == ["id", "amount", "amount2"]
              and arrow_cache["arrow/query"]["method"] == "arrow_schema",
              f"learn recorded the arrow query's source, not its output: {arrow_cache['arrow/query']}")
        check([c["name"] for c in arrow_cache["arrow/renamed"]["columns"]] == ["id", "amt"],
              f"learn missed the arrow query's rename: {arrow_cache['arrow/renamed']}")
        check(arrow_cache["arrow/schema"]["method"] == "arrow_schema" and all(c.get("type") for c in arrow_cache["arrow/schema"]["columns"]),
              f"a bare arrow Schema lost its types: {arrow_cache['arrow/schema']}")
        junk = learn_arrow("arrow/junk", "arrow::ChunkedArray$create(1:3)")
        check(junk.returncode != 0 and "not a data frame" in junk.stderr,
              f"learn recorded an arrow array as a table:\n{junk.stdout}{junk.stderr}")
    # a literal inside a lambda is not a cache key
    lam = os.path.join(root, "R", "lambda.R")
    with open(lam, "w") as f:
        f.write("library(magrittr)\n"
                "x <- tibble::tibble(status = \"a\", r2 = 1)\n"
                "y <- x %>% purrr::map_dfr(~ .x %>% dplyr::filter(status == \"holdings/latest\"))\n"
                "y %>% dplyr::filter(zz == 1)\n"
                "z <- x %>% unknown_helper(function(d) d %>% dplyr::filter(status == \"holdings/latest\"))\n"
                "z %>% dplyr::filter(zz == 1)\n")
    out = subprocess.run([tool, "--root", root, lam], capture_output=True, text=True, timeout=120).stdout
    check("'zz'" not in out, f"a literal inside a lambda matched a cache key:\n{out}")
    # A wrapper around a cached loader (both pipes) is followed instead of
    # taking the loader's literal; a call naming two cached datasets is not
    # guessed from the first; 2L in the code matches 2 in a call-shaped key
    # and a named match beats a positional one; an option argument's value
    # is no key; `# cols:` is read across a multi-line call, in any case.
    with open(os.path.join(root, ".colcheck", "schemas.json")) as f:
        cache = json.load(f)
    yesterday = (datetime.date.today() - datetime.timedelta(days=1)).isoformat() + "T00:00:00Z"
    entry = lambda cols: {"expr": "x", "observed_at": yesterday, "method": "data_frame",  # noqa: E731
                          "columns": [{"name": c, "type": "character"} for c in cols]}
    cache['prepare(type = "monthly")'] = entry(["month", "mret"])
    cache['prepare(type = "monthly", lag = 2)'] = entry(["month", "mret", "lag2"])
    cache['prepare("daily")'] = entry(["day", "dret"])
    cache["p"] = entry(["pp"])
    with open(os.path.join(root, ".colcheck", "schemas.json"), "w") as f:
        json.dump(cache, f)
    with open(os.path.join(root, "R", "join.R"), "w") as f:
        f.write('s3_join <- function(a, b) dplyr::inner_join(s3_read(a), s3_read(b), by = "fund_id")\n')
    keys = os.path.join(root, "R", "keys.R")
    with open(keys, "w") as f:
        f.write('source("R/loaders.R")\nsource("R/join.R")\nlibrary(dplyr)\n'
                'clean <- function(d) d |> rename(w = weight)\n'
                'x <- clean(s3_read("holdings/latest"))\n'
                'x |> select(w)\n'
                'x |> select(weight) # bad\n'
                'y <- s3_read("holdings/latest") |> clean()\n'
                'y |> select(w)\n'
                'y |> select(weight) # bad\n'
                'j <- s3_join("returns/monthly", "holdings/latest")\n'
                'j |> select(weight, nope)\n'
                'm4 <- prepare(df, type = "monthly", lag = 2L)\n'
                'm4 |> select(lag2, nope) # bad\n'
                'm7 <- prepare(df, "daily", type = "monthly")\n'
                'm7 |> select(mret, nope) # bad\n'
                'o <- fetch_table("orders", mode = "p")\n'
                'o |> select(pp)\n'
                'a5 <- load_from_somewhere(\n  "x"\n)   # Cols: returns/monthly\n'
                'a5 |> select(fundid) # bad\n'
                'z <- s3_read(paste0("holdings/", "latest"))\n'
                'z |> select(wieght) # bad\n')
    out = subprocess.run([tool, "--root", root, keys], capture_output=True, text=True, timeout=120).stdout
    klines = [l for l in out.splitlines() if l.startswith("R/keys.R:")]
    got = {(int(l.split(":")[1]), l.split("'")[1]) for l in klines if "is not a column" in l}
    check(got == {(7, "weight"), (10, "weight"), (14, "nope"), (16, "nope"), (22, "fundid"), (24, "wieght")},
          f"cache key matching unexpected: {sorted(got)}\n{out}")
    ksrc = {int(l.split(":")[1]): l for l in klines if "is not a column" in l}
    check('cached schema "prepare(type = "monthly", lag = 2)"' in ksrc[14] and 'cached schema "prepare(type = "monthly")"' in ksrc[16]
          and 'cached schema "holdings/latest"' in ksrc[24] and 'cached schema "returns/monthly"' in ksrc[22],
          f"call-shaped or built keys resolved to the wrong entry:\n{out}")
    note_j = [l for l in klines if l.startswith("R/keys.R:11:")]
    check(note_j and "'j' comes from s3_join(\"returns/monthly\", \"holdings/latest\"), which names 2 cached datasets" in note_j[0]
          and "r-colcheck learn 's3_join(\"returns/monthly\", \"holdings/latest\")' 's3_join(\"returns/monthly\", \"holdings/latest\")' --source R/join.R --source R/loaders.R" in note_j[0],
          f"a call naming two cached datasets must get its own note:\n{out}")
    note_o = [l for l in klines if l.startswith("R/keys.R:17:")]
    check(note_o and 'no cached schema for key "orders"' in note_o[0] and "'pp'" not in out,
          f"an option argument's value matched a cache key:\n{out}")
    learned = subprocess.run([tool, "learn", 's3_join("returns/monthly", "holdings/latest")', 's3_join("returns/monthly", "holdings/latest")',
                              "--source", "R/join.R", "--source", "R/loaders.R", "--root", root], capture_output=True, text=True, timeout=120)
    check(learned.returncode == 0 and "5 columns" in learned.stdout, f"learning the call-shaped key the note suggests failed:\n{learned.stdout}{learned.stderr}")
    out = subprocess.run([tool, "--root", root, keys], capture_output=True, text=True, timeout=120).stdout
    check("R/keys.R:12: 'nope' is not a column" in out and 'cached schema "s3_join(' in out and "names 2 cached datasets" not in out,
          f"the call-shaped key did not resolve the join:\n{out}")
    # A function key answers calls without a literal only: a project function
    # of the same bare name as a cached "pkg::fn" entry is followed, not
    # matched, and a keyed loader whose literal has no entry stays
    # unresolved (with the note for that literal) instead of taking the
    # loader's bare key.
    with open(os.path.join(root, ".colcheck", "schemas.json")) as f:
        cache = json.load(f)
    cache["teamdata::default_rates"] = entry(["rate"])
    cache["s3_read"] = entry(["generic"])
    with open(os.path.join(root, ".colcheck", "schemas.json"), "w") as f:
        json.dump(cache, f)
    hj = os.path.join(root, "R", "hijack.R")
    with open(hj, "w") as f:
        f.write('source("R/loaders.R")\nlibrary(dplyr)\n'
                'default_rates <- function() s3_read("returns/monthly")\n'
                'd <- default_rates()\n'
                'd |> select(ret)\n'
                'd |> select(rate) # bad\n'
                'm <- s3_read("meta/funds")\n'
                'm |> select(name, generic)\n'
                'r <- s3_read("returns/monthly")\n'
                'r |> select(generic) # bad\n')
    out = subprocess.run([tool, "--root", root, hj], capture_output=True, text=True, timeout=120).stdout
    hgot = {(int(l.split(":")[1]), l.split("'")[1]) for l in out.splitlines() if l.startswith("R/hijack.R:") and "is not a column" in l}
    check(hgot == {(6, "rate"), (10, "generic")}, f"function keys hijacked a project function or a keyed loader call: {sorted(hgot)}\n{out}")
    check("R/hijack.R:7: 'm' comes from a source the checker cannot open" in out and 'no cached schema for key "meta/funds"' in out,
          f"a keyed loader call with an unlearned literal must get the note for that literal:\n{out}")
    # a local file that is there but cannot be read (an RDS over 20 MB) says why and falls back to the cache
    rscript('dir.create("data", showWarnings = FALSE); saveRDS(data.frame(a = runif(1.5e6), b = runif(1.5e6)), "data/big.rds", compress = FALSE)', cwd=root)
    big = os.path.join(root, "R", "big.R")
    with open(big, "w") as f:
        f.write('b <- readRDS("data/big.rds")\nb |> dplyr::select(zz)\n')
    out = subprocess.run([tool, "--root", root, big], capture_output=True, text=True, timeout=120).stdout
    check("big.rds is an RDS file of 23 MB, over the 20 MB the checker loads" in out
          and "r-colcheck learn \"data/big.rds\" 'readRDS(\"data/big.rds\")'" in out, f"unreadable local file note unexpected:\n{out}")
    lb = subprocess.run([tool, "learn", "data/big.rds", 'readRDS("data/big.rds")', "--root", root], capture_output=True, text=True, timeout=120)
    out = subprocess.run([tool, "--root", root, big], capture_output=True, text=True, timeout=120).stdout
    check(lb.returncode == 0 and "R/big.R:2: 'zz' is not a column" in out and 'cached schema "data/big.rds"' in out,
          f"the cache did not cover a local file the checker cannot read:\n{lb.stdout}{lb.stderr}{out}")
    # `# cols:` on a split/map statement survives the lambda walk, brace body with an assignment included
    sp = os.path.join(root, "R", "split.R")
    with open(sp, "w") as f:
        f.write("library(magrittr)\n"
                "x <- tibble::tibble(grp = \"a\", v = 1)\n"
                "b <- x %>% split(.$grp) %>% purrr::map(~ .x %>% dplyr::select(dplyr::all_of(unknown_cols))) %>% dplyr::bind_rows()   # cols: holdings/latest\n"
                "b %>% dplyr::filter(zz1 == 1)\n"
                "b2 <- x %>% split(.$grp) %>% purrr::map(function(d) { out <- d %>% dplyr::select(dplyr::all_of(unknown_cols)); out }) %>% dplyr::bind_rows()   # cols: holdings/latest\n"
                "b2 %>% dplyr::filter(zz2 == 1)\n")
    out = subprocess.run([tool, "--root", root, sp], capture_output=True, text=True, timeout=120).stdout
    check("'zz1' is not a column" in out and "'zz2' is not a column" in out and out.count('cached schema "holdings/latest"') == 2,
          f"# cols: on a split/map statement did not survive the lambda walk:\n{out}")
    # --load-all: the package at the root is loaded, its imports resolve
    pkg = s.make_package("loadall")
    with open(os.path.join(pkg, "R", "make_table.R"), "w") as f:
        f.write("make_table <- function() data.frame(a = 1, b = 2) %>% transform(c = a + b)\n")
    with open(os.path.join(pkg, "NAMESPACE"), "a") as f:
        f.write("importFrom(magrittr, \"%>%\")\n")
    with open(os.path.join(pkg, "DESCRIPTION")) as f:
        desc = f.read()
    with open(os.path.join(pkg, "DESCRIPTION"), "w") as f:
        f.write(desc.rstrip("\n") + "\nImports: magrittr\n")
    la = subprocess.run([tool, "learn", "tbl", "make_table()", "--load-all", "--root", pkg],
                        capture_output=True, text=True, timeout=180)
    check(la.returncode == 0 and 'learned "tbl": 3 columns' in la.stdout and "loaded with pkgload" in la.stdout,
          f"learn --load-all failed:\n{la.stdout}\n{la.stderr}")
    bare = subprocess.run([tool, "learn", "tbl2", "make_table()", "--root", pkg], capture_output=True, text=True, timeout=180)
    check(bare.returncode != 0, "learn without --load-all found the package function (test premise broken)")
    # refresh re-runs the stored expressions with the flags they were learned with
    with open(os.path.join(pkg, ".colcheck", "schemas.json")) as f:
        stored = json.load(f)
    check(stored["tbl"].get("load_all") is True, f"learn did not record --load-all on the entry:\n{stored['tbl']}")
    rf = subprocess.run([tool, "refresh", "--root", pkg], capture_output=True, text=True, timeout=180)
    check(rf.returncode == 0 and 'unchanged "tbl": 3 columns' in rf.stdout and "refreshed 1 of 1" in rf.stdout,
          f"refresh unexpected:\n{rf.stdout}{rf.stderr}")
    st = subprocess.run([tool, "refresh", "--stale", "--root", pkg], capture_output=True, text=True, timeout=180)
    check(st.returncode == 0 and "nothing to refresh" in st.stdout, f"refresh --stale on fresh entries unexpected:\n{st.stdout}{st.stderr}")
    with open(os.path.join(pkg, "R", "make_table.R"), "w") as f:
        f.write("make_table <- function() data.frame(a = 1, b = 2) %>% transform(c = a + b, d = 4)\n")
    stored["broken"] = {"expr": "stop('boom')", "observed_at": "2020-01-01T00:00:00Z", "method": "data_frame",
                        "columns": [{"name": "x", "type": "numeric"}]}
    with open(os.path.join(pkg, ".colcheck", "schemas.json"), "w") as f:
        json.dump(stored, f)
    rf2 = subprocess.run([tool, "refresh", "--root", pkg], capture_output=True, text=True, timeout=180)
    check(rf2.returncode == 1 and 'changed   "tbl": 4 columns, added d' in rf2.stdout
          and 'failed    "broken": evaluating the expression failed: boom' in rf2.stdout
          and "learn it again with its --load-all or --source flags" in rf2.stdout
          and "refreshed 1 of 2: 1 changed, 0 unchanged, 1 failed" in rf2.stdout,
          f"refresh with a change and a failure unexpected:\n{rf2.stdout}{rf2.stderr}")
    with open(os.path.join(pkg, ".colcheck", "schemas.json")) as f:
        after = json.load(f)
    check([c["name"] for c in after["tbl"]["columns"]] == ["a", "b", "c", "d"] and after["tbl"].get("load_all") is True
          and after["broken"]["observed_at"] == "2020-01-01T00:00:00Z",
          f"refresh wrote the cache wrong:\n{after}")
    one = subprocess.run([tool, "refresh", "nope", "--root", pkg], capture_output=True, text=True, timeout=180)
    check(one.returncode == 1 and 'no cached schema for "nope"' in one.stderr, f"refresh of a missing key unexpected:\n{one.stderr}")

    hook = os.path.join(REPO, "plugins", "r-lsp", "hooks", "post-edit.sh")
    ctx = hook_context(run_hook(hook, {"tool_name": "Edit", "tool_input": {"file_path": report}, "cwd": root},
                                env={"CLAUDE_PROJECT_DIR": root}))
    check("'wieght'" in ctx and "r-colcheck learn" in ctx, f"post-edit hook context missing cache findings:\n{ctx[:600]}")

    fresh = s.make_project("s3fresh", {"R/loaders.R": os.path.join(FIXTURES, "s3columns", "R", "loaders.R")})
    first = subprocess.run([tool, "learn", "meta/funds", 's3_read("meta/funds")', "--source", "R/loaders.R"],
                           cwd=os.path.join(fresh, "R"), capture_output=True, text=True, timeout=120)
    check(first.returncode == 0, f"learn from a subdirectory failed:\n{first.stderr}")
    # --source relative to the shell's directory works too
    rel_src = subprocess.run([tool, "learn", "meta/funds2", 's3_read("meta/funds")', "--source", "loaders.R"],
                             cwd=os.path.join(fresh, "R"), capture_output=True, text=True, timeout=120)
    check(rel_src.returncode == 0, f"learn with --source relative to the shell's directory failed:\n{rel_src.stderr}")
    # wrong arguments exit 2, a cache that does not parse stops the run instead of being overwritten
    check(subprocess.run([tool, "learn", "only-a-key", "--root", fresh], capture_output=True, text=True, timeout=60).returncode == 2, "learn with one argument should exit 2")
    check(subprocess.run([tool, "schemas", "extra", "--root", fresh], capture_output=True, text=True, timeout=60).returncode == 2, "schemas with an argument should exit 2")
    cache_file = os.path.join(fresh, ".colcheck", "schemas.json")
    with open(cache_file) as f:
        good = f.read()
    with open(cache_file, "w") as f:
        f.write(good.rstrip().rstrip("}") + ",}")
    broken = subprocess.run([tool, "learn", "k2", "data.frame(a = 1)", "--root", fresh], capture_output=True, text=True, timeout=120)
    with open(cache_file) as f:
        still = f.read()
    check(broken.returncode == 1 and "cannot parse .colcheck/schemas.json" in broken.stderr and still.endswith(",}"),
          f"a corrupt cache was overwritten or not reported:\n{broken.stdout}{broken.stderr}")
    with open(cache_file, "w") as f:
        f.write(good)
    # a field the writer does not know survives a rewrite
    data_ = json.loads(good)
    data_["meta/funds"]["note"] = "kept"
    with open(cache_file, "w") as f:
        json.dump(data_, f)
    subprocess.run([tool, "learn", "k3", "data.frame(a = 1)", "--root", fresh], capture_output=True, text=True, timeout=120)
    with open(cache_file) as f:
        check(json.load(f)["meta/funds"].get("note") == "kept", "a hand-written field in the cache was dropped by learn")
    gitignore = os.path.join(fresh, ".colcheck", ".gitignore")
    check(os.path.exists(gitignore) and open(gitignore).read().strip() == "*",
          "a new .colcheck/ directory did not get its own .gitignore")
    check(os.path.exists(os.path.join(fresh, ".colcheck", "schemas.json")),
          "learn run from a subdirectory did not write the cache at the project root")

    # In a renv project, learn must run inside the project's .Rprofile so the
    # loader sees the renv library (the checker itself runs with --vanilla).
    renv_root = s.make_renv_project("s3renv", {"R/renv_probe.R": os.path.join(FIXTURES, "s3columns", "R", "renv_probe.R")})
    probe = subprocess.run([tool, "learn", "probe", "probe()", "--source", "R/renv_probe.R", "--root", renv_root],
                           capture_output=True, text=True, timeout=120)
    check(probe.returncode == 0, f"learn in a renv project failed:\n{probe.stderr}")
    with open(os.path.join(renv_root, ".colcheck", "schemas.json")) as f:
        probe_cols = [c["name"] for c in json.load(f)["probe"]["columns"]]
    check(probe_cols == ["renv_on"], f"learn ran without renv active: columns {probe_cols}")

    # The loader registry in .colcheck.json: a registered loader is keyed on
    # its key argument, the note gives the learn command the registry
    # derives, a parquet on a bucket is learned from its footer, `discover`
    # lists every uncached source and `learn --all` learns them in one run.
    reg = os.path.join(s.scratch, "s3registry")
    shutil.copytree(os.path.join(FIXTURES, "s3columns"), reg)
    with open(os.path.join(reg, ".colcheck.json"), "w") as f:
        json.dump({"loaders": [{"fn": "s3_read", "key": 1, "learn": "s3_read(KEY)", "source": ["R/loaders.R"]},
                               {"fn": "fetch_table", "key": "name", "learn": "s3_read(KEY)", "source": ["R/loaders.R"]}],
                   "schema_ttl_days": 30}, f)
    with open(os.path.join(reg, "R", "loaders.R"), "a") as f:
        f.write("fetch_table <- function(date, name) s3_read(name)\n")
    more = os.path.join(reg, "R", "more.R")
    with open(more, "w") as f:
        f.write('source("R/loaders.R")\nlibrary(dplyr)\nh <- fetch_table("2024-01", name = "holdings/latest")\nh2 <- h |> filter(wieght > 0)\n'
                'p <- arrow::read_parquet("s3://my-bucket/prices/2024.parquet")\np2 <- p |> filter(px > 0)\n')
    out = subprocess.run([tool, "--root", reg, more], capture_output=True, text=True, timeout=120).stdout
    check('no cached schema for key "holdings/latest"' in out
          and 'r-colcheck learn "holdings/latest" \'s3_read("holdings/latest")\' --source R/loaders.R (from the loader registry' in out,
          f"registered loader note unexpected (key argument, derived command):\n{out}")
    check('r-colcheck learn "s3://my-bucket/prices/2024.parquet" \'arrow::open_dataset("s3://my-bucket/prices/2024.parquet")\'' in out
          and "reads the schema without the rows" in out, f"remote parquet note should suggest arrow::open_dataset:\n{out}")
    disc = subprocess.run([tool, "discover", "--root", reg], capture_output=True, text=True, timeout=180)
    check(disc.returncode == 0 and "2 data sources the checker cannot open, used in 3 places" in disc.stdout
          and '"holdings/latest"   from fetch_table("2024-01", name = "holdings/latest")   at R/more.R:3, R/report.R:17' in disc.stdout
          and '"returns/monthly"   3 columns, observed 2025-01-15' in disc.stdout and ", stale)" in disc.stdout
          and "r-colcheck refresh --stale" in disc.stdout and "r-colcheck learn --all" in disc.stdout,
          f"discover output unexpected:\n{disc.stdout}{disc.stderr}")
    dry = subprocess.run([tool, "learn", "--all", "--dry-run", "--root", reg], capture_output=True, text=True, timeout=180)
    check(dry.returncode == 0 and dry.stdout.count("r-colcheck learn ") == 2 and "2 source(s) would be learned" in dry.stdout,
          f"learn --all --dry-run unexpected:\n{dry.stdout}{dry.stderr}")
    with open(os.path.join(reg, ".colcheck", "schemas.json")) as f:
        check("holdings/latest" not in f.read(), "test premise: holdings/latest cached before learn --all")
    # the bucket is not reachable from here: one learned, one failed, exit 1
    la = subprocess.run([tool, "learn", "--all", "--root", reg], capture_output=True, text=True, timeout=300)
    check(la.returncode == 1 and 'learned  "holdings/latest": 3 columns' in la.stdout
          and 'failed   "s3://my-bucket/prices/2024.parquet"' in la.stdout and "learned 1 of 2" in la.stdout,
          f"learn --all unexpected:\n{la.stdout}{la.stderr}")
    out = subprocess.run([tool, "--root", reg, more], capture_output=True, text=True, timeout=120).stdout
    check("R/more.R:4: 'wieght' is not a column" in out and 'cached schema "holdings/latest"' in out,
          f"a registered loader's key argument did not resolve after learn --all:\n{out}")


def scenario_colcheck_packages(s):
    """Data that passes through an installed package's cleaning function: the
    checker reads the function's body (fixture package `teamdata`, installed
    into the project's renv library) and checks the columns that come out."""
    yesterday = (datetime.date.today() - datetime.timedelta(days=1)).isoformat() + "T00:00:00Z"

    def entry(table, columns):
        return {"expr": f"teamdata::fetch_table(Sys.Date(), \"{table}\")", "observed_at": yesterday,
                "method": "data_frame", "columns": [{"name": c, "type": "character"} for c in columns]}
    schemas = json.dumps({"orders": entry("orders", ["id", "customer_id", "amount", "status", "extra"]),
                          "customers": entry("customers", ["customer_id", "region"]),
                          # keyed on the function: no literal in the call, no comment on the line
                          "teamdata::taxonomy": entry("taxonomy", ["product", "category"]),
                          # keyed on the function and an argument value: the output depends on it
                          "teamdata::taxonomy(level = \"fine\")": entry("taxonomy", ["product", "category", "subcategory"]),
                          "fixed_table": dict(entry("fixed", ["code", "label"]),
                                              columns=[{"name": "code", "type": "character"}, {"name": "label", "type": "character"},
                                                       {"name": "series", "type": "list",
                                                        "inner": [{"name": "d", "type": "Date"}, {"name": "r", "type": "double"}]}])})
    script = (
        "library(teamdata)\n"
        "raw <- teamdata::fetch_table(Sys.Date(), \"orders\")\n"
        "customers <- teamdata::fetch_table(Sys.Date(), \"customers\")\n"
        "orders <- teamdata::clean_orders(raw, customers)\n"
        "ok <- orders %>% dplyr::mutate(big = total > 100)\n"
        "bad1 <- orders %>% dplyr::mutate(z = amount)\n"
        "bad2 <- orders %>% dplyr::select(idd)\n"
        "orders2 <- clean_orders(raw, customers)\n"
        "bad3 <- orders2 %>% dplyr::filter(extra > 1)\n"
        "b <- teamdata::opaque(raw)\n"
        "unknown <- b %>% dplyr::mutate(q = nothing)\n"
        "r <- teamdata::fetch_table(Sys.Date(), \"returns\")\n"
        "r2 <- r %>% dplyr::mutate(x = y)\n"
        # `# cols:` names the assigned expression, not the loader's first argument
        "tagged <- teamdata::fetch_table(Sys.Date(), \"nightly\") # cols: customers\n"
        "bad4 <- tagged %>% dplyr::select(regionn)\n"
        # a loader inside a followed function hits the cache by its own literal
        "inner <- teamdata::load_clean(Sys.Date())\n"
        "bad5 <- inner %>% dplyr::select(id, amount, status)\n"
        # reassignment: every step reads the old frame
        "orders <- orders %>% dplyr::filter(total > 0) %>% dplyr::select(id)\n"
        "ok2 <- orders %>% dplyr::select(id)\n"
        "tx <- teamdata::taxonomy(raw, customers)\n"
        "bad6 <- tx %>% dplyr::select(product, categry)\n"
        "ft <- fixed_table()\n"
        "bad7 <- ft %>% dplyr::select(code, labell)\n"
        # a function from a public (CRAN-style) package: unreadable, but no note
        "rates <- cranlike::pull_rates()\n"
        "r3 <- rates %>% dplyr::mutate(z = y)\n"
        # a pipeline from a cached loader ending in a vector: checked, no note
        "ids <- teamdata::fetch_table(Sys.Date(), \"orders\") %>% dplyr::pull(id)\n"
        "cnt <- raw %>% dplyr::filter(status == \"open\") %>% nrow()\n"
        "bad8 <- raw %>% dplyr::pull(idd)\n"
        # a cached source, then a step that loses the columns: the note names the step
        "nested <- teamdata::taxonomy(raw, customers) %>% dplyr::filter(category != \"x\") %>% tidyr::unnest(product)\n"
        "nested %>% dplyr::select(zz)\n"
        # a list-column with recorded inner columns: unnest() and hoist() are followed
        "ser <- fixed_table() %>% dplyr::filter(code != \"x\") %>% tidyr::unnest(series)\n"
        "bad9 <- ser %>% dplyr::select(code, d, rr)\n"
        "h <- fixed_table() %>% tidyr::hoist(series, first = \"d\", second = \"qq\")\n"
        "h %>% dplyr::select(first, second, code, serie)\n"
        # a key with an argument beats the bare function key; a piped helper hits the cache too
        "tx2 <- teamdata::taxonomy(raw, customers, level = \"fine\")\n"
        "bad10 <- tx2 %>% dplyr::select(subcategory, categry)\n"
        "tx3 <- raw %>% teamdata::taxonomy(customers)\n"
        "bad11 <- tx3 %>% dplyr::select(product, categry)\n")
    root = s.make_renv_project("colcheck_pkgs", {}, extra_packages=("teamdata", "cranlike"))
    for rel, text in (("script/run.R", script), (".colcheck/schemas.json", schemas)):
        os.makedirs(os.path.dirname(os.path.join(root, rel)), exist_ok=True)
        with open(os.path.join(root, rel), "w") as f:
            f.write(text)
    tool = os.path.join(REPO, "plugins", "r-lsp", "bin", "r-colcheck")
    out = subprocess.run([tool, "--json", "script/run.R"], cwd=root, capture_output=True, text=True, timeout=300)
    check(out.returncode == 1, f"r-colcheck exited {out.returncode} (expected 1, findings):\n{out.stderr}")
    findings = json.loads(out.stdout)
    cols = {(f["line"], f["name"]) for f in findings if f["kind"] == "column"}
    check(cols == {(6, "amount"), (7, "idd"), (9, "extra"), (15, "regionn"), (17, "status"), (21, "categry"), (23, "labell"), (28, "idd"), (32, "rr"), (33, "qq"), (34, "serie"), (36, "categry"), (38, "categry")},
          f"column findings unexpected: {sorted(cols)}\n{out.stdout}")
    src = {f["line"]: f["source"] for f in findings if f["kind"] == "column"}
    check(src[6] == src[9] == "teamdata::clean_orders() (read from its code)", f"finding source unexpected: {src}")
    check(src[15].startswith("cached schema \"customers\""), f"annotated loader source unexpected: {src[15]}")
    check(src[17] == "teamdata::load_clean() (read from its code)", f"inner-loader source unexpected: {src[17]}")
    check(src[21].startswith("cached schema \"teamdata::taxonomy\"") and src[23].startswith("cached schema \"fixed_table\""),
          f"function-keyed cache sources unexpected: {src[21]} / {src[23]}")
    check(src[32].startswith("cached schema \"fixed_table\""), f"unnested frame lost its source: {src[32]}")
    check(src[36].startswith('cached schema "teamdata::taxonomy(level = "fine")"') and src[38].startswith('cached schema "teamdata::taxonomy", '),
          f"call-shaped key / piped cache sources unexpected: {src[36]} / {src[38]}")
    check(not [f for f in findings if f["kind"] == "error"], f"checker errors:\n{out.stdout}")
    notes = {f["line"]: f["message"] for f in findings if f["kind"] == "note"}
    check(sorted(notes) == [10, 12, 29], f"notes unexpected (a CRAN-style package and a pull() pipeline must get none):\n{notes}")
    check("'nested' is known up to tidyr::unnest(product) on line 29" in notes[29]
          and "list-column" in notes[29] and "teamdata::taxonomy" not in notes[29],
          f"the note for a mid-pipeline stop must name the step, not the source:\n{notes[29]}")
    check("teamdata::opaque()" in notes[10] and "could not be read from its code" in notes[10]
          and "r-colcheck learn \"teamdata::opaque\" '<an expression that returns its table>'" in notes[10],
          f"opaque-function note unexpected:\n{notes[10]}")
    # A self-contained package loader with a literal key keeps the learn suggestion.
    check("r-colcheck learn \"returns\" 'teamdata::fetch_table(Sys.Date(), \"returns\")'" in notes[12],
          f"uncached loader note lost its learn command:\n{notes[12]}")
    check("23 of 27 pipeline step(s) checked" in out.stderr and "4 on unknown" in out.stderr
          and "checker error" not in out.stderr, f"summary unexpected:\n{out.stderr}")
    # Switched off, the package function is opaque and nothing after it is checked.
    off = subprocess.run([tool, "--json", "script/run.R"], cwd=root, capture_output=True, text=True, timeout=300,
                         env=dict(os.environ, R_LSP_COLCHECK_PACKAGES="false"))
    check(off.returncode == 1 and "r-colcheck: 15 of " in off.stderr,
          f"R_LSP_COLCHECK_PACKAGES=false still followed the package function:\n{off.stderr}")
    off_cols = {(f["line"], f["name"]) for f in json.loads(off.stdout) if f["kind"] == "column"}
    check(off_cols == {(15, "regionn"), (21, "categry"), (23, "labell"), (28, "idd"), (32, "rr"), (33, "qq"), (34, "serie"), (36, "categry"), (38, "categry")},
          f"with packages off, only the cache should answer: {sorted(off_cols)}")

def scenario_lint_fallback(s):
    """A project .lintr replaces the plugin's profile in the server (lintr
    takes the first config it finds), so the post-edit hook runs the bug
    linters itself and the session-start context says so."""
    root = s.make_package("lintfb")
    lintr_cfg = ("linters: linters_with_defaults(\n    line_length_linter(120L),\n"
                 "    pipe_consistency_linter(pipe = \"%>%\"),\n    object_usage_linter = NULL,\n"
                 "    object_length_linter = NULL,\n    commented_code_linter = NULL\n  )\n")
    probe = os.path.join(root, "R", "probe.R")
    with open(probe, "w") as f:
        f.write("x <- stats::not_real_function(1)\ny <- misspelled_fn(2)\nz <- 3\n")
    hook = os.path.join(REPO, "plugins", "r-lsp", "hooks", "post-edit.sh")
    start = os.path.join(REPO, "plugins", "r-lsp", "hooks", "session-start.sh")
    payload = {"tool_name": "Edit", "tool_input": {"file_path": probe}, "cwd": root}
    env = {"CLAUDE_PROJECT_DIR": root}

    # No project config: the server's profile applies, the hook adds nothing.
    out = run_hook(hook, payload, env=env)
    check(out.strip() == "", f"hook ran the bug linters although no .lintr exists:\n{out}")
    check("Lint profile inactive" not in run_hook(start, {}, env=env, stdin_text="{}"),
          "session-start reported an inactive profile without a .lintr")

    with open(os.path.join(root, ".lintr"), "w") as f:
        f.write(lintr_cfg)
    ctx = hook_context(run_hook(hook, payload, env=env))
    check("bug linters on probe.R" in ctx and ".lintr takes precedence" in ctx, f"hook note missing the fallback section:\n{ctx}")
    check("R/probe.R:1:13: 'not_real_function' is not exported from {stats}. [namespace_linter]" in ctx,
          f"namespace lint missing from the hook note:\n{ctx}")
    check('R/probe.R:2:6: could not find function "misspelled_fn" [undefined_function_linter]' in ctx,
          f"undefined-function lint missing from the hook note:\n{ctx}")
    check("could not run" not in ctx, f"the profile failed to load in the hook:\n{ctx}")
    if rscript('cat(requireNamespace("dplyr", quietly = TRUE))').strip() == "TRUE":
        with open(probe, "a") as f:
            f.write("w <- dplyr::summarise_each(data.frame(a = 1), dplyr::funs(mean))\n")
        ctx = hook_context(run_hook(hook, payload, env=env))
        check("summarise_each" in ctx and ("deprecated" in ctx or "superseded" in ctx or "defunct" in ctx),
              f"lifecycle lint missing from the hook note:\n{ctx}")
        # dplyr 1.2 made it defunct: lintr types that "error", which the hook
        # once dropped along with parse errors (now only lintr's own parse
        # pseudo-lint, linter "error", is left out).
        dplyr = tuple(int(x) for x in rscript('cat(as.character(packageVersion("dplyr")))').strip().split(".")[:2])
        if dplyr >= (1, 2):
            check("summarise_each() is defunct" in ctx, f"the defunct call (an error-type lint) is missing from the hook note:\n{ctx}")
        with open(probe, "a") as f:
            f.write("v <- (\n")
        ctx = hook_context(run_hook(hook, payload, env=env))
        check(ctx.count("unexpected end of input") == 1 and "[error]" not in ctx,
              f"a parse error should be reported once, by r-colcheck, not also as a lint:\n{ctx}")
        with open(probe) as f:
            kept = f.read().replace("v <- (\n", "")
        with open(probe, "w") as f:
            f.write(kept)
    started = run_hook(start, {}, env=env, stdin_text="{}")
    check("Lint profile inactive" in started and ".lintr takes precedence" in started and "post-edit hook" in started,
          f"session-start did not report the inactive profile:\n{started}")

    # A .lintr.R and a config under .github/linters/ win the same way.
    os.remove(os.path.join(root, ".lintr"))
    os.makedirs(os.path.join(root, ".github", "linters"), exist_ok=True)
    with open(os.path.join(root, ".github", "linters", ".lintr"), "w") as f:
        f.write(lintr_cfg)
    ctx = hook_context(run_hook(hook, payload, env=env))
    check(".github/linters/.lintr takes precedence" in ctx and "[namespace_linter]" in ctx,
          f"a .github/linters/.lintr was not treated as the winning config:\n{ctx}")
    shutil.rmtree(os.path.join(root, ".github"))
    check(run_hook(hook, payload, env=env).strip() == "", "hook kept running the bug linters after the config was removed")


def scenario_callmap(s):
    """r-callmap: the static call map of a package, its neighbourhood view and
    JSON, with no server. The chain avoids one-letter names: a project function
    called `c` would rightly attract every c() call in the fixture."""
    root = os.path.join(s.scratch, "callmap")
    files = {
        "DESCRIPTION": "Package: callmapfix\nTitle: Fixture\nVersion: 0.0.1\nDescription: r-callmap fixture.\nLicense: MIT\n",
        "NAMESPACE": "export(outer_step)\nexport(run_all)\nexport(Account)\nS3method(print, myclass)\n",
        # R6: Class$new() reaches initialize (up the inherit chain), self$/
        # private$/super$ and an object made in the same function resolve.
        "R/account.R": ("Account <- R6::R6Class(\"Account\",\n  public = list(\n    balance = 0,\n"
                        "    initialize = function(balance = 0) {\n      self$balance <- balance\n      private$audit(\"init\")\n    },\n"
                        "    deposit = function(x) {\n      self$balance <- self$balance + validate_amount(x)\n"
                        "      private$audit(\"deposit\")\n      invisible(self)\n    }\n  ),\n"
                        "  private = list(audit = function(what) message(what)),\n"
                        "  active = list(pretty = function() paste0(\"$\", self$balance))\n)\n\n"
                        "Savings <- R6::R6Class(\"Savings\", inherit = Account,\n"
                        "  public = list(accrue = function(rate) {\n    super$deposit(self$balance * rate)\n    self$deposit(0)\n  })\n)\n\n"
                        "validate_amount <- function(x) {\n  stopifnot(is.numeric(x))\n  x\n}\n\n"
                        "open_account <- function() {\n  acct <- Account$new(10)\n  acct$deposit(5)\n  acct$pretty\n}\n"),
        # Dotted names: an S3 method needs a registered or project generic, or
        # a base generic with a class the code names; the rest are functions.
        "R/dotted.R": ("summary.stats <- function(x) mean(x)\nt.test_wrapper <- function(x) x\n"
                       "split.by <- function(df, g) split(df, df[[g]])\n"
                       "format.money <- function(x, ...) paste0(\"$\", unclass(x))\n"
                       "new_money <- function(x) structure(x, class = \"money\")\n"),
        # Aliases and lists of functions, a memoised literal, a string held in
        # a local or pasted from literals, a comment naming the callee.
        "R/values.R": ("twice_helper <- helper\nhelpers <- list(h = helper, byname = by_string)\n"
                       "memo_helper <- memoise::memoise(function(x) helper(x) + 1)\n"
                       "use_alias <- function(x) twice_helper(x)\n"
                       "use_registry <- function(x) helpers$h(x) + helpers[[\"byname\"]](list(x))\n"
                       "use_memo <- function(x) memo_helper(x)\n\n"
                       "pinned <- function(xs) {\n  lapply(\n    # map helper over each item\n    xs, helper)\n}\n\n"
                       "by_const <- function(xs) {\n  fname <- \"by_string\"\n  do.call(fname, list(xs))\n}\n"
                       "by_paste <- function(xs) {\n  f <- match.fun(paste0(\"by_\", \"string\"))\n  f(xs)\n}\n"),
        # A testthat helper with a setup statement and a fixture built at the
        # top is shared with the test files, like a file of definitions.
        "tests/testthat/helper-fixtures.R": ("withr::local_options(list(digits = 3))\nfixture <- by_string(list(1, 2))\n"
                                             "check_by <- function(x) testthat::expect_equal(by_string(x), 3)\n"),
        # Outside R/: a definitions file loaded by a loop is shared wherever it
        # lives (a bare setClass() does not make it a script); a function
        # defined in a script is private to the script and its source()rs.
        "analysis/lib/tools.R": "setClass(\"Thing\", representation(x = \"numeric\"))\ntool_fn <- function(x) helper(x)\n",
        "analysis/main.R": ("for (p in list.files(\"analysis/lib\", full.names = TRUE)) source(p)\n"
                            "main_only <- function() tool_fn(2)\nprint(main_only())\n"),
        "analysis/other.R": "main_only()\ntool_fn(3)\ntar_read(total)\n",
        # targets: a command naming another target depends on it; a target is
        # pinned to its own tar_target() line, not the first mention of its name.
        "_targets.R": ("library(targets)\ntar_source(\"R\")\nlist(\n  tar_target(raw, helper(1)),\n"
                       "  tar_target(report, describe(total)),\n  tar_target(total, by_string(list(raw)))\n)\n"),
        "R/chain.R": ("outer_step <- function(x, n = 1) {\n  y <- middle_step(x)\n  y + n\n}\n\n"
                      "middle_step <- function(x) {\n  inner_step(x) * 2\n}\n\n"
                      "inner_step <- function(x) {\n  x + 1\n}\n"),
        "R/apply.R": ("run_all <- function(xs, how = \"fast\") {\n  out <- lapply(xs, helper)\n"
                      "  do.call(\"by_string\", list(out))\n}\n\nhelper <- function(x) x * 3\n\n"
                      "by_string <- function(parts) {\n  Reduce(`+`, parts)\n}\n\n"
                      "wrap <- function(x, inner_step) {\n  lapply(x, inner_step)\n}\n"),
        "R/print.R": ("describe <- function(obj, ...) {\n  UseMethod(\"describe\")\n}\n\n"
                      "describe.myclass <- function(obj, ...) {\n  helper(obj$value)\n}\n\n"
                      "print.myclass <- function(x, ...) {\n  cat(\"myclass\", describe(x), \"\\n\")\n  invisible(x)\n}\n\n"
                      "orphan <- function() {\n  \"nobody calls me\"\n}\n"),
        "tests/testthat/test-a.R": ("test_that(\"outer_step works\", {\n  expect_equal(outer_step(1), 5)\n})\n"
                                    "test_that(\"savings\", {\n  a <- Savings$new(1)\n  a$accrue(0.5)\n"
                                    "  expect_equal(a$balance, 1.5)\n  check_by(fixture)\n})\n"),
        "scripts/run.R": "source(\"R/apply.R\")\nres <- run_all(list(1, 2))\nprint(res)\n",
        # Chunks are read as knitr runs them: prose and a python chunk are not
        # calls, a chunk that does not parse is skipped on its own, a function
        # defined in a chunk belongs to the vignette.
        "vignettes/intro.Rmd": ("---\ntitle: x\n---\n\nProse that mentions middle_step() and `inner_step(2)` is not a call.\n\n"
                                "```{r setup, include=FALSE}\nlibrary(callmapfix)\n```\n\n```{python}\nouter_step(0)\n```\n\n"
                                "```{r}\nouter_step(1)\n```\n\n```{r broken}\nmiddle_step(\n```\n\n"
                                "```{r helpers}\nvig_helper <- function(x) inner_step(x)\nvig_helper(2)\n```\n"),
    }
    for rel, text in files.items():
        dest = os.path.join(root, rel)
        os.makedirs(os.path.dirname(dest), exist_ok=True)
        with open(dest, "w") as f:
            f.write(text)
    tool = os.path.join(REPO, "plugins", "r-lsp", "bin", "r-callmap")

    def run(*args, cwd=None):
        out = subprocess.run([tool, *args], cwd=cwd or root, capture_output=True, text=True, timeout=120)
        check(out.returncode == 0, f"r-callmap {' '.join(args)} exited {out.returncode}:\n{out.stderr}")
        return out.stdout

    # From a subdirectory with no --root: the wrapper finds the package root.
    full = run(cwd=os.path.join(root, "R"))
    lines = full.splitlines()
    check("static call map of" in lines[0] and "36 functions and 3 targets in 14 files, 1 of them Rmd/qmd" in lines[0],
          f"header unexpected:\n{full[:600]}")
    rmd = os.path.join(root, "vignettes", "intro.Rmd")
    broken_chunk = line_of(rmd, "```{r broken}") + 1
    check(any(l == f"Skipped: vignettes/intro.Rmd: the R chunk starting at line {broken_chunk} does not parse: "
              f"{broken_chunk + 1}:0: unexpected end of input" for l in lines),
          f"the broken chunk is not named as skipped:\n{full[:800]}")

    def def_line(rel, sig):
        loc = f"{rel}:{line_of(os.path.join(root, rel), sig.split('(')[0] + ' <- function')}"
        hit = [l for l in lines if l.strip().startswith(loc + " ") and sig in l]
        check(len(hit) == 1, f"no definition line '{loc} {sig}':\n{full}")
        return hit[0]

    outer = def_line("R/chain.R", "outer_step(x, n)")
    middle = def_line("R/chain.R", "middle_step(x)")
    inner = def_line("R/chain.R", "inner_step(x)")
    run_all = def_line("R/apply.R", "run_all(xs, how)")
    helper = def_line("R/apply.R", "helper(x)")
    by_string = def_line("R/apply.R", "by_string(parts)")
    wrap = def_line("R/apply.R", "wrap(x, inner_step)")
    method = def_line("R/print.R", "print.myclass(x, ...)")
    vig = def_line("vignettes/intro.Rmd", "vig_helper(x)")
    check("calls: middle_step" in outer and "[exported]" in outer and "called by: tests/testthat/test-a.R, vignettes/intro.Rmd" in outer,
          f"outer_step line unexpected:\n{outer}")
    check("calls: inner_step" in middle and "called by: outer_step" in middle and "[exported]" not in middle
          and "intro.Rmd" not in middle, f"middle_step line unexpected (prose and a python chunk are not calls):\n{middle}")
    check("called by: middle_step, vig_helper" in inner and "wrap" not in inner,
          f"inner_step line unexpected (a parameter is not the function):\n{inner}")
    check("calls: inner_step" in vig and "called by: vignettes/intro.Rmd" in vig, f"vignette-defined function unexpected:\n{vig}")
    check(any(l.startswith("vignettes/intro.Rmd") and "top-level code calls: outer_step, vig_helper" in l for l in lines),
          f"vignette heading unexpected:\n{full}")
    check("helper (as value)" in run_all and "by_string (by string)" in run_all and "[exported]" in run_all
          and "called by: scripts/run.R" in run_all, f"run_all line unexpected:\n{run_all}")
    check("run_all (as value)" in helper and "describe.myclass" in helper, f"helper line unexpected:\n{helper}")
    check("called by: run_all (by string)" in by_string, f"by_string line unexpected:\n{by_string}")
    check("calls:" not in wrap and "called by:" not in wrap, f"wrap line unexpected:\n{wrap}")
    check("[S3 method of print]" in method and "calls: describe" in method, f"S3 method line unexpected:\n{method}")
    check("[S3 method of describe]" in def_line("R/print.R", "describe.myclass(obj, ...)"), "describe.myclass not tagged as an S3 method")
    check(any(l.startswith("scripts/run.R") and "sources: R/apply.R" in l and "run_all" in l for l in lines),
          f"script heading unexpected:\n{full}")
    entry = full.split("Entry points (not called by any project function):")[1].split("Unreferenced")[0]
    unref = full.split("Unreferenced (no callers anywhere):")[1].split("\n\n")[0]
    check("outer_step [exported]" in entry and "run_all [exported]" in entry and "middle_step" not in entry
          and "vig_helper" in entry, f"entry points unexpected:\n{entry}")
    check("orphan" in unref and "wrap" in unref and "print.myclass" not in unref and "outer_step" not in unref
          and "vig_helper" not in unref, f"unreferenced unexpected:\n{unref}")
    check("Recursion/cycles" not in full, f"cycles reported for an acyclic fixture:\n{full}")
    check("[private to this file]" in vig and "[private to this file]" not in inner, f"the private tag is for a script's definitions:\n{vig}\n{inner}")

    def row(rel, sig, anchor):
        """The listing line of the definition `sig` whose line holds `anchor` in `rel`."""
        loc = f"{rel}:{line_of(os.path.join(root, rel), anchor)}"
        hit = [l for l in lines if l.strip().startswith(loc + " ") and f"  {sig}" in l]
        check(len(hit) == 1, f"no definition line '{loc} {sig}':\n{full}")
        return hit[0]

    # Definitions outside R/ (CM-B1): a loop-loaded lib file and a testthat
    # helper with top-level statements are shared; a script's own function is
    # private to it and its source()rs, and says so.
    tool_fn = row("analysis/lib/tools.R", "tool_fn(x)", "tool_fn <- function")
    main_only = row("analysis/main.R", "main_only()", "main_only <- function")
    check_by = row("tests/testthat/helper-fixtures.R", "check_by(x)", "check_by <- function")
    check("called by: main_only, analysis/other.R" in tool_fn and "private" not in tool_fn, f"lib function lost its callers:\n{tool_fn}")
    check("[private to this file]" in main_only and "calls: tool_fn" in main_only and "called by: analysis/main.R" in main_only
          and "other.R" not in main_only, f"script-private function unexpected:\n{main_only}")
    check(any(l.startswith("analysis/other.R") and "top-level code calls: tool_fn, target total" in l for l in lines),
          f"other.R heading unexpected (main_only is private to main.R, tar_read() names a target):\n{full}")
    check("calls: by_string" in check_by and "called by: tests/testthat/test-a.R" in check_by and "private" not in check_by,
          f"testthat helper unexpected:\n{check_by}")
    check("check_by" not in unref and "tool_fn" not in unref and "main_only" in entry, f"summary sections unexpected:\n{entry}\n{unref}")

    # Dotted names (CM-B4).
    stats = row("R/dotted.R", "summary.stats(x)", "summary.stats <- function")
    money = row("R/dotted.R", "format.money(x, ...)", "format.money <- function")
    check('[not an S3 method of summary: no class "stats" in the project]' in stats and "[S3 method" not in stats,
          f"summary.stats tagged as a method:\n{stats}")
    check("[S3 method of format]" in money, f"format.money not a method although class \"money\" is made:\n{money}")
    for plain in ("summary.stats", "t.test_wrapper", "split.by"):
        check(plain in unref and plain in entry, f"{plain} missing from the entry/unreferenced lists:\n{entry}\n{unref}")
    check("format.money" not in unref and "[S3 method of t]" not in full, f"unreferenced unexpected:\n{unref}")

    # R6 (CM-B2).
    account = os.path.join(root, "R", "account.R")
    cls = row("R/account.R", "Account (R6 class)", "Account <- R6::R6Class")
    init = row("R/account.R", "Account$initialize(balance)", "initialize = function")
    deposit = row("R/account.R", "Account$deposit(x)", "deposit = function")
    pretty = row("R/account.R", "Account$pretty()", "pretty = function")
    accrue = row("R/account.R", "Savings$accrue(rate)", "accrue = function")
    opened = row("R/account.R", "open_account()", "open_account <- function")
    check("[exported]" in cls, f"export(Account) did not resolve:\n{cls}")
    check("calls: Account$audit" in init and "called by: open_account, tests/testthat/test-a.R" in init,
          f"Class$new() does not reach initialize (also through Savings, which inherits it):\n{init}")
    check("calls: validate_amount, Account$audit" in deposit and "called by: Savings$accrue (2), open_account" in deposit,
          f"super$/self$ up the inherit chain and obj$method() unexpected:\n{deposit}")
    check("called by: open_account" in pretty, f"active binding access unexpected:\n{pretty}")
    check("calls: Account$deposit" in accrue and "called by: tests/testthat/test-a.R" in accrue, f"Savings$accrue unexpected:\n{accrue}")
    check("calls: Account$initialize, Account$deposit, Account$pretty" in opened, f"open_account unexpected:\n{opened}")
    check("Account$deposit (via Savings$accrue)" in run("--tests", "Account$deposit"), "--tests does not reach an R6 method through $new/obj$m()")
    near_cls = run("Account")
    check(f"methods: Account$initialize (R/account.R:{line_of(account, 'initialize = function')})" in near_cls, f"class neighbourhood unexpected:\n{near_cls}")

    # Aliases, lists of functions and wrapped literals (CM-B3); constant
    # strings (CM-B7); the call site is never a comment line (CM-B6).
    values = os.path.join(root, "R", "values.R")
    check("calls: helper" in row("R/values.R", "use_alias(x)", "use_alias <- function"), "a call through an alias is not a call")
    check("calls: helper, by_string" in row("R/values.R", "use_registry(x)", "use_registry <- function"), "helpers$h()/helpers[[\"byname\"]]() not resolved")
    memo = row("R/values.R", "memo_helper(x)", "memo_helper <- memoise")
    check("calls: helper" in memo and "called by: use_memo" in memo, f"memoised literal unexpected:\n{memo}")
    check("use_alias" in helper and "use_registry" in helper and "memo_helper" in helper and "twice_helper" not in full,
          f"helper's callers through aliases unexpected:\n{helper}")
    check("calls: by_string (by string)" in row("R/values.R", "by_const(xs)", "by_const <- function"), "do.call(fname) with a constant local not resolved")
    check("calls: by_string (by string)" in row("R/values.R", "by_paste(xs)", "by_paste <- function"), "match.fun(paste0()) of literals not resolved")
    near_helper = run("helper")
    check(f"pinned (as value)   R/values.R:{line_of(values, 'pinned <- function')}   (call at R/values.R:{line_of(values, 'xs, helper)')})" in near_helper,
          f"call site pinned to a comment line:\n{near_helper}")

    # targets (CM-B5): dependencies between targets, pinned to their own line.
    tfile = os.path.join(root, "_targets.R")
    raw = row("_targets.R", "target raw", "tar_target(raw")
    total = row("_targets.R", "target total", "tar_target(total")
    check("calls: helper   called by: target total" in raw, f"target raw unexpected:\n{raw}")
    check("calls: by_string, target raw" in total and "called by: target report, analysis/other.R" in total, f"target total unexpected:\n{total}")
    check(f"_targets.R:{line_of(tfile, 'tar_target(report')}   target report" in full and "calls: describe, target total" in full,
          f"target report unexpected:\n{full}")

    # Neighbourhood: callers two levels up, with call sites, and nothing unrelated.
    near = run("inner_step")
    check("static" in near.splitlines()[0], f"neighbourhood header unexpected:\n{near}")
    body = near.split("inner_step(x)", 1)[1]
    chain = os.path.join(root, "R", "chain.R")
    mid_def, mid_call = line_of(chain, "middle_step <- function"), line_of(chain, "inner_step(x) * 2")
    top_def, top_call = line_of(chain, "outer_step <- function"), line_of(chain, "y <- middle_step(x)")
    check(f"    middle_step   R/chain.R:{mid_def}   (call at R/chain.R:{mid_call})" in body and
          f"      outer_step   R/chain.R:{top_def}   (call at R/chain.R:{top_call})" in body,
          f"neighbourhood callers unexpected:\n{near}")
    for unrelated in ("run_all", "orphan", "wrap", "describe", " helper"):
        check(unrelated not in body, f"neighbourhood of inner_step mentions {unrelated}:\n{near}")
    check(f"    vig_helper   vignettes/intro.Rmd:{line_of(rmd, 'vig_helper <- function')}   (call at vignettes/intro.Rmd:{line_of(rmd, 'vig_helper <- function')})" in body
          and f"      vignettes/intro.Rmd:{line_of(rmd, 'vig_helper(2)')}   (top level)" in body,
          f"neighbourhood callers in the vignette unexpected:\n{near}")
    missing = run("innr_step")
    check("not defined in the project" in missing and "inner_step" in missing, f"unknown name output unexpected:\n{missing}")

    data = json.loads(run("--json"))
    by_name = {d["name"]: d for d in data}
    check(len(data) == 39 and by_name["outer_step"]["exported"] is True and by_name["print.myclass"]["kind"] == "s3_method",
          f"JSON map unexpected:\n{json.dumps(data)[:800]}")
    check({"by": "vignettes/intro.Rmd", "kind": "direct", "file": "vignettes/intro.Rmd", "line": line_of(rmd, "outer_step(1)")}
          in by_name["outer_step"]["called_by"], f"JSON vignette call site unexpected:\n{json.dumps(by_name['outer_step'])}")
    check({"name": "helper", "kind": "value", "line": 2} in by_name["run_all"]["calls"] and
          any(c["by"] == "scripts/run.R" and c["line"] == 2 for c in by_name["run_all"]["called_by"]),
          f"JSON edges unexpected:\n{json.dumps(by_name['run_all'])}")
    sub = json.loads(run("--json", "inner_step"))
    check(sorted(d["name"] for d in sub) == ["inner_step", "middle_step", "outer_step", "vig_helper"], f"JSON neighbourhood unexpected:\n{sub}")
    check(by_name["middle_step"]["end"] == line_of(chain, "middle_step <- function") + 2, f"JSON end line unexpected:\n{by_name['middle_step']}")

    # --tests: the test files that reach a function through its callers.
    tests = run("--tests", "inner_step", "helper")
    check("tests/testthat/test-a.R   inner_step (via middle_step <- outer_step)" in tests
          and "Rscript -e 'devtools::test(filter = \"^a$\")'" in tests and "No test reaches: helper" in tests,
          f"--tests output unexpected:\n{tests}")
    # --file: the neighbourhood of every function defined in one file.
    per_file = run("--file", "R/apply.R", "--depth", "1")
    check("around run_all, helper, by_string, wrap, depth 1" in per_file.splitlines()[0] and "by_string (by string)" in per_file,
          f"--file output unexpected:\n{per_file}")

    # --diff: what the working tree changes against HEAD. The fixture becomes a
    # repository; then a signature changes, a body changes, a function goes, a
    # new one arrives in a new (untracked) file, and top-level code changes.
    def git(*a):
        out = subprocess.run(["git", *a], cwd=root, capture_output=True, text=True)
        check(out.returncode == 0, f"git {' '.join(a)} failed:\n{out.stderr}")
        return out.stdout
    git("init", "-q")
    git("add", "-A")
    git("-c", "user.email=smoke@test", "-c", "user.name=smoke", "commit", "-qm", "base")
    before = run("--diff")
    check("No R file differs from HEAD" in before, f"--diff on a clean tree unexpected:\n{before}")
    with open(chain) as f:
        text = f.read()
    text = text.replace("inner_step <- function(x) {\n  x + 1\n}", "inner_step <- function(x, offset = 1) {\n  x + offset\n}")
    with open(chain, "w") as f:
        f.write(text)
    printr = os.path.join(root, "R", "print.R")
    with open(printr) as f:
        text = f.read()
    with open(printr, "w") as f:
        f.write(text.replace('orphan <- function() {\n  "nobody calls me"\n}\n', ""))
    with open(os.path.join(root, "R", "apply.R"), "a") as f:
        f.write("\nhelper <- function(x) x * 4\n")
    with open(os.path.join(root, "R", "extra.R"), "w") as f:
        f.write("extra_fn <- function() helper(1)\n")
    with open(os.path.join(root, "scripts", "run.R"), "a") as f:
        f.write("print(orphan())\n")
    diff = run("--diff")
    check(f"inner_step(x, offset)   R/chain.R:{line_of(chain, 'inner_step <- function')}   [arguments were (x)]   called by: middle_step, vig_helper" in diff,
          f"--diff missed the signature change:\n{diff}")
    check("A changed argument list" in diff and "[body changed]" in diff and "extra_fn()   R/extra.R:1   [new]" in diff,
          f"--diff change kinds unexpected:\n{diff}")
    check("helper(x)   R/apply.R:16   [body changed]   [also defined at R/apply.R:6]" in diff,
          f"--diff missed the redefined helper:\n{diff}")
    check("orphan()   was R/print.R:" in diff and "still mentioned at: scripts/run.R:4" in diff, f"--diff missed the removed function:\n{diff}")
    check("Top-level code changed:" in diff and "  scripts/run.R (lines 4)" in diff and "R/print.R" not in diff.split("Top-level code changed:")[1].split("\n\n")[0],
          f"--diff top-level section unexpected:\n{diff}")
    check("tests/testthat/test-a.R   inner_step (via middle_step <- outer_step)" in diff and "No test reaches: " in diff
          and "extra_fn" in diff.split("No test reaches: ")[1], f"--diff tests section unexpected:\n{diff}")
    check("Callers and callees of the changed definitions (depth 2):" in diff and f"      outer_step   R/chain.R:{top_def}   (call at R/chain.R:{top_call})" in diff,
          f"--diff neighbourhood unexpected:\n{diff}")
    data = json.loads(run("--json", "--diff"))
    changed = {c["name"]: c for c in data["changed"]}
    check(changed["inner_step"]["change"] == "arguments were (x)" and changed["extra_fn"]["change"] == "new"
          and data["deleted"][0]["name"] == "orphan" and data["deleted"][0]["mentions"] == ["scripts/run.R:4"]
          and any(t["file"] == "tests/testthat/test-a.R" and "inner_step" in t["reaches"] for t in data["tests"])
          and "extra_fn" in data["untested"], f"--diff JSON unexpected:\n{json.dumps(data)[:1200]}")
    nogit = s.make_project("callmap_nogit", {"R/apply.R": os.path.join(root, "R", "apply.R")})
    check("is not inside a git repository" in run("--root", nogit, "--diff"), "--diff outside git should say so")

    # Without a NAMESPACE the dotted-name rule is the same: a class the code
    # names corroborates a base generic's method, nothing else does.
    plain = run("--root", s.make_project("callmap_plain", {"R/dotted.R": os.path.join(root, "R", "dotted.R")}))
    check('[not an S3 method of summary: no class "stats" in the project]' in plain and "[S3 method of format]" in plain
          and "[S3 method of t]" not in plain, f"dotted names outside a package unexpected:\n{plain}")

    # A wrong or empty root is an error (exit 2, nothing on stdout), not a map
    # of no functions (DT-06).
    empty = os.path.join(s.scratch, "callmap_empty")
    os.makedirs(empty, exist_ok=True)
    for args, said in ((["--root", "/nonexistent/dir", "helper"], "no such directory"), (["--root", empty], "no R, Rmd or qmd file"),
                       (["--json", "--root", empty], "pass --root DIR"), (["--root", os.path.join(root, "DESCRIPTION")], "no such directory")):
        out = subprocess.run([tool, *args], cwd=root, capture_output=True, text=True, timeout=120)
        check(out.returncode == 2 and said in out.stderr and out.stdout == "",
              f"r-callmap {' '.join(args)}: exit {out.returncode}, expected 2 and '{said}' on stderr:\n{out.stdout}{out.stderr}")


def scenario_hooks(s):
    """SessionStart context + index pre-warm, the note after LSP results, and
    r-lsp-status. The LSP payloads are in the shapes Claude Code's LSP tool
    prints (captured from real sessions): a file line followed by indented
    "Line N:M" lines, call-hierarchy items with "[calls at: ...]", and
    "Defined in FILE:N:M"."""
    require_packages("languageserver")   # the pre-warm builds the server's index cache
    root = s.make_package("hooks_pkg")
    start = os.path.join(REPO, "plugins", "r-lsp", "hooks", "session-start.sh")
    out = run_hook(start, {}, env={"CLAUDE_PROJECT_DIR": root}, stdin_text="{}")
    check("R LSP plugin: R package 'pkgtwo'" in out and "static call map of all" in out,
          f"session-start context unexpected:\n{out}")
    cache = rscript(f'cat(file.path(tools::R_user_dir("languageserver", "cache"), "workspace-index", '
                    f'paste0(digest::digest(normalizePath("{root}"), algo = "xxhash64"), ".rds")))').strip()
    for _ in range(60):
        if os.path.exists(cache):
            break
        time.sleep(0.5)
    check(os.path.exists(cache), f"pre-warm did not create the index cache {cache}")

    # Extra files for the note heuristics: a roxygen link in a file the
    # server returns, a script-level variable assigned in two branches, and
    # a column created inside mutate().
    with open(os.path.join(root, "R", "other.R"), "a") as f:
        f.write("#' See [add_tax()] for the rate.\nlinked <- function(x) add_tax(x)\n")
    os.makedirs(os.path.join(root, "scripts"), exist_ok=True)
    with open(os.path.join(root, "scripts", "run.R"), "w") as f:
        f.write("if (file.exists('cache.rds')) {\n  totals <- readRDS('cache.rds')\n} else {\n"
                "  totals <- add_tax(1:3)\n  saveRDS(totals, 'cache.rds')\n}\n"
                "print(totals)\nreport <- dplyr::mutate(data.frame(x = totals), net_amount = x - 1)\n"
                "summary(report$net_amount)\n")
    lsp_hook = os.path.join(REPO, "plugins", "r-lsp", "hooks", "post-lsp.sh")
    env = {"CLAUDE_PROJECT_DIR": root}

    def lsp(op, rel, line, char, response):
        payload = {"tool_name": "LSP", "cwd": root,
                   "tool_input": {"operation": op, "filePath": os.path.join(root, rel), "line": line, "character": char},
                   "tool_response": response}
        return run_hook(lsp_hook, payload, env=env)

    refs = ("Found 4 references across 3 files:\n\nR/tax.R:\n  Line 5:1\n  Line 9:20\n\nR/other.R:\n  Line 1:22\n"
            "  Line 4:28\n\ntests/testthat/test-tax.R:\n  Line 2:16")
    ctx = hook_context(lsp("findReferences", "R/tax.R", 5, 1, refs))
    check("R/other.R:3:" in ctx and "[add_tax()]" in ctx, f"post-lsp note missed the roxygen link in a returned file:\n{ctx}")
    check("R/other.R:4:" not in ctx and "test-tax.R" not in ctx, f"post-lsp note repeated lines the server returned:\n{ctx}")
    old_shape = "Found 3 references across 2 files:\nR/tax.R: Line 5:1, Line 9:20\nR/other.R: Line 1:28"
    ctx = hook_context(lsp("findReferences", "R/tax.R", 5, 1, old_shape))
    check("tests/testthat/test-tax.R:2:" in ctx, f"post-lsp note (old answer shape) missed the test file:\n{ctx}")

    defn = lsp("goToDefinition", "R/other.R", 1, 22, "Defined in R/tax.R:5:1")
    check(not defn.strip(), f"post-lsp hook spoke after a definition that answered the question:\n{defn}")
    defn = lsp("goToDefinition", "scripts/run.R", 7, 7, "Defined in scripts/run.R:4:3")
    ctx = hook_context(defn)
    check("scripts/run.R:2:" in ctx and "scripts/run.R:4:" not in ctx, f"definition note should list the other branch only:\n{ctx}")

    with open(os.path.join(root, "tests", "testthat", "test-tax.R"), "a") as f:
        f.write("test_that('more', {\n  expect_equal(add_tax(1), 1.2)\n  expect_equal(add_tax(2), 2.4)\n})\n")
    calls = ("Found 2 incoming calls:\n\nR/other.R:\n  gross (Function) - Line 1 [calls at: 1:22]\n\n"
             "tests/testthat/test-tax.R:\n  test_that (Function) - Line 4 [calls at: 5:16, 6:16]")
    ctx = hook_context(lsp("incomingCalls", "R/tax.R", 5, 1, calls))
    check("tests/testthat/test-tax.R:2:" in ctx and "R/tax.R:9:" in ctx and "R/other.R:4:" in ctx,
          f"incomingCalls note missed callers:\n{ctx}")
    check("R/other.R:1:" not in ctx and "test-tax.R:5:" not in ctx and "test-tax.R:6:" not in ctx,
          f"incomingCalls note repeated a returned call site:\n{ctx}")
    check("Roxygen links" in ctx and "R/other.R:3:" in ctx, f"incomingCalls note missed the roxygen link:\n{ctx}")

    split = "Found 3 references across 1 files:\n\nscripts/run.R:\n  Line 4:3\n  Line 5:11\n  Line 7:7"
    ctx = hook_context(lsp("findReferences", "scripts/run.R", 4, 3, split))
    check("assigned at script level inside a block" in ctx and "scripts/run.R:2:" in ctx and "scripts/run.R:8:" in ctx,
          f"script-level variable note wrong:\n{ctx}")
    check("trust" not in ctx, f"script-level variable note tells Claude to trust a partial answer:\n{ctx}")
    none = "No references found. This may occur if the symbol has no usages"
    ctx = hook_context(lsp("findReferences", "scripts/run.R", 9, 16, none))
    check("data-frame column" in ctx and "scripts/run.R:8" in ctx, f"column-name note wrong:\n{ctx}")

    status_file = os.path.join(s.scratch, "hooks.status.log")
    with open(status_file, "w") as f:
        f.write(f"12:00:00 [{root}] R LSP: workspace: indexing 3 files\n")
    payload = {"tool_name": "LSP", "cwd": root,
               "tool_input": {"operation": "findReferences", "filePath": os.path.join(root, "R", "tax.R"),
                              "line": 5, "character": 1},
               "tool_response": none}
    ctx = hook_context(run_hook(lsp_hook, payload, env={"CLAUDE_PROJECT_DIR": root, "R_LSP_STATUS_FILE": status_file}))
    check("still indexing" in ctx, f"post-lsp note did not report the running index:\n{ctx}")
    covered = dict(payload, tool_response="R/tax.R R/other.R tests/testthat/test-tax.R scripts/run.R")
    quiet = run_hook(lsp_hook, covered, env={"CLAUDE_PROJECT_DIR": root})
    check(not quiet.strip(), f"post-lsp hook spoke although every file was covered:\n{quiet}")

    # r-lsp-status shows only the newest server's events and notices a dead one.
    status_tool = os.path.join(REPO, "plugins", "r-lsp", "bin", "r-lsp-status")
    with open(status_file, "w") as f:
        f.write(f"12:00:00 [{root}] R LSP: server started (pid 999999, languageserver 0.3.20)\n"
                f"12:00:01 [{root}] R LSP: index ready (3 files, 1.0 s)\n"
                f"12:05:00 [{root}] R LSP: server started (pid {os.getpid()}, languageserver 0.3.20)\n"
                f"12:05:01 [{root}] R LSP: workspace: indexing 3 files\n")
    shown = subprocess.run([status_tool], env=dict(os.environ, CLAUDE_PROJECT_DIR=root, R_LSP_STATUS_FILE=status_file),
                           capture_output=True, text=True).stdout
    check("index ready" not in shown and "indexing 3 files" in shown and "exited" not in shown,
          f"r-lsp-status showed a previous server's events:\n{shown}")
    with open(status_file, "w") as f:
        f.write(f"12:00:00 [{root}] R LSP: server started (pid 999999, languageserver 0.3.20)\n"
                f"12:00:01 [{root}] R LSP: index ready (3 files, 1.0 s)\n")
    shown = subprocess.run([status_tool], env=dict(os.environ, CLAUDE_PROJECT_DIR=root, R_LSP_STATUS_FILE=status_file),
                           capture_output=True, text=True).stdout
    check("has exited" in shown, f"r-lsp-status did not notice the dead server:\n{shown}")

    # Two live servers in one project (a second session in the same
    # directory): every line carries its server's pid, and the readers show
    # this session's server when its session id or its parent process (here
    # the test process, an ancestor of the hook) tells it apart, else every
    # live one, saying so. Server a is ready, server b still indexing.
    pid_a, pid_b = os.getpid(), os.getppid()

    def two_servers(ppid_b, sessions=True):
        with open(status_file, "w") as f:
            f.write(f"12:00:00 [{root}] [pid {pid_a}] R LSP: server started (pid {pid_a}, ppid 1, languageserver 0.3.20{', session sess-a' if sessions else ''})\n"
                    f"12:00:01 [{root}] [pid {pid_a}] R LSP: index ready (3 files, 1.0 s)\n"
                    f"12:05:00 [{root}] [pid {pid_b}] R LSP: server started (pid {pid_b}, ppid {ppid_b}, languageserver 0.3.20{', session sess-b' if sessions else ''})\n"
                    f"12:05:01 [{root}] [pid {pid_b}] R LSP: workspace: indexing 3 files\n"
                    f"12:05:02 [{root}] [pid {pid_a}] R LSP: added workspace /tmp/other\n")

    def status_as(session):
        return subprocess.run([status_tool], env=dict(os.environ, CLAUDE_PROJECT_DIR=root, R_LSP_STATUS_FILE=status_file,
                                                      CLAUDE_CODE_SESSION_ID=session), capture_output=True, text=True).stdout

    def note_as(session):
        p = dict(payload, session_id=session) if session else {k: v for k, v in payload.items() if k != "session_id"}
        return hook_context(run_hook(lsp_hook, p, env={"CLAUDE_PROJECT_DIR": root, "R_LSP_STATUS_FILE": status_file,
                                                        "CLAUDE_CODE_SESSION_ID": session}))

    two_servers(ppid_b=1)
    shown = status_as("sess-a")
    check("index ready" in shown and "added workspace" in shown and "indexing 3 files" not in shown,
          f"r-lsp-status as session a showed the other session's server:\n{shown}")
    check("still indexing" not in note_as("sess-a"), "post-lsp note said 'still indexing' from the other session's server")
    shown = status_as("sess-b")
    check("indexing 3 files" in shown and "index ready" not in shown, f"r-lsp-status as session b wrong:\n{shown}")
    ctx = note_as("sess-b")
    check("still indexing" in ctx and "another session" not in ctx, f"post-lsp note as session b wrong:\n{ctx}")
    two_servers(ppid_b=1, sessions=False)
    shown = status_as("")
    check("2 servers are running" in shown and "index ready" in shown and "indexing 3 files" in shown,
          f"r-lsp-status with no way to tell the servers apart should show both:\n{shown}")
    ctx = note_as("")
    check("still indexing" in ctx and "another session's, or this one's" in ctx and f"server pid {pid_b}" in ctx,
          f"post-lsp note with two live servers should name the uncertainty and the pid:\n{ctx}")
    two_servers(ppid_b=pid_a, sessions=False)   # b was started by this process, the hook's ancestor
    shown = status_as("")
    check("indexing 3 files" in shown and "index ready" not in shown and "2 servers" not in shown,
          f"r-lsp-status did not pick the server whose parent is its ancestor:\n{shown}")
    ctx = note_as("")
    check("still indexing" in ctx and "another session" not in ctx, f"post-lsp note did not pick the server by ancestry:\n{ctx}")
    # A server that could not activate renv: the note says its 'not
    # installed' diagnostics and empty hovers are not answers.
    with open(status_file, "a") as f:
        f.write(f"12:06:00 [{root}] [pid {pid_b}] R LSP: renv activation added no library: packages the project pins read as not installed\n")
    ctx = note_as("")
    check("could not use this project's renv library" in ctx and "added no library" in ctx, f"post-lsp note missed the renv failure:\n{ctx}")
    # Without CLAUDE_PROJECT_DIR (the Bash tool after a cd into a
    # subdirectory) the project is the nearest ancestor with events.
    no_project = {k: v for k, v in os.environ.items() if k != "CLAUDE_PROJECT_DIR"}
    shown = subprocess.run([status_tool], cwd=os.path.join(root, "R"), env=dict(no_project, R_LSP_STATUS_FILE=status_file, CLAUDE_CODE_SESSION_ID=""),
                           capture_output=True, text=True).stdout
    check("indexing 3 files" in shown and "No R LSP status events" not in shown,
          f"r-lsp-status run from a subdirectory did not find the project's events:\n{shown}")

    # The monitor forwards only the events Claude can act on, for this
    # directory only, and drops the timestamp and tag.
    monitor = os.path.join(REPO, "plugins", "r-lsp", "hooks", "monitor-status.sh")
    data_dir = os.path.join(s.scratch, "monitor_data")
    os.makedirs(data_dir, exist_ok=True)
    # The monitor tags on $PWD, which bash resolves to the real path (the
    # scratch dir sits under a symlink on macOS), as Claude Code's cwd is.
    tag = os.path.realpath(root)
    proc = subprocess.Popen(["bash", monitor, data_dir], cwd=tag, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                            text=True, start_new_session=True)
    time.sleep(1.0)
    with open(os.path.join(data_dir, "status.log"), "a") as f:
        f.write(f"12:00:00 [{tag}] R LSP: server started (pid 1, languageserver 0.3.20)\n"
                f"12:00:01 [{tag}] R LSP: loaded 3 test/script files as documents\n"
                f"12:00:01 [{tag}] R LSP: workspace: indexing 3 files\n"
                f"12:00:02 [{tag}] R LSP: index ready (3 files, 1.0 s)\n"
                f"12:00:03 [{tag}/elsewhere] R LSP: index ready (9 files, 1.0 s)\n"
                f"12:00:04 [{tag}] R LSP: added workspace /tmp/other (with its renv library)\n")
    time.sleep(1.5)
    os.killpg(os.getpgid(proc.pid), signal.SIGTERM)
    forwarded = proc.communicate(timeout=10)[0].splitlines()
    check(forwarded == ["R LSP: index ready (3 files, 1.0 s)", "R LSP: added workspace /tmp/other (with its renv library)"],
          f"monitor forwarded the wrong lines:\n{forwarded}")
    # Outside an R project the monitor exits at once instead of tailing.
    no_r = os.path.join(s.scratch, "not_r_project", "src")
    os.makedirs(no_r, exist_ok=True)
    with open(os.path.join(no_r, "main.py"), "w") as f:
        f.write("print(1)\n")
    try:
        idle = subprocess.run(["bash", monitor, data_dir], cwd=os.path.dirname(no_r), capture_output=True, text=True, timeout=5)
        check(idle.returncode == 0 and not idle.stdout, f"monitor misbehaved in a non-R directory: {idle.returncode} {idle.stdout!r}")
    except subprocess.TimeoutExpired:
        check(False, "monitor kept running in a directory with no R files")


def scenario_hooks_changes(s):
    """Changes the Edit|Write hook never sees. A file written through Bash is
    checked by the post-bash hook (bug linters included) once, the ledger
    keeping it from being checked again; the stop hook holds the turn once
    for findings in a file no check has seen; a subagent gets the context at
    its start and the bug linters after its edits; session start on compact
    re-stamps the session."""
    root = s.make_project("hooks_changes", {"data/sales.csv": os.path.join(FIXTURES, "columns", "data", "sales.csv")})
    data_dir = os.path.join(s.scratch, "hooks_changes_data")
    env = {"CLAUDE_PROJECT_DIR": root, "CLAUDE_PLUGIN_DATA": data_dir}
    hooks = os.path.join(REPO, "plugins", "r-lsp", "hooks")
    sid = "smoke-session"

    def payload(**kw):
        return dict({"session_id": sid, "cwd": root}, **kw)

    def hook(name, **kw):
        return run_hook(os.path.join(hooks, name), payload(**kw), env=env)

    rfile = os.path.join(root, "R", "clean.R")
    os.makedirs(os.path.dirname(rfile))
    with open(rfile, "w") as f:
        f.write("library(dplyr)\nsales <- readr::read_csv('data/sales.csv')\nout <- sales %>% filter(regoin == 'A')\n"
                "undefined_helper_xyz(out)\n")
    # Before any session start there is no stamp: the hooks stay quiet.
    check(not hook("post-bash.sh", tool_name="Bash", tool_input={"command": "cat > R/clean.R"}).strip(),
          "post-bash spoke before the session was stamped")
    out = hook("session-start.sh", source="compact")
    check("R LSP plugin: R scripts project" in out, f"session-start context on compact unexpected:\n{out}")
    stamp = os.path.join(data_dir, "sessions", sid, "start.stamp")
    check(os.path.exists(stamp), "session-start did not stamp the session")
    time.sleep(1.1)  # the ledger keys on whole seconds
    with open(rfile, "a") as f:
        f.write("# written through Bash\n")
    ctx = hook_context(hook("post-bash.sh", tool_name="Bash", tool_input={"command": "cat >> R/clean.R <<'EOF'"}))
    check("this command changed 1 R file (R/clean.R)" in ctx and "'regoin' is not a column" in ctx, f"post-bash note unexpected:\n{ctx}")
    check("bug linters" in ctx and "undefined_helper_xyz" in ctx and "could not find function" in ctx,
          f"post-bash note has no bug-linter diagnostics (the server never sees Bash-made changes):\n{ctx}")
    check("re-reads files changed on disk" in ctx and "Read it again before editing it" in ctx, f"post-bash note does not say how the change reaches the server and Claude:\n{ctx}")
    check(not hook("post-bash.sh", tool_name="Bash", tool_input={"command": "ls"}).strip(), "post-bash checked the same file twice")
    check(not hook("stop-check.sh", stop_hook_active=False).strip(), "stop hook spoke although every file was checked")
    time.sleep(1.1)
    with open(rfile, "a") as f:
        f.write("more <- sales %>% select(amuont)\n")
    data = json.loads(hook("stop-check.sh", stop_hook_active=False))
    check(data["decision"] == "block" and "without a post-edit check" in data["reason"] and "'amuont' is not a column" in data["reason"]
          and "will not hold the turn twice" in data["reason"], f"stop hook block unexpected:\n{data}")
    check(not hook("stop-check.sh", stop_hook_active=False).strip(), "stop hook held the turn again for a file it had checked")
    time.sleep(1.1)
    with open(rfile, "a") as f:
        f.write("again <- sales %>% select(amuont)\n")
    check(not hook("stop-check.sh", stop_hook_active=True).strip(), "stop hook held the turn twice (stop_hook_active)")

    # A subagent: context at its start, the bug linters after its edits.
    ctx = hook_context(hook("subagent-start.sh", agent_id="agent-1", agent_type="general-purpose"))
    check("You are a subagent" in ctx and "r-callmap" in ctx and "R LSP plugin:" in ctx, f"subagent-start context unexpected:\n{ctx}")
    ctx = hook_context(hook("post-edit.sh", agent_id="agent-1", tool_name="Edit", tool_input={"file_path": rfile}))
    check("bug linters" in ctx and "undefined_helper_xyz" in ctx and "'regoin' is not a column" in ctx,
          f"post-edit inside a subagent ran no bug linters:\n{ctx}")
    ctx = hook_context(hook("post-edit.sh", tool_name="Edit", tool_input={"file_path": rfile}))
    check("bug linters" not in ctx and "'regoin' is not a column" in ctx,
          f"post-edit in the main conversation ran the bug linters without a .lintr (the server reports there):\n{ctx}")
    check(not hook("post-bash.sh", tool_name="Bash", tool_input={"command": "ls"}).strip(), "post-bash re-checked a file the edit hook had checked")
    sdir = os.path.join(data_dir, "sessions", sid)

    def ledger_lines():
        with open(os.path.join(sdir, "checked.tsv"), encoding="utf-8") as f:
            return [l for l in f.read().splitlines() if l]

    # A note the Stop hook shows to the user alone (a systemMessage: nothing
    # worth holding the turn for) is not said to Claude: the once-per-session
    # markers are set only when the text reaches it (additionalContext, a
    # block reason), so the next edit still gives the full remedy.
    time.sleep(1.1)
    stopped = os.path.join(root, "R", "stopped.R")
    with open(stopped, "w") as f:
        f.write("library(dplyr)\nsales <- readr::read_csv('data/sales.csv')\nshuffle_cols <- function(d) d[, sample(names(d))]\n"
                "w <- shuffle_cols(sales)\nz <- w %>% filter(amount > 1)\n")
    out = hook("stop-check.sh", stop_hook_active=False)
    check("systemMessage" in out and "cache it once by name" in out and "decision" not in out, f"stop hook on a note-only file unexpected:\n{out}")
    check(not os.path.exists(os.path.join(sdir, "said-note-w_4")), "a systemMessage, which Claude never sees, set the once-per-session marker")
    ctx = hook_context(hook("post-edit.sh", tool_name="Edit", tool_input={"file_path": stopped}))
    check("cache it once by name" in ctx and "said in full earlier" not in ctx, f"post-edit gave the short form of a note Claude never saw in full:\n{ctx}")
    check(os.path.exists(os.path.join(sdir, "said-note-w_4")), "additionalContext did not set the once-per-session marker")
    ctx = hook_context(hook("post-edit.sh", tool_name="Edit", tool_input={"file_path": stopped}))
    check("R/stopped.R:4: 'w' is known up to shuffle_cols(sales) on line 4; nothing downstream is checked (said in full earlier this session)" in ctx,
          f"the second edit should give the short form:\n{ctx}")

    # Only the notes shown take their once-per-session turn: of seven
    # unresolved sources five are said in full and the two clipped stay
    # unsaid, so they are not later given in the short form unseen.
    many = os.path.join(root, "R", "many.R")
    with open(many, "w") as f:
        f.write("library(dplyr)\n" + "".join(f'd{i} <- readr::read_csv("data/none{i}.csv") %>% filter(x > 1)\n' for i in range(1, 8)))
    ctx = hook_context(hook("post-edit.sh", tool_name="Edit", tool_input={"file_path": many}))
    check("... and 2 more" in ctx and 'r-colcheck learn "data/none5.csv"' in ctx, f"post-edit note for seven sources unexpected:\n{ctx}")
    check(all(os.path.exists(os.path.join(sdir, f"said-note-data_none{i}.csv")) for i in range(1, 6))
          and not any(os.path.exists(os.path.join(sdir, f"said-note-data_none{i}.csv")) for i in (6, 7)),
          f"clipped notes were marked as said: {sorted(os.listdir(sdir))}")

    # In a C locale a file whose name has non-ASCII characters is still
    # checked: the shell hands R the path as bytes (R_LSP_HOOK_FILE), where
    # the JSON's UTF-8 string would not translate and the file would go
    # unchecked in silence. The payload carries the name raw, as Claude Code
    # writes it.
    accented = os.path.join(root, "R", "\u00e9\u00e9.R")
    with open(accented, "w") as f:
        f.write("library(dplyr)\nsales <- readr::read_csv('data/sales.csv')\nout <- sales %>% filter(regoin == 'A')\n")
    c_env = dict(env, LANG="C", LC_ALL="C")
    ctx = hook_context(run_hook(os.path.join(hooks, "post-edit.sh"), None, env=c_env,
                                stdin_text=json.dumps(payload(tool_name="Edit", tool_input={"file_path": accented}), ensure_ascii=False)))
    check("'regoin' is not a column" in ctx, f"post-edit in a C locale missed a file with a non-ASCII name:\n{ctx}")
    check(any(l.endswith("\t" + os.path.realpath(accented)) for l in ledger_lines()), "the file with a non-ASCII name was not ledgered")
    check(not run_hook(os.path.join(hooks, "post-bash.sh"), payload(tool_name="Bash", tool_input={"command": "ls"}), env=c_env).strip(),
          "post-bash re-checked the file with a non-ASCII name")

    # A file rewritten while the edit hook's check ran is ledgered as it was
    # checked, so the new content comes back at the Stop hook. (r-colcheck's
    # results cache is off for that Stop: it keys a result on the file as it
    # is after the check, so it would serve the pre-rewrite result.)
    time.sleep(1.1)
    race = os.path.join(root, "R", "race.R")
    with open(race, "w") as f:
        f.write("library(dplyr)\nsales <- readr::read_csv('data/sales.csv')\nok <- sales %>% filter(region == 'A')\n")
    before = os.stat(race)
    appender = subprocess.Popen(["sh", "-c", "sleep 0.8; printf '%s\\n' \"bad <- sales %>% select(amuont)\" >> \"$0\"", race])
    ctx = hook_context(hook("post-edit.sh", tool_name="Edit", tool_input={"file_path": race}))
    appender.wait(timeout=30)
    stamps = [l.split("\t")[0] for l in ledger_lines() if l.endswith("\t" + os.path.realpath(race))]
    check(stamps and (stamps[-1] == f"{int(before.st_mtime)}:{before.st_size}" or "'amuont'" in ctx),
          f"the edit hook ledgered a file rewritten during its check under the new content's stamp ({stamps}, checked {int(before.st_mtime)}:{before.st_size}):\n{ctx}")
    out = run_hook(os.path.join(hooks, "stop-check.sh"), payload(stop_hook_active=False), env=dict(env, R_LSP_COLCHECK_CACHE="off"))
    check("'amuont'" in ctx or ("'amuont' is not a column" in out and json.loads(out)["decision"] == "block"),
          f"the content written during the edit hook's check was never reported:\n{ctx}\n{out}")

    # Many files at once (a git checkout): the newest ten are checked, the rest listed.
    time.sleep(1.1)
    for i in range(12):
        with open(os.path.join(root, "R", f"gen_{i:02d}.R"), "w") as f:
            f.write(f"gen_{i} <- function(x) x + {i}\n")
    ctx = hook_context(hook("post-bash.sh", tool_name="Bash", tool_input={"command": "git checkout other"}))
    check("this command changed 12 R files" in ctx and "and 2 more" in ctx and "Not checked (over the limit of 10 per command)" in ctx
          and "Checked (10 files): no column, argument or parse findings" in ctx, f"post-bash note for many files unexpected:\n{ctx}")
    # The two past the limit are ledgered as listed: the Bash hook does not
    # bring them back at every command, the Stop hook (its own budget) checks
    # them at the end of the turn, as the README promises, and not again.
    listed = [l.split("\t")[1] for l in ledger_lines() if l.startswith("listed:") and "/gen_" in l]
    check(len(listed) == 2, f"the files past the limit are not ledgered as listed:\n{ledger_lines()}")
    check(not hook("post-bash.sh", tool_name="Bash", tool_input={"command": "ls"}).strip(), "post-bash re-checked files it had listed")
    check(not hook("stop-check.sh", stop_hook_active=False).strip(), "stop hook spoke for clean files past the Bash hook's limit")
    seen = {l.split("\t")[1] for l in ledger_lines() if not l.startswith("listed:")}
    check(all(p in seen for p in listed), f"the stop hook did not check the files the Bash hook listed past its limit:\n{ledger_lines()}")
    check(not hook("stop-check.sh", stop_hook_active=False).strip(), "stop hook came back for files it had checked")

    # Outside an R project nothing runs.
    other = os.path.join(s.scratch, "hooks_changes_other")
    os.makedirs(other, exist_ok=True)
    with open(os.path.join(other, "main.py"), "w") as f:
        f.write("print(1)\n")
    quiet = run_hook(os.path.join(hooks, "post-bash.sh"), {"session_id": "other-session", "cwd": other, "tool_name": "Bash"},
                     env={"CLAUDE_PROJECT_DIR": other, "CLAUDE_PLUGIN_DATA": data_dir})
    check(not quiet.strip(), f"post-bash spoke outside an R project:\n{quiet}")

    # A file in another repository (after /add-dir, or edited by a subagent)
    # or in a package nested under the session root is checked against its
    # own project: R/ helpers, .colcheck.json, schema cache, renv library.
    # Rooted at the session, pkgcols' pipeline helper was unknown and the
    # finding silently lost.
    other_repo = os.path.join(s.scratch, "hooks_changes_other_repo")
    shutil.copytree(os.path.join(FIXTURES, "pkgcols"), other_repo)
    ctx = hook_context(hook("post-edit.sh", tool_name="Edit", tool_input={"file_path": os.path.join(other_repo, "R", "pipeline.R")}))
    check("R/pipeline.R:5: 'amout' is not a column" in ctx, f"post-edit on a file in another repository missed the finding:\n{ctx}")
    mono = os.path.join(s.scratch, "hooks_changes_mono")
    os.makedirs(os.path.join(mono, "pkgs"))
    open(os.path.join(mono, ".git"), "w").close()
    shutil.copytree(os.path.join(FIXTURES, "pkgcols"), os.path.join(mono, "pkgs", "a"))
    ctx = hook_context(run_hook(os.path.join(hooks, "post-edit.sh"),
                                {"session_id": sid, "cwd": mono, "tool_name": "Edit",
                                 "tool_input": {"file_path": os.path.join(mono, "pkgs", "a", "R", "pipeline.R")}},
                                env={"CLAUDE_PROJECT_DIR": mono, "CLAUDE_PLUGIN_DATA": data_dir}))
    check("R/pipeline.R:5: 'amout' is not a column" in ctx, f"post-edit on a package nested under the session root missed the finding:\n{ctx}")

    # The short form of a note (the second time it is met in the session)
    # fits the note's shape: a stale-cache note has no "known up to" wording
    # and is said in full rather than garbled into one.
    stale = os.path.join(s.scratch, "hooks_changes_stale")
    shutil.copytree(os.path.join(FIXTURES, "s3columns"), stale)
    report = os.path.join(stale, "R", "report.R")
    first = hook_context(hook("post-edit.sh", tool_name="Edit", tool_input={"file_path": report}))
    check("rely on a cached schema" in first and "(observed 2025-01-15)" in first, f"the stale-cache note is missing:\n{first}")
    again = hook_context(hook("post-edit.sh", tool_name="Edit", tool_input={"file_path": report}))
    check('R/report.R:9: the findings above for "returns/monthly" rely on a cached schema' in again and "is known up to the findings above" not in again
          and "R/report.R:17: 'holdings' from s3_read(\"holdings/latest\") cannot be opened; nothing downstream is checked (r-colcheck learn" in again,
          f"the short form of the notes is wrong:\n{again}")

    # hook-post-bash.R's bare-path form (a line or argument without a
    # "mtime:size<TAB>" key, as its header accepts) records the file as it
    # is, like the edit hook, so it does not come back at every command.
    bare = os.path.join(root, "R", "bare.R")
    with open(bare, "w") as f:
        f.write("y <- 2\n")
    out = subprocess.run(["Rscript", "--vanilla", os.path.join(REPO, "plugins", "r-lsp", "scripts", "hook-post-bash.R"), bare],
                         input=json.dumps(payload()), capture_output=True, text=True, timeout=120,
                         env=dict(os.environ, **env, R_LSP_HOOK_VIA="bash", R_LSP_HOOK_ROOT=root, R_LSP_HOOK_SESSION=sdir))
    st = os.stat(bare)
    check(out.returncode == 0 and f"{int(st.st_mtime)}:{st.st_size}\t{os.path.realpath(bare)}" in ledger_lines(),
          f"a bare path was not recorded in the ledger:\n{out.stdout}{out.stderr}\n{ledger_lines()}")
    check(not hook("post-bash.sh", tool_name="Bash", tool_input={"command": "ls"}).strip(), "post-bash re-checked a file recorded from a bare path")

    # R itself failing (a broken library) is said, not swallowed with its
    # stderr: silence means checked and clean. A fake Rscript stands in.
    fake = os.path.join(s.scratch, "fake_rscript")
    os.makedirs(fake, exist_ok=True)
    with open(os.path.join(fake, "Rscript"), "w") as f:
        f.write("#!/bin/sh\nexit 3\n")
    os.chmod(os.path.join(fake, "Rscript"), 0o755)
    broken_env = dict(env, PATH=fake + os.pathsep + os.environ.get("PATH", ""))
    ctx = hook_context(run_hook(os.path.join(hooks, "post-edit.sh"), payload(tool_name="Edit", tool_input={"file_path": rfile}), env=broken_env))
    check("did not run on" in ctx and "status 3" in ctx and rfile in ctx and "r-colcheck" in ctx,
          f"post-edit hook was silent when Rscript failed:\n{ctx}")
    time.sleep(1.1)
    crash = os.path.join(root, "R", "crash.R")
    with open(crash, "w") as f:
        f.write("x <- 1\n")
    ctx = hook_context(run_hook(os.path.join(hooks, "post-bash.sh"), payload(tool_name="Bash", tool_input={"command": "touch R/crash.R"}), env=broken_env))
    check("the check did not run" in ctx and "status 3" in ctx and "R/crash.R" in ctx, f"post-bash hook was silent when Rscript failed:\n{ctx}")
    out = run_hook(os.path.join(hooks, "stop-check.sh"), payload(stop_hook_active=False), env=broken_env)
    check("systemMessage" in out and "could not be checked" in out, f"stop hook was silent when Rscript failed:\n{out}")
    # The ledger was not written, so the real check still happens.
    ctx = hook_context(hook("post-bash.sh", tool_name="Bash", tool_input={"command": "ls"}))
    check("this command changed 1 R file (R/crash.R)" in ctx and "Checked: no column" in ctx,
          f"a file whose check crashed was not checked again:\n{ctx}")


    # r-lint: the same linters from the shell, exit 1 with findings, parse errors named.
    lint_tool = os.path.join(REPO, "plugins", "r-lsp", "bin", "r-lint")
    out = subprocess.run([lint_tool, rfile], capture_output=True, text=True, timeout=120)
    check(out.returncode == 1 and "undefined_helper_xyz" in out.stdout and "[undefined_function_linter]" in out.stdout
          and "r-lint: " in out.stderr and "finding(s) in 1 file(s)" in out.stderr, f"r-lint unexpected:\n{out.stdout}{out.stderr}")
    broken = os.path.join(root, "R", "broken.R")
    with open(broken, "w") as f:
        f.write("x <- (1\n")
    out = subprocess.run([lint_tool, "--json", broken], capture_output=True, text=True, timeout=120)
    check(out.returncode == 1 and "does not parse" in json.loads(out.stdout)["lints"][0] and "1 did not parse" in out.stderr,
          f"r-lint on a broken file unexpected:\n{out.stdout}{out.stderr}")
    clean = os.path.join(root, "R", "clean_fn.R")
    with open(clean, "w") as f:
        f.write("add_one <- function(x) x + 1\n")
    out = subprocess.run([lint_tool, clean], capture_output=True, text=True, timeout=120)
    check(out.returncode == 0 and not out.stdout.strip(), f"r-lint on a clean file unexpected:\n{out.stdout}{out.stderr}")
    check(subprocess.run([lint_tool, "nope.R"], capture_output=True, text=True, timeout=60).returncode == 2, "r-lint on a missing file should exit 2")

def scenario_colcheck_flow(s):
    """Data that flows between scripts: a writer's columns resolve a reader of
    the same path when the file is not on disk (saveRDS, write_csv, a
    database table), r-colmap lists what the code writes and who reads it,
    and a targets pipeline's objects resolve through tar_read()/tar_load()
    from _targets.R."""
    root = s.make_project("colflow", {"data/sales.csv": os.path.join(FIXTURES, "columns", "data", "sales.csv")})
    tool = os.path.join(REPO, "plugins", "r-lsp", "bin", "r-colcheck")
    colmap = os.path.join(REPO, "plugins", "r-lsp", "bin", "r-colmap")
    os.makedirs(os.path.join(root, "R"))

    def write(rel, text):
        with open(os.path.join(root, rel), "w") as f:
            f.write(text)
    write("R/01_clean.R", 'library(dplyr)\nsales <- readr::read_csv("data/sales.csv")\n'
          'clean <- sales |> filter(amount > 0) |> mutate(big = amount > 5) |> select(region, year, big)\n'
          'saveRDS(clean, "data/clean.rds")\nclean |> readr::write_csv("out/clean.csv")\n'
          'write.csv(clean, "out/clean_base.csv")\nDBI::dbWriteTable(con, "clean_tbl", clean)\n')
    write("R/02_report.R", 'library(dplyr)\nclean <- readRDS("data/clean.rds")\nclean |> filter(bigg)\n'
          'back <- readr::read_csv("out/clean.csv") |> select(regoin)\n'
          't <- dplyr::tbl(con, "clean_tbl") |> filter(yeer > 1)\n'
          'base <- read.csv("out/clean_base.csv") |> select(X, region, nope)\n'
          'missing <- readRDS("data/nothing.rds") |> filter(x > 1)\n')
    out = subprocess.run([tool, "--root", root, os.path.join(root, "R", "02_report.R")], capture_output=True, text=True, timeout=120)
    got = {(int(l.split(":")[1]), l.split("'")[1]) for l in out.stdout.splitlines() if l.startswith("R/02_report.R:") and "is not a column" in l}
    check(got == {(3, "bigg"), (4, "regoin"), (5, "yeer")}, f"readers of written files mis-checked: {sorted(got)}\n{out.stdout}")
    check("data from data/clean.rds (written by R/01_clean.R:4)" in out.stdout and "data from out/clean.csv (written by R/01_clean.R:5)" in out.stdout
          and "data from clean_tbl (written by R/01_clean.R:7)" in out.stdout, f"the source of a written file is not named:\n{out.stdout}")
    check("'nope'" not in out.stdout, f"write.csv()'s row-name column makes its file partly known; nothing may be reported against it:\n{out.stdout}")
    check("R/02_report.R:7: 'missing' comes from a source the checker cannot open (readRDS(\"data/nothing.rds\"): no file data/nothing.rds under the project, and no script in it writes that path)" in out.stdout,
          f"a path nothing writes should say so:\n{out.stdout}")
    cm = subprocess.run([colmap, "--root", root], cwd=root, capture_output=True, text=True, timeout=180).stdout
    check("Written by the code:\n  data/clean.rds   written at R/01_clean.R:4 (saveRDS)   3 columns: region, year, big   read by R/02_report.R" in cm
          and "  out/clean_base.csv   written at R/01_clean.R:6 (write.csv)   at least 3 columns: region, year, big   read by R/02_report.R" in cm
          and "  table clean_tbl   written at R/01_clean.R:7 (dbWriteTable)   3 columns: region, year, big   read by R/02_report.R" in cm
          and "data/clean.rds (written by R/01_clean.R:4)   first used at R/02_report.R:3   3 columns: region, year, big" in cm,
          f"colmap's written-data section unexpected:\n{cm}")
    cj = json.loads(subprocess.run([colmap, "--json", "--root", root], cwd=root, capture_output=True, text=True, timeout=180).stdout)
    written = [w for w in cj if w["kind"] == "written"]
    check({d["kind"] for d in cj} == {"data", "written"} and [w["path"] for w in written] == ["data/clean.rds", "out/clean.csv", "out/clean_base.csv", "clean_tbl"]
          and written[3]["table"] is True and written[0]["read_by"] == ["R/02_report.R"] and written[2]["open"] is True,
          f"colmap JSON written entries unexpected:\n{json.dumps(cj)[:800]}")
    # a targets pipeline
    write("_targets.R", "library(targets)\nlist(\n  tar_target(raw, readr::read_csv(\"data/sales.csv\")),\n"
          "  tar_target(summed, raw |> group_by(region) |> summarise(total = sum(amount))),\n"
          "  tarchetypes::tar_file_read(sales2, \"data/sales.csv\", readr::read_csv(!!.x))\n)\n")
    write("R/03_targets.R", "library(targets); library(dplyr)\ntar_load(summed)\nsummed |> filter(totl > 1)\n"
          "x <- tar_read(raw) |> select(amuont)\ny <- tar_read(sales2) |> select(customer, yeer)\ntar_load(c(raw, summed))\nraw |> select(region, total) |> mutate(z = summed)\n")
    out = subprocess.run([tool, "--root", root, os.path.join(root, "R", "03_targets.R")], capture_output=True, text=True, timeout=120)
    got = {(int(l.split(":")[1]), l.split("'")[1]) for l in out.stdout.splitlines() if l.startswith("R/03_targets.R:") and "is not a column" in l}
    check(got == {(3, "totl"), (4, "amuont"), (5, "yeer"), (7, "total")}, f"targets mis-checked: {sorted(got)}\n{out.stdout}")
    check("data from target summed (_targets.R:4)" in out.stdout and "data from target raw (_targets.R:3)" in out.stdout
          and "data from target sales2 (_targets.R:5)" in out.stdout, f"target sources not named with their line:\n{out.stdout}")
    steps = subprocess.run([tool, "--steps", "--root", root, os.path.join(root, "_targets.R")], capture_output=True, text=True, timeout=120).stdout
    check("_targets.R:4: summarise(., total = sum(amount)) => region, total" in steps, f"_targets.R steps unexpected:\n{steps}")
    # the map lists the targets as sources, and runs on a targets project without a checker error
    cm = subprocess.run([colmap, "--root", root], cwd=root, capture_output=True, text=True, timeout=180)
    check(cm.returncode == 0 and "target summed (_targets.R:4)   first used at R/03_targets.R:3   2 columns: region, total" in cm.stdout
          and "hit an error" not in cm.stdout, f"colmap on a targets project unexpected:\n{cm.stdout}{cm.stderr}")
    # --at FILE:LINE: the column set before and after each step on a line
    at = subprocess.run([colmap, "--root", root, "--at", "R/01_clean.R:3"], cwd=root, capture_output=True, text=True, timeout=180).stdout
    check("R/01_clean.R:3  mutate(., big = amount > 5)   frame from sales.csv" in at
          and "  in:  region, year, amount, customer\n  out: region, year, amount, customer, big   adds big (from amount)" in at
          and "R/01_clean.R:3  select(., region, year, big)" in at and "drops amount, customer" in at, f"colmap --at unexpected:\n{at}")
    check("No pipeline step with known columns covers R/01_clean.R:1" in subprocess.run([colmap, "--root", root, "--at", "R/01_clean.R:1"], cwd=root, capture_output=True, text=True, timeout=180).stdout,
          "colmap --at on a line outside every pipeline should say so")
    check(subprocess.run([colmap, "--root", root, "--at", "nope"], cwd=root, capture_output=True, text=True, timeout=60).returncode == 2, "colmap --at without FILE:LINE should exit 2")


def scenario_results_cache(s):
    """r-colcheck's results cache: a file's result is kept across processes while
    everything it depended on is unchanged (the file, the data it reads, the
    helper it follows, the writers that resolve its readers), so a second
    r-colmap costs a stat per file and says so; a change to any of those is
    seen; R_LSP_COLCHECK_CACHE=off reads and writes nothing; a corrupt cache is
    ignored; and the edit hook checks the files that depend on the edited one
    and reports what the edit broke there, once."""
    root = s.make_project("results_cache", {"data/sales.csv": os.path.join(FIXTURES, "columns", "data", "sales.csv")})
    colcheck = os.path.join(REPO, "plugins", "r-lsp", "bin", "r-colcheck")
    colmap = os.path.join(REPO, "plugins", "r-lsp", "bin", "r-colmap")
    hooks = os.path.join(REPO, "plugins", "r-lsp", "hooks")
    os.makedirs(os.path.join(root, "R"))

    def write(rel, text):
        with open(os.path.join(root, rel), "w") as f:
            f.write(text)

    def run(tool, *args, env=None):
        out = subprocess.run([tool, "--root", root, *args], cwd=root, capture_output=True, text=True, timeout=180,
                             env=dict(os.environ, **(env or {})))
        check(out.returncode in (0, 1), f"{os.path.basename(tool)} {' '.join(args)} exited {out.returncode}:\n{out.stderr}")
        return out.stdout

    write("R/helpers.R", "prep <- function(d) d |> dplyr::mutate(big = amount > 5) |> dplyr::select(region, year, big)\n")
    write("R/01_clean.R", 'library(dplyr)\nsource("R/helpers.R")\nsales <- readr::read_csv("data/sales.csv")\nclean <- prep(sales)\n'
          'readr::write_csv(clean, "out/clean.csv")\nclean |> select(customer)\n')
    write("R/02_report.R", 'library(dplyr)\nback <- readr::read_csv("out/clean.csv")\nback |> select(region, big)\nback |> select(nope)\n')
    write("R/03_other.R", 'library(dplyr)\nx <- readr::read_csv("data/other.csv") |> select(a)\n')
    cache_dir = os.path.join(os.environ["R_USER_CACHE_DIR"], "R", "r-lsp", "colcheck")

    first = run(colmap)
    check("from the cache" not in first.splitlines()[0], f"a cold run claims the cache:\n{first[:300]}")
    files = [f for f in os.listdir(cache_dir) if f.startswith("results_cache-") and f.endswith(".rds")] if os.path.isdir(cache_dir) else []
    check(len(files) == 1, f"cache file not written under R_USER_CACHE_DIR: {files}")
    cache_file = os.path.join(cache_dir, files[0])
    second = run(colmap)
    check("(4 of 4 files unchanged since the last run, from the cache)" in second.splitlines()[0], f"a warm run does not say so:\n{second[:300]}")
    check(first.splitlines()[1:] == second.splitlines()[1:], f"the cached map differs from the fresh one:\n{first}\n---\n{second}")
    report = run(colcheck, "R/02_report.R")
    check("R/02_report.R:4: 'nope' is not a column" in report and "written by R/01_clean.R:5" in report, f"reader of a written file unexpected:\n{report}")

    # the data file changes: its readers are checked again, the others served
    sales = os.path.join(root, "data", "sales.csv")
    with open(sales) as f:
        original = f.read()
    with open(sales, "w") as f:
        f.write(original.replace("customer", "cust", 1))
    third = run(colmap)
    hdr = third.splitlines()[0]
    check("from the cache" in hdr and "(4 of 4" not in hdr, f"a changed data file must re-check its readers: {hdr}")
    check("4 columns: region, year, amount, cust\n" in third or "region, year, amount, cust" in third, f"the new header is not in the map:\n{third}")
    with open(sales, "w") as f:
        f.write(original)
    # the helper changes: the file that follows it is checked again (its finding goes), and so is the reader of what it writes
    write("R/helpers.R", "prep <- function(d) d |> dplyr::mutate(big = amount > 5) |> dplyr::select(region, year, big, customer)\n")
    fourth = run(colmap)
    check("4 columns: region, year, big, customer" in fourth and "'customer' is not a column" not in run(colcheck, "R/01_clean.R"),
          f"a changed helper must re-check its callers:\n{fourth}")
    # a writer appears in a new file for a path a reader could not resolve: the reader is checked again
    write("R/00_make.R", 'library(dplyr)\nreadr::read_csv("data/sales.csv") |> select(region) |> readr::write_csv("data/other.csv")\n')
    fifth = run(colmap)
    check("data/other.csv (written by R/00_make.R:2)" in fifth and "(3 of 5 files unchanged since the last run, from the cache)" in fifth.splitlines()[0],
          f"a new writer must re-check the readers of its path and nothing else:\n{fifth[:600]}")
    # off: the same map, no cache note, the cache file untouched
    before = os.stat(cache_file).st_mtime
    off = run(colmap, env={"R_LSP_COLCHECK_CACHE": "off"})
    check("from the cache" not in off.splitlines()[0] and off.splitlines()[1:] == fifth.splitlines()[1:] and os.stat(cache_file).st_mtime == before,
          f"R_LSP_COLCHECK_CACHE=off must neither read nor write the cache:\n{off[:300]}")
    # a corrupt cache file is ignored and rewritten
    with open(cache_file, "w") as f:
        f.write("not an rds")
    sixth = run(colmap)
    check(sixth.splitlines()[1:] == fifth.splitlines()[1:] and "from the cache" not in sixth.splitlines()[0], f"a corrupt cache must be ignored:\n{sixth[:300]}")
    check("(5 of 5 files unchanged" in run(colmap).splitlines()[0], "the cache was not rewritten after a corrupt file")

    # the edit hook: 01_clean.R stops writing `big`, which 02_report.R selects
    data_dir = os.path.join(s.scratch, "results_cache_data")
    env = {"CLAUDE_PROJECT_DIR": root, "CLAUDE_PLUGIN_DATA": data_dir}
    rfile = os.path.join(root, "R", "01_clean.R")
    payload = {"session_id": "sess-results-cache", "cwd": root, "tool_name": "Edit", "tool_input": {"file_path": rfile}}
    write("R/01_clean.R", 'library(dplyr)\nsource("R/helpers.R")\nsales <- readr::read_csv("data/sales.csv")\nclean <- prep(sales) |> select(-big)\n'
          'readr::write_csv(clean, "out/clean.csv")\nclean |> select(customer)\n')
    ctx = hook_context(run_hook(os.path.join(hooks, "post-edit.sh"), payload, env=env))
    check("Elsewhere, after this change to R/01_clean.R" in ctx and "R/02_report.R (reads out/clean.csv):" in ctx
          and "R/02_report.R:3: 'big' is not a column" in ctx, f"the edit hook does not report what the edit broke elsewhere:\n{ctx}")
    check("'nope'" not in ctx, f"a finding the reader had before the edit is not the edit's doing:\n{ctx}")
    ctx2 = hook_context(run_hook(os.path.join(hooks, "post-edit.sh"), payload, env=env))
    check("Elsewhere" not in ctx2, f"the same state reported twice:\n{ctx2}")
    # the session start says what the data side is, from the warm cache
    out = run_hook(os.path.join(hooks, "session-start.sh"), {"session_id": "sess-results-cache", "cwd": root, "source": "startup"}, env=env)
    check("Data: 3 sources read by 5 files (sales.csv, out/clean.csv (written by R/01_clean.R:5), data/other.csv (written by R/00_make.R:2)), 2 written by the code." in out
          and "`r-colmap --diff` what a change touches" in out, f"session-start Data line unexpected:\n{out}")


def scenario_hooks_dormant(s):
    """In a directory without R code the plugin is dormant: session start
    remembers the answer (for the session, and for the directory for a day),
    the Bash, Stop, subagent and LSP hooks exit without walking the tree or
    starting R, a command that could have brought R code in looks again, an
    edit of an R file wakes the session up, R_LSP_HOOKS=off silences every
    hook, and a payload whose command output carries a "session_id" field
    cannot send the ledger to another session's directory."""
    hooks = os.path.join(REPO, "plugins", "r-lsp", "hooks")
    root = os.path.join(s.scratch, "dormant_py")
    os.makedirs(os.path.join(root, "src"))
    with open(os.path.join(root, "src", "main.py"), "w") as f:
        f.write("print(1)\n")
    data_dir = os.path.join(s.scratch, "dormant_data")
    env = {"CLAUDE_PROJECT_DIR": root, "CLAUDE_PLUGIN_DATA": data_dir}
    sid = "dormant-session"

    def hook(name, env=env, **kw):
        return run_hook(os.path.join(hooks, name), dict({"session_id": sid, "cwd": root}, **kw), env=env)

    check(not hook("session-start.sh", source="startup").strip(), "session-start spoke in a directory without R code")
    sdir = os.path.join(data_dir, "sessions", sid)
    check(os.path.exists(os.path.join(sdir, "no-r-project")) and os.path.exists(os.path.join(sdir, "start.stamp")),
          "session-start did not remember that the directory has no R code")
    projects = os.path.join(data_dir, "projects")
    check(os.path.isdir(projects) and any(os.path.exists(os.path.join(projects, d, "no-r-project")) for d in os.listdir(projects)),
          "session-start did not remember the directory across sessions")
    # a fake Rscript records every start: none of these hooks may start R
    fake = os.path.join(s.scratch, "dormant_fake")
    os.makedirs(fake, exist_ok=True)
    ran = os.path.join(fake, "ran")
    with open(os.path.join(fake, "Rscript"), "w") as f:
        f.write(f"#!/bin/sh\necho ran >> {ran}\nexit 0\n")
    os.chmod(os.path.join(fake, "Rscript"), 0o755)
    fake_env = dict(env, PATH=fake + os.pathsep + os.environ.get("PATH", ""))
    ts_file = os.path.join(root, "src", "a.ts")
    for name, kw in [("post-bash.sh", dict(tool_name="Bash", tool_input={"command": "ls"})),
                     ("stop-check.sh", dict(stop_hook_active=False)),
                     ("subagent-start.sh", dict(agent_id="a1", agent_type="general-purpose")),
                     ("session-start.sh", dict(source="resume")),
                     ("post-lsp.sh", dict(tool_name="LSP", tool_input={"operation": "findReferences", "filePath": ts_file, "line": 1, "character": 1},
                                          tool_response="No references found")),
                     ("post-lsp.sh", dict(tool_name="LSP", tool_input={"operation": "workspaceSymbol", "query": "x"}, tool_response="none"))]:
        out = hook(name, env=fake_env, **kw)
        check(not out.strip(), f"{name} spoke in a directory without R code:\n{out}")
    check(not os.path.exists(ran), "a hook started R in a directory without R code")
    # an R file appears: a plain command trusts the memory, one that could
    # have brought code in looks again and finds it
    time.sleep(1.1)
    new_r = os.path.join(root, "src", "new.R")
    with open(new_r, "w") as f:
        f.write("library(dplyr)\nx <- tibble::tibble(a = 1)\nx %>% select(zz)\n")
    check(not hook("post-bash.sh", tool_name="Bash", tool_input={"command": "ls"}).strip(),
          "post-bash walked a directory it remembered as having no R code")
    # a command that could bring code in looks again (even in the session's first minute,
    # the usual "clone the repository first" opening), at most once a minute
    ctx = hook_context(hook("post-bash.sh", tool_name="Bash", tool_input={"command": "git -C . checkout feature"}))
    check("this command changed 1 R file (src/new.R)" in ctx, f"post-bash did not look again after a checkout in the first minute:\n{ctx}")
    os.remove(os.path.join(sdir, "r-project"))
    with open(os.path.join(sdir, "no-r-project"), "w") as f:
        f.write(root)
    check(not hook("post-bash.sh", tool_name="Bash", tool_input={"command": "git checkout feature"}).strip(),
          "post-bash looked again within a minute of the last look")
    old = time.time() - 120
    os.utime(os.path.join(sdir, "last-recheck"), (old, old))
    time.sleep(1.1)
    with open(new_r, "a") as f:
        f.write("x %>% select(zz2)\n")
    ctx = hook_context(hook("post-bash.sh", tool_name="Bash", tool_input={"command": "git checkout feature"}))
    check("this command changed 1 R file (src/new.R)" in ctx and "'zz2' is not a column" in ctx,
          f"post-bash did not look again after a command that could bring R code in:\n{ctx}")
    check(os.path.exists(os.path.join(sdir, "r-project")) and not os.path.exists(os.path.join(sdir, "no-r-project")),
          "finding R code did not update the session's memory")
    # only the command decides a second look, never the command's output
    os.remove(os.path.join(sdir, "r-project"))
    with open(os.path.join(sdir, "no-r-project"), "w") as f:
        f.write(root)
    os.utime(os.path.join(sdir, "last-recheck"), (old, old))
    chatty = hook("post-bash.sh", tool_name="Bash", tool_input={"command": "npm test"},
                  tool_response={"stdout": "Installed with curl; see src/new.R and renv; running Rscript later\n", "stderr": ""})
    check(not chatty.strip() and os.path.exists(os.path.join(sdir, "no-r-project")),
          f"a command's output made post-bash look again:\n{chatty}")
    os.remove(os.path.join(sdir, "no-r-project"))
    open(os.path.join(sdir, "r-project"), "w").close()
    # a memory about another directory does not count: a Bash hook in this one looks
    os.remove(os.path.join(sdir, "r-project"))
    with open(os.path.join(sdir, "no-r-project"), "w") as f:
        f.write("/somewhere/else")
    ctx = hook_context(hook("post-bash.sh", tool_name="Bash", tool_input={"command": "ls"}))
    check("src/new.R" in ctx or os.path.exists(os.path.join(sdir, "r-project")), f"a marker written for another root was trusted:\n{ctx}")
    # a stale memory is corrected by an edit of an R file
    with open(os.path.join(sdir, "no-r-project"), "w") as f:
        f.write(root)
    if os.path.exists(os.path.join(sdir, "r-project")):
        os.remove(os.path.join(sdir, "r-project"))
    ctx = hook_context(hook("post-edit.sh", tool_name="Edit", tool_input={"file_path": new_r}))
    check("'zz' is not a column" in ctx and os.path.exists(os.path.join(sdir, "r-project")) and not os.path.exists(os.path.join(sdir, "no-r-project")),
          f"an edit of an R file did not wake the session up:\n{ctx}")
    elsewhere = os.path.join(s.scratch, "dormant_elsewhere.R")
    with open(elsewhere, "w") as f:
        f.write("x <- 1\n")
    os.remove(os.path.join(sdir, "r-project"))
    open(os.path.join(sdir, "no-r-project"), "w").close()
    hook("post-edit.sh", tool_name="Edit", tool_input={"file_path": elsewhere})
    check(os.path.exists(os.path.join(sdir, "no-r-project")) and not os.path.exists(os.path.join(sdir, "r-project")),
          "an R file edited outside the project marked the project as R")
    os.remove(os.path.join(sdir, "no-r-project"))
    open(os.path.join(sdir, "r-project"), "w").close()
    # /clear takes a fresh look: a directory remembered as having no R code for the day is walked again
    fresh_dir = os.path.join(s.scratch, "dormant_fresh")
    os.makedirs(fresh_dir)
    fenv = {"CLAUDE_PROJECT_DIR": fresh_dir, "CLAUDE_PLUGIN_DATA": data_dir}
    check(not run_hook(os.path.join(hooks, "session-start.sh"), {"session_id": "fresh-a", "cwd": fresh_dir, "source": "startup"}, env=fenv).strip(),
          "session-start spoke in an empty directory")
    with open(os.path.join(fresh_dir, "go.R"), "w") as f:
        f.write("x <- 1\n")
    check(not run_hook(os.path.join(hooks, "session-start.sh"), {"session_id": "fresh-b", "cwd": fresh_dir, "source": "resume"}, env=fenv).strip(),
          "a resumed session did not trust the day's memory")
    check("R LSP plugin:" in run_hook(os.path.join(hooks, "session-start.sh"), {"session_id": "fresh-c", "cwd": fresh_dir, "source": "clear"}, env=fenv),
          "/clear did not take a fresh look at a directory that gained R code")
    # the monitor keeps its memory where the hooks do
    monitor = os.path.join(hooks, "monitor-status.sh")
    mon_dir = os.path.join(s.scratch, "dormant_monitor")
    os.makedirs(mon_dir)
    mon_env = {k: v for k, v in os.environ.items() if k != "CLAUDE_PLUGIN_DATA"}
    idle = subprocess.run(["bash", monitor, data_dir], cwd=mon_dir, env=mon_env, capture_output=True, text=True, timeout=5)
    projects = os.path.join(data_dir, "projects")
    check(idle.returncode == 0 and any(open(os.path.join(projects, d, "no-r-project")).read() == "" for d in os.listdir(projects)
                                       if os.path.exists(os.path.join(projects, d, "no-r-project"))), "the monitor did not write its memory under the plugin data directory")
    # an edit of a .qmd without an R chunk neither wakes the session nor starts R
    py_qmd = os.path.join(root, "src", "notes.qmd")
    with open(py_qmd, "w") as f:
        f.write("---\ntitle: x\n---\n\n```{python}\nprint(1)\n```\n")
    os.remove(os.path.join(sdir, "r-project"))
    with open(os.path.join(sdir, "no-r-project"), "w") as f:
        f.write(root)
    out = run_hook(os.path.join(hooks, "post-edit.sh"), dict({"session_id": sid, "cwd": root}, tool_name="Edit", tool_input={"file_path": py_qmd}), env=fake_env)
    check(not out.strip() and os.path.exists(os.path.join(sdir, "no-r-project")) and not os.path.exists(ran),
          f"a .qmd without an R chunk woke the session or started R:\n{out}")
    os.remove(os.path.join(sdir, "no-r-project"))
    with open(os.path.join(sdir, "r-project"), "w") as f:
        f.write(root)
    # a Quarto project without R chunks is not an R project; one with an R chunk is
    qmd = os.path.join(s.scratch, "dormant_qmd")
    os.makedirs(qmd)
    with open(os.path.join(qmd, "index.qmd"), "w") as f:
        f.write("---\ntitle: x\n---\n\n```{python}\nprint(1)\n```\n")
    qenv = {"CLAUDE_PROJECT_DIR": qmd, "CLAUDE_PLUGIN_DATA": data_dir}
    check(not run_hook(os.path.join(hooks, "session-start.sh"), {"session_id": "qmd-py", "cwd": qmd, "source": "startup"}, env=qenv).strip()
          and os.path.exists(os.path.join(data_dir, "sessions", "qmd-py", "no-r-project")), "a Python-only Quarto project was taken for an R project")
    with open(os.path.join(qmd, "index.qmd"), "a") as f:
        f.write("\n```{r}\n1 + 1\n```\n")
    check("R LSP plugin:" in run_hook(os.path.join(hooks, "session-start.sh"), {"session_id": "qmd-r", "cwd": qmd, "source": "startup"}, env=qenv),
          "a Quarto project with an R chunk was not taken for an R project")
    # the post-LSP hook still speaks for an R file (the index is not running: an empty answer is said to be one)
    lsp_r = hook("post-lsp.sh", tool_name="LSP", tool_input={"operation": "findReferences", "filePath": new_r, "line": 2, "character": 1},
                 tool_response="No references found")
    check("r-lsp plugin note" in lsp_r, f"post-lsp hook was silent for an R file:\n{lsp_r}")
    # R_LSP_HOOKS=off: nothing, not even in an R project
    off = dict(env, R_LSP_HOOKS="off")
    for name, kw in [("session-start.sh", dict(source="startup")),
                     ("post-edit.sh", dict(tool_name="Edit", tool_input={"file_path": new_r})),
                     ("post-bash.sh", dict(tool_name="Bash", tool_input={"command": "git checkout x"})),
                     ("stop-check.sh", dict(stop_hook_active=False)),
                     ("post-lsp.sh", dict(tool_name="LSP", tool_input={"operation": "findReferences", "filePath": new_r, "line": 2, "character": 1},
                                          tool_response="No references found"))]:
        out = run_hook(os.path.join(hooks, name), dict({"session_id": "off-session", "cwd": root}, **kw), env=off)
        check(not out.strip(), f"{name} spoke with R_LSP_HOOKS=off:\n{out}")
    check(not os.path.exists(os.path.join(data_dir, "sessions", "off-session")), "a hook wrote a session directory with R_LSP_HOOKS=off")
    out = run_hook(os.path.join(hooks, "subagent-start.sh"), {"session_id": "off-session", "cwd": root, "agent_id": "a1"}, env=off)
    check(not out.strip(), f"subagent-start.sh spoke with R_LSP_HOOKS=off:\n{out}")
    monitor = os.path.join(hooks, "monitor-status.sh")
    try:
        idle = subprocess.run(["bash", monitor, data_dir], cwd=root, env=dict(os.environ, **off), capture_output=True, text=True, timeout=5)
        check(idle.returncode == 0 and not idle.stdout, f"the monitor ran with R_LSP_HOOKS=off: {idle.returncode} {idle.stdout!r}")
    except subprocess.TimeoutExpired:
        check(False, "the monitor kept running with R_LSP_HOOKS=off")
    # R code but no Rscript on PATH: one line says so, instead of silence
    tools_dir = os.path.join(s.scratch, "dormant_tools")
    os.makedirs(tools_dir, exist_ok=True)
    for tool in ("bash", "sh", "cat", "find", "grep", "sed", "head", "cut", "tr", "cksum", "stat", "date", "mkdir", "dirname", "basename",
                 "readlink", "touch", "rm", "xargs", "paste", "pwd", "uname", "env", "mktemp", "sort", "wc"):
        src = shutil.which(tool)
        if src and not os.path.exists(os.path.join(tools_dir, tool)):
            os.symlink(src, os.path.join(tools_dir, tool))
    no_r = run_hook(os.path.join(hooks, "session-start.sh"), {"session_id": "no-r-session", "cwd": root, "source": "startup"},
                    env=dict(env, PATH=tools_dir))
    check("R LSP plugin: this project has R code but Rscript is not on the PATH" in no_r, f"session-start was silent without Rscript:\n{no_r!r}")
    # the session id is the payload's own, never one that a command printed
    time.sleep(1.1)
    with open(os.path.join(root, "src", "second.R"), "w") as f:
        f.write("y <- 1\n")
    payload = {"session_id": sid, "cwd": root, "tool_name": "Bash", "tool_input": {"command": "cat state.json"},
               "tool_response": {"stdout": '{"session_id": "evil", "cwd": "/nowhere"}', "stderr": ""}}
    ctx = hook_context(run_hook(os.path.join(hooks, "post-bash.sh"), payload, env=env))
    check("src/second.R" in ctx, f"post-bash did not check the new file:\n{ctx}")
    with open(os.path.join(sdir, "checked.tsv")) as f:
        ledger = f.read()
    check("second.R" in ledger and not os.path.exists(os.path.join(data_dir, "sessions", "evil")),
          "a session_id inside the command's output hijacked the session directory")


SCENARIOS = [scenario_bugs, scenario_tidyverse, scenario_renv,
             scenario_renv_old_lintr, scenario_renv_old_purrr,
             scenario_complete_answers, scenario_cold_hover, scenario_shared_scope, scenario_stale_files, scenario_other_repository,
             scenario_colcheck, scenario_colcheck_cache, scenario_colcheck_packages, scenario_lint_fallback, scenario_callmap, scenario_colmap,
             scenario_hooks, scenario_hooks_changes, scenario_hooks_dormant, scenario_colcheck_flow, scenario_callmap_cli,
             scenario_results_cache]


def check_manifests():
    """The plugin's version is written in two manifests; a release moves both."""
    with open(os.path.join(REPO, "plugins", "r-lsp", ".claude-plugin", "plugin.json")) as f:
        plugin = json.load(f)["version"]
    with open(os.path.join(REPO, ".claude-plugin", "marketplace.json")) as f:
        market = json.load(f)["plugins"][0]["version"]
    check(plugin == market, f"plugin.json says {plugin}, marketplace.json says {market}")
    with open(os.path.join(REPO, "plugins", "r-lsp", "hooks", "hooks.json")) as f:
        hooks = json.load(f)["hooks"]
    for event, entries in hooks.items():
        for entry in entries:
            for h in entry["hooks"]:
                script = h["command"].replace("${CLAUDE_PLUGIN_ROOT}", os.path.join(REPO, "plugins", "r-lsp")).strip('"')
                check(os.access(script, os.X_OK), f"{event} hook {script} is missing or not executable")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-k", default="", help="run scenarios whose name contains this")
    parser.add_argument("--keep", action="store_true", help="keep the scratch directory")
    parser.add_argument("--no-skip", action="store_true", help="a skipped scenario counts as a failure (for CI)")
    args = parser.parse_args()

    # lintr is needed by most scenarios; the language server only by those
    # that start one (they skip without it, so the checker, the maps and the
    # hooks can be tested on a machine with R and the tidyverse alone)
    try:
        require_packages("lintr")
    except (Skip, RuntimeError, FileNotFoundError) as e:
        print(f"cannot run the smoke test: {e}")
        return 2
    check_manifests()
    scratch = tempfile.mkdtemp(prefix="r-lsp-smoke-")
    # the hooks and the pre-warm write under the plugin data directory and
    # R's user cache: a scratch copy of each, so a run leaves the machine
    # as it was and sees no state from an earlier run or a real session
    os.environ["CLAUDE_PLUGIN_DATA"] = os.path.join(scratch, "plugin-data")
    os.environ["CLAUDE_CODE_SESSION_ID"] = ""
    os.environ["R_USER_CACHE_DIR"] = os.path.join(scratch, "r-cache")
    os.environ.pop("R_LSP_HOOKS", None)
    s = Scenario(scratch)
    failures = passed = skipped = ran = 0
    for scenario in SCENARIOS:
        name = scenario.__name__.replace("scenario_", "")
        if args.k not in name:
            continue
        ran += 1
        try:
            scenario(s)
            passed += 1
            print(f"PASS  {name}")
        except Skip as e:
            if args.no_skip:
                failures += 1
                print(f"FAIL  {name}: skipped: {e}")
            else:
                skipped += 1
                print(f"SKIP  {name}: {e}")
        except Exception as e:  # noqa: BLE001
            failures += 1
            print(f"FAIL  {name}: {e}")
            if not isinstance(e, AssertionError):
                traceback.print_exc()
    if not ran:
        print(f"no scenario matches {args.k!r}; scenarios: {', '.join(f.__name__.replace('scenario_', '') for f in SCENARIOS)}")
        return 2
    print(f"{passed} passed, {skipped} skipped, {failures} failed")
    if args.keep or failures:
        print(f"scratch dir (server logs in *.stderr.log): {scratch}")
    else:
        shutil.rmtree(scratch, ignore_errors=True)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
