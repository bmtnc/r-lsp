#!/usr/bin/env python3
"""End-to-end smoke test for the r-lsp plugin.

Starts plugins/r-lsp/bin/r-lsp-wrapper on scratch projects, the way Claude
Code does, and checks diagnostics and hover. The wrapper patches internals of
the `languageserver` R package, so this is also the early warning when a new
languageserver release breaks those patches.

Needs R on PATH with languageserver, lintr and renv installed; the tidyverse
scenario also needs tidyverse. Scenarios whose packages are missing are
skipped, not failed.

    python3 tests/smoke_test.py            # run everything
    python3 tests/smoke_test.py -k renv    # scenarios whose name contains "renv"
    python3 tests/smoke_test.py --keep     # keep the scratch dir for debugging
"""

import argparse
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
    def __init__(self, scratch, keep_logs):
        self.scratch = scratch
        self.keep_logs = keep_logs

    def server(self, root, cwd=None, env=None):
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


def run_hook(script, payload, env=None, stdin_text=None):
    """Run a plugin hook/bin script with a JSON payload on stdin; return stdout."""
    full_env = dict(os.environ, **(env or {}))
    out = subprocess.run([script], input=stdin_text if stdin_text is not None else json.dumps(payload),
                         capture_output=True, text=True, env=full_env, timeout=120)
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
            if len({i["from"]["uri"].rsplit("/", 1)[-1] for i in incoming}) >= 2:
                break
            time.sleep(1)
        outgoing = lsp.request("callHierarchy/outgoingCalls", {"item": items[0]}).get("result") or [] if items else []
        refs = references(lsp, "R/flags.R", 3, 1)
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
    check({"run.R", "test-flags.R"} <= callers,
          f"incomingCalls missed callers in scripts/ or tests/ that nothing had opened: {sorted(callers)}")
    ref_files = {u.rsplit("/", 1)[-1] for u in refs}
    check({"run.R", "test-flags.R"} <= ref_files, f"references missed scripts/ or tests/: {sorted(ref_files)}")
    called = {o["to"]["name"] for o in outgoing}
    check({"is.na", "sum", "as.character"} <= called,
          f"outgoingCalls dropped calls inside `!`, unary minus or a subscript: {sorted(called)}")
    check("flag_missing" in hover, f"hover from a script into R/ is empty (document scope not shared): {hover[:80]!r}")
    check(bool(defn) and "flags.R" in json.dumps(defn), f"definition from a script did not reach R/flags.R: {defn}")
    with open(status_file) as f:
        check("test/script files as documents" in f.read(), "status log has no 'loaded ... test/script files' event")


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
    analysis = os.path.join(root, "R", "analysis.R")
    out = subprocess.run([tool, "--root", root, analysis], capture_output=True, text=True, timeout=120).stdout
    reported = {int(line.split(":")[1]) for line in out.splitlines() if line.startswith("R/analysis.R:")}
    with open(analysis) as f:
        src = f.read().splitlines()
    bad = {i for i, line in enumerate(src, 1) if line.rstrip().endswith("# bad")}
    check(bad <= reported, f"r-colcheck missed lines {sorted(bad - reported)}:\n{out}")
    check(reported <= bad, f"r-colcheck flagged correct lines {sorted(reported - bad)}:\n{out}")
    tricky = subprocess.run([tool, "--root", root, os.path.join(root, "R", "tricky_ok.R")],
                            capture_output=True, text=True, timeout=120).stdout
    check(not tricky.strip(), f"r-colcheck flagged correct tricky code:\n{tricky}")

    # Frames passed through project functions (same file, source()d file), both pipes.
    funcs = os.path.join(root, "R", "funcs.R")
    out = subprocess.run([tool, "--root", root, funcs], capture_output=True, text=True, timeout=120).stdout
    reported = {int(line.split(":")[1]) for line in out.splitlines() if line.startswith("R/funcs.R:")}
    with open(funcs) as f:
        bad = {i for i, line in enumerate(f.read().splitlines(), 1) if line.rstrip().endswith("# bad")}
    check(bad <= reported, f"function tracking missed lines {sorted(bad - reported)}:\n{out}")
    check(reported <= bad, f"function tracking flagged correct lines {sorted(reported - bad)}:\n{out}")
    check("helpers_src.R" not in out, f"findings attributed to the sourced helper file:\n{out}")

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
    check("1 pipeline step(s) checked" in run.stderr and "3 finding(s)" in run.stderr, f"summary line unexpected:\n{run.stderr}")
    clean = subprocess.run([tool, "--root", pkg, os.path.join(pkg, "R", "load.R")], capture_output=True, text=True, timeout=120)
    check(clean.returncode == 0 and not clean.stdout.strip() and "r-colcheck:" in clean.stderr,
          f"clean file: exit {clean.returncode}, stdout {clean.stdout!r}, stderr {clean.stderr!r}")
    hook = os.path.join(REPO, "plugins", "r-lsp", "hooks", "post-edit.sh")
    ctx = hook_context(run_hook(hook, {"tool_name": "Edit", "tool_input": {"file_path": analysis}, "cwd": root},
                                env={"CLAUDE_PROJECT_DIR": root}))
    check("'amt'" in ctx and "regoin" in ctx, f"post-edit hook context missing findings:\n{ctx[:400]}")
    ctx = hook_context(run_hook(hook, {"tool_name": "Edit", "tool_input": {"file_path": os.path.join(pkg, "R", "calls.R")},
                                       "cwd": pkg}, env={"CLAUDE_PROJECT_DIR": pkg}))
    check("argument names do not match" in ctx and "no argument 'rows'" in ctx, f"post-edit hook missed argument findings:\n{ctx[:600]}")
    silent = run_hook(hook, {"tool_name": "Edit", "tool_input": {"file_path": "/tmp/x.py"}})
    check(not silent.strip(), "post-edit hook spoke for a non-R file")


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
    listing = subprocess.run([tool, "schemas", "--root", root], capture_output=True, text=True, timeout=120).stdout
    check("holdings/latest  3 columns" in listing, f"schemas listing unexpected:\n{listing}")

    hook = os.path.join(REPO, "plugins", "r-lsp", "hooks", "post-edit.sh")
    ctx = hook_context(run_hook(hook, {"tool_name": "Edit", "tool_input": {"file_path": report}, "cwd": root},
                                env={"CLAUDE_PROJECT_DIR": root}))
    check("'wieght'" in ctx and "r-colcheck learn" in ctx, f"post-edit hook context missing cache findings:\n{ctx[:600]}")

    fresh = s.make_project("s3fresh", {"R/loaders.R": os.path.join(FIXTURES, "s3columns", "R", "loaders.R")})
    first = subprocess.run([tool, "learn", "meta/funds", 's3_read("meta/funds")', "--source", "R/loaders.R"],
                           cwd=os.path.join(fresh, "R"), capture_output=True, text=True, timeout=120)
    check(first.returncode == 0, f"learn from a subdirectory failed:\n{first.stderr}")
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


def scenario_callmap(s):
    """r-callmap: the static call map of a package, its neighbourhood view and
    JSON, with no server. The chain avoids one-letter names: a project function
    called `c` would rightly attract every c() call in the fixture."""
    root = os.path.join(s.scratch, "callmap")
    files = {
        "DESCRIPTION": "Package: callmapfix\nTitle: Fixture\nVersion: 0.0.1\nDescription: r-callmap fixture.\nLicense: MIT\n",
        "NAMESPACE": "export(outer_step)\nexport(run_all)\nS3method(print, myclass)\n",
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
        "tests/testthat/test-a.R": "test_that(\"outer_step works\", {\n  expect_equal(outer_step(1), 5)\n})\n",
        "scripts/run.R": "source(\"R/apply.R\")\nres <- run_all(list(1, 2))\nprint(res)\n",
        "vignettes/intro.Rmd": "---\ntitle: x\n---\n\n```{r}\nouter_step(1)\n```\n",
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
    check("static call map of" in lines[0] and "1 Rmd/qmd not parsed" in lines[0] and "11 functions in 5 files" in lines[0],
          f"header unexpected:\n{full[:600]}")

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
    check("calls: middle_step" in outer and "[exported]" in outer and "called by: tests/testthat/test-a.R" in outer,
          f"outer_step line unexpected:\n{outer}")
    check("calls: inner_step" in middle and "called by: outer_step" in middle and "[exported]" not in middle,
          f"middle_step line unexpected:\n{middle}")
    check("called by: middle_step" in inner and "wrap" not in inner, f"inner_step line unexpected (a parameter is not the function):\n{inner}")
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
    check("outer_step [exported]" in entry and "run_all [exported]" in entry and "middle_step" not in entry,
          f"entry points unexpected:\n{entry}")
    check("orphan" in unref and "wrap" in unref and "print.myclass" not in unref and "outer_step" not in unref,
          f"unreferenced unexpected:\n{unref}")
    check("Recursion/cycles" not in full, f"cycles reported for an acyclic fixture:\n{full}")

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
    for unrelated in ("helper", "run_all", "orphan", "wrap", "describe"):
        check(unrelated not in body, f"neighbourhood of inner_step mentions {unrelated}:\n{near}")
    missing = run("innr_step")
    check("not defined in the project" in missing and "inner_step" in missing, f"unknown name output unexpected:\n{missing}")

    data = json.loads(run("--json"))
    by_name = {d["name"]: d for d in data}
    check(len(data) == 11 and by_name["outer_step"]["exported"] is True and by_name["print.myclass"]["kind"] == "s3_method",
          f"JSON map unexpected:\n{json.dumps(data)[:800]}")
    check({"name": "helper", "kind": "value", "line": 2} in by_name["run_all"]["calls"] and
          any(c["by"] == "scripts/run.R" and c["line"] == 2 for c in by_name["run_all"]["called_by"]),
          f"JSON edges unexpected:\n{json.dumps(by_name['run_all'])}")
    sub = json.loads(run("--json", "inner_step"))
    check(sorted(d["name"] for d in sub) == ["inner_step", "middle_step", "outer_step"], f"JSON neighbourhood unexpected:\n{sub}")


def scenario_hooks(s):
    """SessionStart context + index pre-warm, the note after LSP results, and
    r-lsp-status. The LSP payloads are in the shapes Claude Code's LSP tool
    prints (captured from real sessions): a file line followed by indented
    "Line N:M" lines, call-hierarchy items with "[calls at: ...]", and
    "Defined in FILE:N:M"."""
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


SCENARIOS = [scenario_bugs, scenario_tidyverse, scenario_renv,
             scenario_renv_old_lintr, scenario_renv_old_purrr,
             scenario_complete_answers, scenario_shared_scope, scenario_other_repository,
             scenario_colcheck, scenario_colcheck_cache, scenario_callmap, scenario_hooks]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-k", default="", help="run scenarios whose name contains this")
    parser.add_argument("--keep", action="store_true", help="keep the scratch directory")
    args = parser.parse_args()

    require_packages("languageserver", "lintr")
    scratch = tempfile.mkdtemp(prefix="r-lsp-smoke-")
    s = Scenario(scratch, args.keep)
    failures = 0
    for scenario in SCENARIOS:
        name = scenario.__name__.replace("scenario_", "")
        if args.k not in name:
            continue
        try:
            scenario(s)
            print(f"PASS  {name}")
        except Skip as e:
            print(f"SKIP  {name}: {e}")
        except Exception as e:  # noqa: BLE001
            failures += 1
            print(f"FAIL  {name}: {e}")
            if not isinstance(e, AssertionError):
                traceback.print_exc()
    if args.keep or failures:
        print(f"scratch dir (server logs in *.stderr.log): {scratch}")
    else:
        shutil.rmtree(scratch, ignore_errors=True)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
