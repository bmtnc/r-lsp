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
    finally:
        lsp.close()
    files = {u.rsplit("/", 1)[-1] for u in uris}
    check("other.R" in files, f"references in the other repository missed R/other.R: {sorted(files)}")
    check(any(x.get("name") == "gross" for x in sym), "workspace symbol did not find gross() in the other repository")
    with open(status_file) as f:
        check("added workspace" in f.read(), "status log has no 'added workspace' event")


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
    hook = os.path.join(REPO, "plugins", "r-lsp", "hooks", "post-edit.sh")
    ctx = hook_context(run_hook(hook, {"tool_name": "Edit", "tool_input": {"file_path": analysis}, "cwd": root},
                                env={"CLAUDE_PROJECT_DIR": root}))
    check("'amt'" in ctx and "regoin" in ctx, f"post-edit hook context missing findings:\n{ctx[:400]}")
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


def scenario_hooks(s):
    """SessionStart context + index pre-warm, and the note after LSP results."""
    root = s.make_package("hooks_pkg")
    start = os.path.join(REPO, "plugins", "r-lsp", "hooks", "session-start.sh")
    out = run_hook(start, {}, env={"CLAUDE_PROJECT_DIR": root}, stdin_text="{}")
    check("R LSP plugin: R package 'pkgtwo'" in out, f"session-start context unexpected:\n{out}")
    cache = rscript(f'cat(file.path(tools::R_user_dir("languageserver", "cache"), "workspace-index", '
                    f'paste0(digest::digest(normalizePath("{root}"), algo = "xxhash64"), ".rds")))').strip()
    for _ in range(60):
        if os.path.exists(cache):
            break
        time.sleep(0.5)
    check(os.path.exists(cache), f"pre-warm did not create the index cache {cache}")
    lsp_hook = os.path.join(REPO, "plugins", "r-lsp", "hooks", "post-lsp.sh")
    payload = {"tool_name": "LSP", "cwd": root,
               "tool_input": {"operation": "findReferences", "filePath": os.path.join(root, "R", "tax.R"),
                              "line": 5, "character": 1},
               "tool_response": "Found 3 references across 2 files:\nR/tax.R: Line 5:1, Line 9:20\nR/other.R: Line 1:28"}
    ctx = hook_context(run_hook(lsp_hook, payload, env={"CLAUDE_PROJECT_DIR": root}))
    check("tests/testthat/test-tax.R" in ctx, f"post-lsp note missed the test file:\n{ctx}")
    status_file = os.path.join(s.scratch, "hooks.status.log")
    with open(status_file, "w") as f:
        f.write(f"12:00:00 [{root}] R LSP: workspace: indexing 3 files\n")
    payload["tool_response"] = "No references found. This may occur if the symbol has no usages"
    ctx = hook_context(run_hook(lsp_hook, payload, env={"CLAUDE_PROJECT_DIR": root, "R_LSP_STATUS_FILE": status_file}))
    check("still indexing" in ctx, f"post-lsp note did not report the running index:\n{ctx}")
    covered = dict(payload, tool_response="R/tax.R R/other.R tests/testthat/test-tax.R")
    quiet = run_hook(lsp_hook, covered, env={"CLAUDE_PROJECT_DIR": root})
    check(not quiet.strip(), f"post-lsp hook spoke although every file was covered:\n{quiet}")


SCENARIOS = [scenario_bugs, scenario_tidyverse, scenario_renv,
             scenario_renv_old_lintr, scenario_renv_old_purrr,
             scenario_complete_answers, scenario_other_repository,
             scenario_colcheck, scenario_colcheck_cache, scenario_hooks]


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
