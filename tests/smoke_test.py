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
import os
import shutil
import subprocess
import sys
import tempfile
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

    def server(self, root, cwd=None):
        log = os.path.join(self.scratch, os.path.basename(root) + ".stderr.log")
        client = LspClient([WRAPPER], root, cwd=cwd, stderr_path=log)
        client.initialize()
        return client

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


SCENARIOS = [scenario_bugs, scenario_tidyverse, scenario_renv,
             scenario_renv_old_lintr, scenario_renv_old_purrr]


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
