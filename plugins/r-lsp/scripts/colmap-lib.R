# colmap: the columns of every data source the project reads and where the
# code uses, creates or drops each one; the data-side counterpart of
# r-callmap. Built from r-colcheck's step records, so it has the checker's
# tracking and its limits: a pipeline the checker cannot follow (a frame
# from a source it cannot open and has no schema for, a step that makes the
# column set unknowable) is not here, and the summary says how many steps
# that was.
#
# A column the code creates and no followed step reads is not called unused:
# the verdict says where the frame carrying it was last seen (returned by a
# function whose callers the checker did not reach, written by a sink,
# tracking ended at a step), since that is where the use may be.
#
# Besides data sources, a parameter a named function pipes into a verb is a
# source of its own ("parameter df of f()"): the columns its body reads are
# what the function expects of the frame, and what it creates and returns is
# reported like any other. A join's other frame keeps its own source: its
# keys are used at the join, its other columns carried into the result.
#
# Entry points: colmap_build(root, files) -> map; colmap_format(map, names)
#               -> character lines; colmap_json(map, names) -> list.

# "cached schema \"k\", observed 2025-01-15" and "cached schema \"k\"" are one source.
source_key <- function(src) sub(", observed [0-9-]+$", "", src)

# A parameter of a named function that its body uses as a frame is a source
# of its own: id "parameter:f:df", label "parameter df of f()".
is_parameter_source <- function(key) startsWith(key, "parameter:") || startsWith(key, "parameter ")

# verbs that add the columns they name even on a partly known input
creating_verbs <- c("mutate", "transmute", "summarise", "summarize", "reframe", "count", "add_count", "tally",
                    "add_tally", "add_column", "rownames_to_column", "rowid_to_column", "unite", "pivot_longer",
                    "mutate_at", "mutate_if", "mutate_all", "group_by", "nest")

# The source a frame comes from, as the map keys it: the structured id when
# the step has one (a path, a cache key, a literal's text, a parameter), else
# the label, a literal's with where it is (tibble() at R/a.R:3 and tibble()
# at R/b.R:9 differ). A literal is placed in the file it is in (`source_file`:
# a literal in a test, followed into R/, is still the test's), else the
# step's file (`rel`, relative to the root; passed when the step was read
# from a file under another name).
source_key_of <- function(src, id, source_line, source_file, rel, root) {
  if (is.null(src) || is.na(src) || !nzchar(src)) return(NULL)
  if (!is.null(id) && !is.na(id) && nzchar(id)) return(id)
  source_label_of(src, source_line, source_file, rel, root)
}
source_label_of <- function(src, source_line, source_file, rel, root) {
  key <- source_key(src)
  if (!is.null(source_line) && !is.na(source_line) && grepl(" literal$", key)) {
    at <- if (!is.null(source_file) && !is.na(source_file) && nzchar(source_file)) relative_path(source_file, root) else rel
    key <- sprintf("%s at %s:%d", key, at, source_line)
  }
  key
}
step_source_key <- function(s, root, rel = relative_path(s$file, root)) source_key_of(s$source, s$source_id, s$source_line, s$source_file, rel, root)
step_source_label <- function(s, root, rel = relative_path(s$file, root)) source_label_of(s$source, s$source_line, s$source_file, rel, root)
joined_source_key <- function(j, s, root) source_key_of(j$source, j$source_id, j$source_line, j$source_file, relative_path(s$file, root), root)
joined_source_label <- function(j, s, root) source_label_of(j$source, j$source_line, j$source_file, relative_path(s$file, root), root)

# The columns a step adds to its frame (not a grouping column summarise()
# keeps, which the input had, nor what a join brought from the other frame)
# and the ones it drops. On a partly known input ("at least these") only
# what a creating verb names and did not read counts as added, and nothing
# as dropped; nothing on a listed input; a result with no columns is not a
# frame (deframe(), pull()), so nothing was dropped there.
step_created <- function(s) {
  if (isTRUE(s$terminal) || !is.null(s$fate) || is.null(s$columns) || is.null(s$before) || isTRUE(s$listed)) return(character())
  new <- setdiff(setdiff(s$columns, s$before), c(s$groups_before %||% character(), s$joined$brought %||% character()))
  if (!isTRUE(s$open_before)) return(new)
  verb <- sub("^.*::", "", sub("\\(.*$", "", s$call))
  if (verb %in% creating_verbs) setdiff(new, s$used) else character()
}
step_dropped <- function(s) {
  if (isTRUE(s$terminal) || !is.null(s$fate) || is.null(s$columns) || is.null(s$before) || isTRUE(s$listed) || isTRUE(s$open_before) || !length(s$columns)) return(character())
  setdiff(s$before, s$columns)
}

colmap_build <- function(root, files = NULL) {
  root <- normalizePath(root, mustWork = FALSE)
  if (is.null(files)) {
    found <- project_r_files(root)
    files <- c(found$r, found$rmd)
  }
  findings <- colcheck_files(files, root)
  # a name r-colcheck reports is a mistake, not a column the code uses or
  # creates: within the statement that reports it, that name is left out of
  # the map (a misspelt grouping column travels into summarise()'s output in
  # the same pipeline; the same name used rightly elsewhere stays)
  bad <- mistakes_of(findings)
  sources <- list()
  entry_for <- function(key, label, loc) {
    sources[[key]] %||% list(key = label, id = key, kind = if (is_parameter_source(key)) "parameter" else "data",
                             origin = NULL, first = loc, columns = character(), sites = list(), fates = list(),
                             n_steps = 0L, files = character())
  }
  for (s in attr(findings, "steps")) {
    key <- step_source_key(s, root)
    if (is.null(key)) next
    rel <- relative_path(s$file, root)
    loc <- sprintf("%s:%d", rel, s$line)
    entry <- entry_for(key, step_source_label(s, root), loc)
    # the loader's columns: the input of the first step that has one (a
    # parameter's are worked out at the end, from what the body reads)
    if (is.null(entry$origin) && !is.null(s$before) && entry$kind == "data") entry$origin <- s$before
    entry$n_steps <- entry$n_steps + 1L
    entry$files <- union(entry$files, rel)
    entry$columns <- union(entry$columns, c(s$before, s$columns))
    verb <- sub("\\(.*$", "", s$call)
    bare_verb <- sub("^.*::", "", verb)
    # `from`: the input columns a created column derives from (the step's
    # lineage record); `joined_from`: the source a joined column came from;
    # `into`: the frame a carried column went into
    add <- function(col, kind, from = NULL, joined_from = NULL, into = NULL) {
      if (paste(s$file, s$line, col) %in% bad) return(invisible())
      site <- list(loc = loc, verb = verb, kind = kind, call = s$call, from = as.character(from %||% character()))
      if (!is.null(joined_from)) site$joined_from <- joined_from
      if (!is.null(into)) site$into <- into
      entry$sites[[col]] <<- c(entry$sites[[col]], list(site))
    }
    if (!is.null(s$fate)) {
      # where the frame went: every column it carried shares the fate
      entry$fates <- c(entry$fates, list(list(loc = loc, kind = s$fate, fn = s$fate_fn, call = s$call, columns = s$before)))
      sources[[key]] <- entry
      next
    }
    for (u in s$used) add(u, "used")
    for (o in s$overwritten %||% character()) add(o, "overwritten", s$derived[[o]])
    if (is.null(s$columns) && !is.null(s$before) && !isTRUE(s$terminal) && bare_verb != "ggplot" && !grepl("^\\. \\+ ", verb)) {
      # a verb the checker could not follow: tracking of this frame ends here
      # (a plot is not a frame, and pull()/nrow() end it on purpose: nothing was lost there)
      entry$fates <- c(entry$fates, list(list(loc = loc, kind = "lost", fn = verb, call = s$call, columns = s$before)))
    }
    for (cn in step_created(s)) add(cn, "created", s$derived[[cn]])
    for (cn in step_dropped(s)) add(cn, "dropped")
    brought <- s$joined$brought %||% character()
    from_key <- if (length(brought)) joined_source_key(s$joined, s, root) else NULL
    from_label <- if (!is.null(from_key)) joined_source_label(s$joined, s, root) else "another frame"
    for (cn in brought) add(cn, "joined", joined_from = from_label)
    sources[[key]] <- entry
    if (!is.null(from_key)) {
      # the other frame's own entry: its keys were read here, its other
      # columns carried into this frame's pipeline
      other <- entry_for(from_key, from_label, loc)
      if (is.null(other$origin) && other$kind == "data") other$origin <- s$joined$columns
      other$n_steps <- other$n_steps + 1L
      other$files <- union(other$files, rel)
      other$columns <- union(other$columns, s$joined$columns)
      for (k in s$joined$keys) other$sites[[k]] <- c(other$sites[[k]], list(list(loc = loc, verb = verb, kind = "used", call = s$call, from = character())))
      for (cn in setdiff(s$joined$columns, s$joined$keys)) {
        other$sites[[cn]] <- c(other$sites[[cn]], list(list(loc = loc, verb = verb, kind = "carried", call = s$call, from = character(), into = entry$key)))
      }
      sources[[from_key]] <- other
    }
  }
  for (k in names(sources)) {
    e <- sources[[k]]
    # a site reached twice (a body walked on its own and followed from a caller) is one site
    e$sites <- lapply(e$sites, function(sites) sites[!duplicated(vapply(sites, function(x) paste(x$loc, x$kind, x$call, paste(x$from, collapse = ","), x$joined_from %||% "", x$into %||% ""), ""))])
    if (e$kind == "parameter") {
      # what the body reads of the frame it was given, less what it made itself
      kinds <- lapply(e$sites, function(sites) vapply(sites, `[[`, "", "kind"))
      made <- names(kinds)[vapply(kinds, function(x) "created" %in% x || "joined" %in% x, TRUE)]   # carried out is not made
      e$origin <- setdiff(names(kinds)[vapply(kinds, function(x) "used" %in% x, TRUE)], made)
    } else if (is.null(e$origin)) e$origin <- e$columns
    sources[[k]] <- e
  }
  # a parameter whose body reads and adds nothing the map can name says nothing
  sources <- Filter(function(e) e$kind == "data" || length(e$origin) || length(e$sites), sources)
  # a column assertion on an unknown frame is not data: listed apart, as what the code asserts
  is_assertion <- startsWith(names(sources) %||% character(), "assertion:")
  asserted <- sources[is_assertion]
  sources <- sources[!is_assertion]
  labels <- vapply(sources, `[[`, "", "key")
  for (k in names(sources)) if (sum(labels == sources[[k]]$key) > 1L && startsWith(k, "file:")) {
    sources[[k]]$key <- relative_path(sub("^file:", "", k), root)
  }
  # data sources first, in the order the code meets them, then the parameters
  first_file <- vapply(sources, function(e) sub(":[0-9]+$", "", e$first), "")
  first_line <- vapply(sources, function(e) as.integer(sub("^.*:", "", e$first)), 1L)
  sources <- sources[order(vapply(sources, function(e) e$kind == "parameter", TRUE), first_file, first_line, method = "radix")]
  # the files and tables the code writes, with the sources that read them
  # (a reader resolved from a writer has the key "<path> (written by file:line)")
  sinks <- lapply(attr(findings, "sinks") %||% list(), function(s) {
    rel <- relative_path(s$file, root)
    key <- sprintf("sink:%s:%s:%d", s$path, s$file, s$line)
    readers <- if (!is.null(sources[[key]])) sources[[key]]$files else character()
    list(path = s$path, table = isTRUE(s$table), loc = sprintf("%s:%d", rel, s$line), fn = s$fn, columns = s$columns,
         open = isTRUE(s$open), read_by = readers)
  })
  # a sink reached twice (a body walked on its own, with a parameter's open
  # frame, and followed from a caller with a known one) is one sink: the
  # known columns are kept
  if (length(sinks)) {
    sinks <- sinks[order(vapply(sinks, function(w) is.null(w$columns), TRUE), vapply(sinks, function(w) isTRUE(w$open), TRUE))]
    sinks <- sinks[!duplicated(vapply(sinks, function(w) paste(w$path, w$loc), ""))]
    sinks <- sinks[order(vapply(sinks, `[[`, "", "loc"), method = "radix")]
  }
  list(root = root, sources = sources, asserted = unname(asserted), n_files = length(files), stats = attr(findings, "stats"),
       steps = attr(findings, "steps"), findings = unclass(findings), sinks = sinks,
       notes = Filter(function(x) identical(x$kind, "note"), findings))
}

# --- the column set at a line -------------------------------------------------
# `r-colmap --at FILE:LINE`: every pipeline step whose statement covers the
# line, with the columns before and after it, what it derives, and where
# its new columns are read later. What Claude needs before adding a step.
colmap_at <- function(map, rel, line) {
  steps <- Filter(function(s) identical(relative_path(s$file, map$root), rel) &&
                    s$line <= line && (if (is.null(s$line_end) || is.na(s$line_end)) s$line else s$line_end) >= line, map$steps)
  out <- colmap_header(map, sprintf("at %s:%d", rel, line))
  if (!length(steps)) return(c(out, "", sprintf("No pipeline step with known columns covers %s:%d (a step the checker could not follow, or a line outside every pipeline; `r-colcheck --steps %s` lists the steps it followed).", rel, line, rel)))
  set_text <- function(cols, open, listed, groups) if (is.null(cols)) "(unknown)" else paste0(
    if (isTRUE(listed)) "each: " else "", if (isTRUE(open)) "at least " else "", paste(cols, collapse = ", "),
    if (length(groups)) sprintf("   [grouped by %s]", paste(groups, collapse = ", ")) else "")
  for (s in steps) {
    key <- step_source_key(s, map$root)
    src <- if (!is.null(key)) map$sources[[key]] else NULL
    out <- c(out, "", sprintf("%s:%d  %s   frame from %s", rel, s$line, s$call, if (!is.null(src)) src$key else source_key(s$source %||% "an unknown source")))
    out <- c(out, sprintf("  in:  %s", set_text(s$before, s$open_before, FALSE, s$groups_before)))
    made <- step_created(s)
    gone <- step_dropped(s)
    tail_ <- c(if (length(made)) sprintf("adds %s", paste(vapply(made, function(cn) {
      from <- s$derived[[cn]]
      if (length(from)) sprintf("%s (from %s)", cn, paste(from, collapse = ", ")) else cn
    }, ""), collapse = ", ")),
    if (length(s$overwritten)) sprintf("overwrites %s", paste(s$overwritten, collapse = ", ")),
    if (length(gone)) sprintf("drops %s", paste(gone, collapse = ", ")))
    out <- c(out, sprintf("  out: %s%s", set_text(s$columns, s$open, s$listed, s$groups), if (length(tail_)) paste0("   ", paste(tail_, collapse = "; ")) else ""))
    if (!is.null(src) && length(made)) {
      later <- character()
      for (cn in made) {
        sites <- Filter(function(x) x$kind == "used", src$sites[[cn]] %||% list())
        sites <- Filter(function(x) { f <- sub(":[0-9]+$", "", x$loc); l <- as.integer(sub("^.*:", "", x$loc)); f != rel || l > s$line }, sites)
        if (length(sites)) later <- c(later, sprintf("%s at %s", cn, paste(vapply(head(sites, 4L), site_short, ""), collapse = ", ")))
      }
      if (length(later)) out <- c(out, sprintf("  read later: %s", paste(later, collapse = "; ")))
    }
  }
  out
}

# --- what a change touches ------------------------------------------------
# The data side of `r-callmap --diff`: the steps the working tree's diff
# against REF falls in (git_changes and in_ranges come from callmap-lib.R),
# the columns those steps use, create or drop, with the other places each
# of those columns is used (the same column of the same source, which a
# rename or a removal reaches), the columns the changed files created at
# REF and no longer create (their old text is parsed from git, so a column
# the diff stopped making while other code still reads it is named), the
# checker's findings on the changed lines, and the sources in the changed
# files that it cannot open. One listing for a review.

step_in_diff <- function(s, rel, changes) {
  ch <- changes[[rel]]
  if (is.null(ch) || isTRUE(ch$deleted)) return(FALSE)
  end <- s$line_end %||% NA_integer_
  if (is.null(end) || is.na(end) || end < s$line) end <- s$line
  in_ranges(s$line, end, ch$ranges)
}

# The steps of one changed file as it was at REF, read through `git show`
# into a file with the same extension; their file is relabelled to `rel`.
old_steps_of <- function(root, prefix, base, old_path) {
  text <- suppressWarnings(tryCatch(system2("git", c("-C", shQuote(root), "show", shQuote(paste0(base, ":", prefix, old_path))),
                                            stdout = TRUE, stderr = FALSE), error = function(e) NULL))
  if (is.null(text) || !is.null(attr(text, "status"))) return(list())
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(old_path)))
  on.exit(unlink(tmp))
  writeLines(text, tmp)
  res <- quiet(colcheck_file(tmp, root))
  if (is.null(res)) return(list())
  bad <- mistakes_of(res)
  # a literal's id names the file it is in: the real path, not the copy's
  real <- normalizePath(file.path(root, old_path), mustWork = FALSE)
  copy <- normalizePath(tmp, mustWork = FALSE)
  lapply(attr(res, "steps"), function(s) {
    s$rel <- old_path
    s$bad <- bad
    if (!is.null(s$source_id) && !is.na(s$source_id)) s$source_id <- sub(copy, real, s$source_id, fixed = TRUE)
    if (!is.null(s$source_file) && !is.na(s$source_file)) s$source_file <- sub(copy, real, s$source_file, fixed = TRUE)
    s
  })
}

# "file statement name" of every column name the checker reported as a
# mistake: within that statement the name is not a column the code uses or
# creates (a misspelt grouping column travels into summarise()'s output).
mistakes_of <- function(findings) {
  bad <- character()
  for (x in findings) if (identical(x$kind %||% "column", "column") && !is.null(x$statement)) {
    bad <- c(bad, paste(x$file, x$statement, x$name))
  }
  bad
}

colmap_diff <- function(map, ref = NULL) {
  root <- map$root
  base <- ref %||% "HEAD"
  changes <- git_changes(root, ref)
  if (is.null(changes)) return(structure(list(ok = FALSE, base = base, message = sprintf("%s is not inside a git repository, so there is no diff to read.", root))))
  if (!is.null(attr(changes, "error"))) return(list(ok = FALSE, base = base, message = attr(changes, "error")))
  if (!length(changes)) return(list(ok = FALSE, base = base, message = sprintf("No R file differs from %s (tracked or untracked).", base)))
  prefix <- attr(changes, "prefix")
  bad <- mistakes_of(map$findings)
  rel_of <- function(f) relative_path(f, root)
  in_diff <- function(rel, line) {
    ch <- changes[[rel]]
    !is.null(ch) && !isTRUE(ch$deleted) && in_ranges(line, line, ch$ranges)
  }
  # the columns the changed steps touch, per source
  touched <- list()
  files <- character()
  n_steps <- 0L
  for (s in map$steps) {
    rel <- rel_of(s$file)
    if (!step_in_diff(s, rel, changes)) next
    key <- step_source_key(s, root)
    if (is.null(key)) next
    n_steps <- n_steps + 1L
    files <- union(files, rel)
    loc <- sprintf("%s:%d", rel, s$line)
    verb <- sub("\\(.*$", "", s$call)
    src <- touched[[key]] %||% list(key = key, columns = list())
    note <- function(col, kind, from = character()) {
      if (paste(s$file, s$line, col) %in% bad) return(invisible())
      c0 <- src$columns[[col]] %||% list(column = col, kinds = character(), sites = list(), from = character())
      c0$kinds <- union(c0$kinds, kind)
      c0$sites <- c(c0$sites, list(list(loc = loc, verb = verb, kind = kind, call = s$call)))
      c0$from <- union(c0$from, as.character(from))
      src$columns[[col]] <<- c0
    }
    for (u in s$used) note(u, "used")
    for (o in s$overwritten %||% character()) note(o, "overwritten", s$derived[[o]])
    for (cn in step_created(s)) note(cn, "created", s$derived[[cn]])
    for (cn in step_dropped(s)) note(cn, "dropped")
    touched[[key]] <- src
  }
  # the other sites of each touched column: outside the diff's lines
  for (key in names(touched)) {
    entry <- map$sources[[key]]
    touched[[key]]$label <- if (!is.null(entry)) entry$key else key
    for (col in names(touched[[key]]$columns)) {
      sites <- entry$sites[[col]] %||% list()
      others <- Filter(function(x) !in_diff(sub(":[0-9]+$", "", x$loc), as.integer(sub("^.*:", "", x$loc))), sites)
      touched[[key]]$columns[[col]]$other_sites <- others
      touched[[key]]$columns[[col]]$in_data <- col %in% (entry$origin %||% character())
      touched[[key]]$columns[[col]]$feeds <- if (is.null(entry)) character() else as.character(fed_by(entry, col)$direct)
    }
  }
  # columns a changed file created at REF and no longer creates (the same
  # name made by another file is said, not taken as a replacement)
  gone <- list()
  for (ch in changes) {
    if (isTRUE(ch$new) || is.na(ch$old)) next
    old <- old_steps_of(root, prefix, base, ch$old)
    for (s in old) {
      key <- step_source_key(s, root, rel = s$rel)
      # a column assertion is a pseudo-source keyed on its line: not compared
      if (is.null(key) || startsWith(key, "assertion:")) next
      for (cn in step_created(s)) {
        if (paste(s$file, s$line, cn) %in% (s$bad %||% character())) next
        now <- map$sources[[key]]$sites[[cn]] %||% list()
        made <- Filter(function(x) identical(x$kind, "created"), now)
        same_file <- vapply(made, function(x) identical(sub(":[0-9]+$", "", x$loc), ch$path), TRUE)
        if (any(same_file)) next
        if (!is.null(gone[[paste(key, cn)]])) next
        gone[[paste(key, cn)]] <- list(source = if (!is.null(map$sources[[key]])) map$sources[[key]]$key else step_source_label(s, root, rel = s$rel),
                                        column = cn, was = sprintf("%s:%d (%s)", s$rel, s$line, sub("\\(.*$", "", s$call)),
                                        still_used = Filter(function(x) identical(x$kind, "used"), now), also_made = made)
      }
    }
  }
  # the checker's findings on the changed lines, and the sources it cannot open there
  findings <- Filter(function(x) !identical(x$kind, "note") && in_diff(rel_of(x$file), x$line), map$findings)
  # the unresolved sources (a note with a key to learn), not the stop notes
  notes <- Filter(function(x) identical(x$kind, "note") && !is.null(x$key) && rel_of(x$file) %in% names(changes), map$findings)
  list(ok = TRUE, base = base, touched = touched, gone = unname(gone), findings = findings, notes = notes,
       n_steps = n_steps, files = files)
}

colmap_diff_format <- function(map, d) {
  out <- colmap_header(map, sprintf("changes in the working tree against %s", d$base))
  if (!isTRUE(d$ok)) return(c(out, "", d$message))
  n_cols <- sum(vapply(d$touched, function(s) length(s$columns), 1L))
  out <- c(out, "", sprintf("The diff falls in %d pipeline step%s in %s, touching %d column%s of %d source%s.",
                            d$n_steps, if (d$n_steps == 1L) "" else "s", paste(d$files, collapse = ", "),
                            n_cols, if (n_cols == 1L) "" else "s", length(d$touched), if (length(d$touched) == 1L) "" else "s"))
  if (!length(d$touched) && !length(d$gone)) out <- c(out, "No pipeline step with known columns is on the changed lines.")
  sites_text <- function(sites, n = 6L) paste0(paste(vapply(head(sites, n), site_short, ""), collapse = ", "),
                                               if (length(sites) > n) sprintf(", and %d more", length(sites) - n) else "")
  for (src in d$touched) {
    out <- c(out, "", src$label %||% src$key)
    for (col in src$columns) {
      parts <- character()
      for (k in c("created", "used", "overwritten", "dropped")) {
        s <- Filter(function(x) x$kind == k, col$sites)
        if (length(s)) parts <- c(parts, sprintf("%s at %s%s", k, sites_text(s, 3L),
                                                 if (k %in% c("created", "overwritten") && length(col$from)) sprintf(", from %s", paste(col$from, collapse = ", ")) else ""))
      }
      line <- sprintf("  %-20s %s", col$column, paste(parts, collapse = "; "))
      if (!col$in_data && !("created" %in% col$kinds)) line <- paste0(line, "   (made by the code elsewhere)")
      used_elsewhere <- Filter(function(x) x$kind == "used", col$other_sites)
      line <- paste0(line, if (length(used_elsewhere)) sprintf("   also used at %s", sites_text(used_elsewhere)) else "   not used anywhere else")
      if (length(col$feeds)) line <- paste0(line, sprintf("   feeds (by name): %s%s", paste(head(col$feeds, 8L), collapse = ", "),
                                                        if (length(col$feeds) > 8L) sprintf(", and %d more", length(col$feeds) - 8L) else ""))
      out <- c(out, line)
    }
  }
  if (length(d$gone)) {
    out <- c(out, "", sprintf("Columns the changed files created at %s and no longer create:", d$base))
    for (g in d$gone) {
      out <- c(out, sprintf("  %-20s was made at %s of %s   %s%s", g$column, g$was, g$source,
                            if (length(g$still_used)) paste0("still used at ", sites_text(g$still_used)) else "not used anywhere now",
                            if (length(g$also_made)) sprintf("   also made at %s", sites_text(g$also_made, 3L)) else ""))
    }
  }
  if (length(d$findings)) out <- c(out, "", "Findings on the changed lines:", paste0("  ", colcheck_format(d$findings, map$root)))
  if (length(d$notes)) {
    keys <- unique(vapply(d$notes, function(x) sprintf("\"%s\" (%s:%d)", x$key %||% x$name, relative_path(x$file, map$root), x$line), ""))
    out <- c(out, "", sprintf("Sources the checker cannot open, used in the changed files: %s; `r-colcheck discover` lists the learn commands.", paste(keys, collapse = ", ")))
  }
  out
}

colmap_diff_json <- function(map, d) {
  site_json <- function(x) list(file = sub(":[0-9]+$", "", x$loc), line = as.integer(sub("^.*:", "", x$loc)), verb = x$verb, kind = x$kind, call = x$call)
  if (!isTRUE(d$ok)) return(list(base = d$base, error = d$message, sources = list(), no_longer_created = list(), findings = list()))
  list(base = d$base, files = I(d$files), steps = d$n_steps,
       sources = lapply(unname(d$touched), function(src) list(
         source = src$label %||% src$key,
         columns = lapply(unname(src$columns), function(col) list(
           column = col$column, in_data = col$in_data, kinds = I(col$kinds), from = I(col$from), feeds = I(col$feeds),
           sites = lapply(col$sites, site_json), other_sites = lapply(col$other_sites, site_json))))),
       no_longer_created = lapply(d$gone, function(g) list(column = g$column, source = g$source, was = g$was,
                                                           still_used_at = lapply(g$still_used, site_json),
                                                           also_made_at = lapply(g$also_made, site_json))),
       findings = lapply(d$findings, function(x) list(file = relative_path(x$file, map$root), line = x$line, kind = x$kind %||% "column",
                                                      name = x$name, message = x$message)),
       unresolved = lapply(d$notes, function(x) list(file = relative_path(x$file, map$root), line = x$line, key = x$key %||% x$name)))
}

colmap_limits <- paste(
  "Static, with r-colcheck's tracking: a frame from a source the checker cannot open (no local file, no cached",
  "schema) is not here, and tracking ends at a step whose result it cannot work out; `r-colcheck discover` lists",
  "the sources to learn. \"no use seen\" means none in the steps followed: the frame may go on where the map cannot",
  "see, and the verdict says where it was last seen (returned by a function, written by a sink, tracking ended).")

data_sources <- function(map) Filter(function(e) e$kind == "data", map$sources)
parameter_sources <- function(map) Filter(function(e) e$kind == "parameter", map$sources)

colmap_header <- function(map, what = NULL) {
  st <- map$stats
  cached <- st$cached %||% 0L
  data <- data_sources(map)
  params <- parameter_sources(map)
  files <- unique(unlist(lapply(data, `[[`, "files")))
  c(sprintf("r-colmap: %d data source%s read by %d file%s of %s%s%s; %d of %d pipeline steps followed%s.",
            length(data), if (length(data) == 1L) "" else "s", length(files), if (length(files) == 1L) "" else "s", map$root,
            if (length(params)) sprintf(", %d function parameter%s used as frames", length(params), if (length(params) == 1L) "" else "s") else "",
            if (is.null(what)) "" else paste0(", ", what),
            st$checked + st$partial, st$checked + st$partial + st$skipped,
            if (cached > 0L) sprintf(" (%d of %d files unchanged since the last run, from the cache)", cached, st$files) else ""),
    colmap_limits)
}

site_short <- function(x, with_call = FALSE) {
  call <- if (nchar(x$call) > 60L) paste0(substr(x$call, 1L, 57L), "...") else x$call
  if (with_call) sprintf("%s (%s)", x$loc, call) else sprintf("%s (%s)", x$loc, x$verb)
}

fate_short <- function(f) switch(f$kind,
  returned = sprintf("returned by %s() at %s", f$fn, f$loc),
  written = sprintf("written by %s() at %s", f$fn, f$loc),
  lost = sprintf("tracking ended at %s (%s)", f$loc, f$fn))

# The fates of the frames that carried a column, most recent last.
column_fates <- function(src, cn) Filter(function(f) cn %in% f$columns, src$fates)

# "; used 2 times", or where the column was last seen when no step read it.
use_verdict <- function(src, cn, sites, unused = "no use seen in the steps followed") {
  read <- Filter(function(x) x$kind == "used", sites)
  if (length(read)) return(sprintf("; used %d %s", length(read), if (length(read) == 1L) "time" else "times"))
  # a column made on the y side and carried into the join's result: its uses are under that result
  carried <- Filter(function(x) x$kind == "carried", sites)
  if (length(carried)) return(sprintf("; carried into %s at %s", paste(unique(vapply(carried, `[[`, "", "into")), collapse = " and "),
                                      paste(head(unique(vapply(carried, site_short, "")), 3L), collapse = ", ")))
  fates <- column_fates(src, cn)
  paste0("; ", unused, "; ",
         if (length(fates)) paste(vapply(head(fates, 3L), fate_short, ""), collapse = ", ")
         else "no later step carries it")
}

# The lines for the columns a frame gained from the code: created (with what
# each derives from), or joined in from another source. A busy source's
# created section is folded to the names unless one source was asked for.
gained_lines <- function(src, indent = "  ", fold = TRUE) {
  out <- character()
  kinds <- lapply(src$sites, function(sites) vapply(sites, `[[`, "", "kind"))
  gained <- setdiff(names(src$sites), src$origin)
  created <- gained[vapply(gained, function(cn) "created" %in% kinds[[cn]], TRUE)]
  if (length(created) > 24L && fold) {
    out <- c(out, sprintf("%screated by the code: %d columns (%s); `r-colmap --source %s` lists each with its sites", indent, length(created),
                          paste(head(created, 12L), collapse = ", "), shQuote(src$key)))
  } else if (length(created)) {
    out <- c(out, paste0(indent, "created by the code:"))
    for (cn in created) {
      sites <- src$sites[[cn]]
      made <- Filter(function(x) x$kind == "created", sites)
      from <- derived_from(made)
      out <- c(out, sprintf("%s%-20s at %s%s%s", indent, cn, paste(vapply(head(made, 3L), site_short, ""), collapse = ", "),
                            if (length(from)) sprintf(", from %s", paste(from, collapse = ", ")) else "",
                            use_verdict(src, cn, sites)))
    }
  }
  joined <- gained[vapply(gained, function(cn) "joined" %in% kinds[[cn]] && !("created" %in% kinds[[cn]]), TRUE)]
  if (length(joined)) {
    by_join <- split(joined, vapply(joined, function(cn) {
      j <- Filter(function(x) x$kind == "joined", src$sites[[cn]])[[1L]]
      sprintf("joined from %s at %s", j$joined_from, site_short(j))
    }, ""))
    for (head_line in names(by_join)) {
      out <- c(out, paste0(indent, head_line, ":"))
      for (cn in by_join[[head_line]]) out <- c(out, sprintf("%s%-20s %s", indent, cn, sub("^; ", "", use_verdict(src, cn, src$sites[[cn]]))))
    }
  }
  out
}

# One source's block: a parameter (what the body reads of the frame it is
# given, then what it adds), or a data source (its columns with their uses,
# overwrites, drops and where they were carried, then what the code added).
source_lines <- function(src, fold = TRUE) {
  out <- character()
  if (src$kind == "parameter") {
    out <- c(out, "", sprintf("%s   at %s   reads %d column%s%s", src$key, src$first, length(src$origin),
                              if (length(src$origin) == 1L) "" else "s",
                              if (length(src$origin)) paste0(": ", paste(src$origin, collapse = ", ")) else ""))
    for (cn in src$origin) {
      used <- Filter(function(x) x$kind == "used", src$sites[[cn]])
      out <- c(out, sprintf("  %-20s used %d%s: %s%s", cn, length(used), if (length(used) == 1L) " time" else " times",
                            paste(vapply(head(used, 4L), site_short, ""), collapse = ", "), if (length(used) > 4L) ", ..." else ""))
    }
    return(c(out, gained_lines(src, fold = fold)))
  }
  out <- c(out, "", sprintf("%s   first used at %s   %d columns: %s", src$key, src$first, length(src$origin),
                            paste(src$origin, collapse = ", ")))
  for (cn in src$origin) {
    sites <- src$sites[[cn]]
    fates <- column_fates(src, cn)
    if (is.null(sites)) {
      out <- c(out, sprintf("  %-20s never named by the code%s", cn,
                            if (length(fates)) paste0("; ", paste(vapply(head(fates, 3L), fate_short, ""), collapse = ", ")) else ""))
      next
    }
    used <- Filter(function(x) x$kind == "used", sites)
    over <- Filter(function(x) x$kind == "overwritten", sites)
    dropped <- Filter(function(x) x$kind == "dropped", sites)
    carried <- Filter(function(x) x$kind == "carried", sites)
    parts <- c(if (length(used)) sprintf("used %d%s: %s%s", length(used), if (length(used) == 1L) " time" else " times",
                                         paste(vapply(head(used, 4L), site_short, ""), collapse = ", "),
                                         if (length(used) > 4L) ", ..." else ""),
               if (length(over)) sprintf("overwritten at %s", paste(vapply(head(over, 3L), site_short, ""), collapse = ", ")),
               if (length(dropped)) sprintf("dropped at %s", paste(vapply(head(dropped, 3L), site_short, ""), collapse = ", ")),
               if (length(carried)) sprintf("carried into %s at %s", paste(unique(vapply(carried, `[[`, "", "into")), collapse = " and "),
                                            paste(head(unique(vapply(carried, site_short, "")), 3L), collapse = ", ")))
    if (!length(parts)) parts <- sprintf("named at %s", paste(vapply(head(sites, 3L), site_short, ""), collapse = ", "))
    # a column no step reads: where its frame went, when the sites alone do not say
    if (!length(used) && !length(carried) && length(fates)) parts <- c(parts, paste(vapply(head(fates, 3L), fate_short, ""), collapse = ", "))
    out <- c(out, sprintf("  %-20s %s", cn, paste(parts, collapse = "; ")))
  }
  c(out, gained_lines(src, fold = fold))
}

# The input columns the created sites among `sites` derive from, in order.
derived_from <- function(sites) as.character(unique(unlist(lapply(Filter(function(x) identical(x$kind, "created"), sites), `[[`, "from"))))

# The columns of a source that are computed from `nm`: directly (a created
# site whose lineage names nm), then through those, each with the site that
# makes it. What a rename or removal of nm breaks beyond its own uses.
fed_by <- function(src, nm) {
  direct <- function(col) {
    out <- character()
    for (cn in names(src$sites)) {
      made <- Filter(function(x) identical(x$kind, "created") && col %in% x$from, src$sites[[cn]])
      if (length(made)) out <- c(out, structure(cn, loc = made[[1L]]$loc))
    }
    out
  }
  level1 <- direct(nm)
  seen <- c(nm, level1)
  queue <- level1
  later <- character()
  while (length(queue)) {
    nxt <- unlist(lapply(queue, direct))
    nxt <- nxt[!(nxt %in% seen)]
    seen <- c(seen, nxt)
    later <- c(later, nxt)
    queue <- nxt
  }
  list(direct = level1, through = later)
}

lineage_lines <- function(src, nm, indent = "    ") {
  out <- character()
  # what it derives from, per site that makes it (one column, several recipes)
  made <- Filter(function(x) identical(x$kind, "created") && length(x$from), src$sites[[nm]] %||% list())
  if (length(made)) {
    recipes <- vapply(made, function(x) sprintf("%s (%s)", paste(x$from, collapse = ", "), x$loc), "")
    out <- c(out, sprintf("%sderived from: %s", indent, paste(unique(recipes), collapse = "; ")))
  }
  fed <- fed_by(src, nm)
  site <- function(col) {
    made <- Filter(function(x) identical(x$kind, "created"), src$sites[[col]])
    if (length(made)) sprintf("%s (%s)", col, made[[1L]]$loc) else col
  }
  cap <- function(cols) paste0(paste(vapply(head(cols, 8L), site, ""), collapse = ", "), if (length(cols) > 8L) sprintf(", and %d more", length(cols) - 8L) else "")
  if (length(fed$direct)) {
    # by name within the source: a column of this name made anywhere on this source's frames
    out <- c(out, sprintf("%sfeeds (by name within this source): %s%s", indent, cap(fed$direct),
                          if (length(fed$through)) sprintf("; and through those: %s", cap(fed$through)) else ""))
  }
  out
}

colmap_format <- function(map, names = NULL, sources = NULL) {
  what <- c(if (length(names)) paste("around", paste(names, collapse = ", ")), if (length(sources)) paste("source", paste(sources, collapse = ", ")))
  out <- colmap_header(map, if (length(what)) paste(what, collapse = ", ") else NULL)
  if (!length(map$sources)) return(c(out, "", "No frame with known columns: nothing to map."))
  if (length(sources)) {
    # --source NAME: the sources whose key contains NAME (a file name, "cached schema", a target, a parameter)
    keep <- vapply(map$sources, function(s) any(vapply(sources, function(q) grepl(q, s$key, fixed = TRUE), TRUE)), TRUE)
    if (!any(keep)) return(c(out, "", sprintf("No source matches %s. Sources: %s", paste(sources, collapse = ", "),
                                              paste(vapply(map$sources, `[[`, "", "key"), collapse = "; "))))
    map$sources <- map$sources[keep]
    map$sinks <- list()
    map$notes <- list()
    map$asserted <- list()
  }
  if (!length(data_sources(map)) && !length(names)) out <- c(out, "", "No data source with known columns: nothing to map from the data.")
  if (length(names)) {
    for (nm in names) {
      if (!nzchar(nm)) next
      out <- c(out, "", paste0(nm, ":"))
      hits <- 0L
      for (src in map$sources) {
        sites <- src$sites[[nm]]
        in_origin <- nm %in% src$origin
        if (is.null(sites) && !in_origin) next
        hits <- hits + 1L
        fates <- column_fates(src, nm)
        if (is.null(sites)) {
          out <- c(out, sprintf("  %s   a column of the data, never named by the code%s", src$key,
                                if (length(fates)) paste0("; ", paste(vapply(head(fates, 3L), fate_short, ""), collapse = ", ")) else ""))
          next
        }
        by_kind <- split(sites, vapply(sites, `[[`, "", "kind"))
        label <- if (src$kind == "parameter" || in_origin) "" else if (!is.null(by_kind$joined) && is.null(by_kind$created))
          sprintf("   (not in the data: joined from %s)", by_kind$joined[[1L]]$joined_from) else "   (not in the data: created by the code)"
        out <- c(out, sprintf("  %s%s", src$key, label))
        for (k in intersect(c("created", "joined", "used", "overwritten", "dropped", "carried"), names(by_kind))) {
          shown <- head(by_kind[[k]], 12L)
          shown_k <- if (k == "carried") "carried into another frame" else k
          out <- c(out, sprintf("    %s at %s%s", shown_k, paste(vapply(shown, site_short, "", with_call = TRUE), collapse = ", "),
                                if (length(by_kind[[k]]) > 12L) sprintf(", and %d more", length(by_kind[[k]]) - 12L) else ""))
        }
        if (length(fates)) out <- c(out, sprintf("    then %s", paste(vapply(head(fates, 4L), fate_short, ""), collapse = ", ")))
        out <- c(out, lineage_lines(src, nm))
      }
      if (!hits) {
        near <- unique(unlist(lapply(map$sources, function(s) c(s$origin, names(s$sites)))))
        near <- near[quiet(agrepl(nm, near, max.distance = 0.3, fixed = TRUE), logical(length(near)))]
        out <- c(out, sprintf("  not a column of any source the map knows.%s",
                              if (length(near)) paste0(" Nearest: ", paste(head(near, 6L), collapse = ", ")) else ""))
      }
    }
    return(out)
  }
  # a bug in one source's listing prints a line for that source and goes on: one file cannot stop the map
  safe_lines <- function(src) tryCatch(source_lines(src, fold = !length(sources)), error = function(e) c("", sprintf(
    "%s   could not be listed (%s). This is a bug in r-colmap worth reporting; the other sources are unaffected.", src$key, conditionMessage(e))))
  for (src in data_sources(map)) out <- c(out, safe_lines(src))
  params <- parameter_sources(map)
  if (length(params)) {
    out <- c(out, "", "Function parameters used as frames (what each body reads of the frame it is given, and what it adds; the callers decide the rest):")
    for (src in params) out <- c(out, safe_lines(src))
  }
  if (length(map$asserted)) {
    out <- c(out, "", "Asserted by the code (a frame the checker could not open, given its columns by an assertion):")
    for (a in map$asserted) out <- c(out, sprintf("  %s   %s", a$first, paste(a$origin, collapse = ", ")))
  }
  if (length(map$sinks)) {
    out <- c(out, "", "Written by the code:")
    for (w in map$sinks) {
      out <- c(out, sprintf("  %s   written at %s (%s)%s%s", if (w$table) paste("table", w$path) else w$path, w$loc, w$fn,
                            if (is.null(w$columns)) "   columns unknown" else sprintf("   %s%d column%s: %s", if (w$open) "at least " else "", length(w$columns), if (length(w$columns) == 1L) "" else "s", paste(w$columns, collapse = ", ")),
                            if (length(w$read_by)) sprintf("   read by %s", paste(w$read_by, collapse = ", ")) else ""))
    }
  }
  if (length(map$notes)) {
    out <- c(out, "", sprintf("Not mapped: %d frame%s from sources the checker cannot open (r-colcheck discover lists them with the command to learn each).",
                              length(map$notes), if (length(map$notes) == 1L) "" else "s"))
  }
  out
}

# The files and tables the code writes, as entries of the same list as the
# sources (kind "written", `files` the readers), so a consumer walks one list.
colmap_sinks_json <- function(map) {
  lapply(map$sinks, function(w) list(source = if (w$table) paste("table", w$path) else w$path, kind = "written", first = w$loc,
                                     files = I(w$read_by), path = w$path, table = w$table, file = sub(":[0-9]+$", "", w$loc),
                                     line = as.integer(sub("^.*:", "", w$loc)), writer = w$fn,
                                     columns = if (is.null(w$columns)) NULL else I(w$columns), open = w$open, read_by = I(w$read_by)))
}

colmap_json <- function(map, names = NULL) {
  site_json <- function(x) {
    out <- list(file = sub(":[0-9]+$", "", x$loc), line = as.integer(sub("^.*:", "", x$loc)), verb = x$verb, kind = x$kind, call = x$call,
                from = I(x$from %||% character()))
    if (!is.null(x$joined_from)) out$joined_from <- x$joined_from
    if (!is.null(x$into)) out$into <- x$into
    out
  }
  fate_json <- function(f) list(kind = f$kind, fn = f$fn, file = sub(":[0-9]+$", "", f$loc), line = as.integer(sub("^.*:", "", f$loc)), call = f$call)
  lapply(unname(map$sources), function(src) {
    cols <- if (length(names)) intersect(names, c(src$origin, names(src$sites))) else union(src$origin, names(src$sites))
    if (length(names) && !length(cols)) return(NULL)
    list(source = src$key, kind = src$kind, first = src$first, files = I(src$files), columns = I(src$origin),
         unused = I(setdiff(src$origin, names(src$sites))),
         uses = lapply(cols, function(cn) list(column = cn, in_data = cn %in% src$origin,
                                                derived_from = I(derived_from(src$sites[[cn]] %||% list())),
                                                feeds = I(as.character(fed_by(src, cn)$direct)),
                                                sites = lapply(src$sites[[cn]] %||% list(), site_json),
                                                fates = lapply(column_fates(src, cn), fate_json))))
  })
}
