# colmap: the columns of every data source the project reads and where the
# code uses, creates or drops each one; the data-side counterpart of
# r-callmap. Built from r-colcheck's step records, so it has the checker's
# tracking and its limits: a pipeline the checker cannot follow (a frame
# from a source it cannot open and has no schema for, a step that makes the
# column set unknowable) is not here, and the summary says how many steps
# that was.
#
# Entry points: colmap_build(root, files) -> map; colmap_format(map, names)
#               -> character lines; colmap_json(map, names) -> list.

# "cached schema \"k\", observed 2025-01-15" and "cached schema \"k\"" are one source.
source_key <- function(src) sub(", observed [0-9-]+$", "", src)

# The source a step's frame comes from, as the map keys it: one literal is
# one source (tibble() at R/a.R:3 and tibble() at R/b.R:9 differ), so a
# literal's key carries where it is. `rel` is the step's file relative to
# the root (passed when the step was read from a file under another name).
step_source_key <- function(s, root, rel = relative_path(s$file, root)) {
  src <- s$source
  if (is.null(src) || is.na(src) || !nzchar(src)) return(NULL)
  id <- s$source_id
  # the structured id when the step has one (a path, a cache key, a literal's
  # text); the label alone for steps recorded without it
  if (!is.null(id) && !is.na(id) && nzchar(id)) return(id)
  key <- source_key(src)
  if (!is.null(s$source_line) && !is.na(s$source_line) && grepl(" literal$", key)) {
    key <- sprintf("%s at %s:%d", key, rel, s$source_line)
  }
  key
}

# The label a source is listed under: the step's own label; a literal's
# carries where it is, a file's its name, with the directory when two files
# share one name.
step_source_label <- function(s, root, rel = relative_path(s$file, root)) {
  key <- source_key(s$source)
  if (!is.null(s$source_line) && !is.na(s$source_line) && grepl(" literal$", key)) key <- sprintf("%s at %s:%d", key, rel, s$source_line)
  key
}

# The columns a step adds to its frame (not a grouping column summarise()
# keeps, which the input had) and the ones it drops; nothing on a partly
# known or listed input, where neither can be told.
step_created <- function(s) {
  if (isTRUE(s$terminal) || is.null(s$columns) || is.null(s$before) || isTRUE(s$listed) || isTRUE(s$open_before)) return(character())
  setdiff(setdiff(s$columns, s$before), s$groups_before %||% character())
}
step_dropped <- function(s) {
  if (isTRUE(s$terminal) || is.null(s$columns) || is.null(s$before) || isTRUE(s$listed) || isTRUE(s$open_before)) return(character())
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
  for (s in attr(findings, "steps")) {
    key <- step_source_key(s, root)
    if (is.null(key)) next
    loc <- sprintf("%s:%d", relative_path(s$file, root), s$line)
    entry <- sources[[key]] %||% list(key = step_source_label(s, root), id = key, origin = NULL, first = loc, columns = character(),
                                      sites = list(), n_steps = 0L, files = character())
    # the loader's columns: the input of the first step that has one
    if (is.null(entry$origin) && !is.null(s$before)) entry$origin <- s$before
    entry$n_steps <- entry$n_steps + 1L
    entry$files <- union(entry$files, relative_path(s$file, root))
    entry$columns <- union(entry$columns, c(s$before, s$columns))
    verb <- sub("\\(.*$", "", s$call)
    # `from`: the input columns a created column derives from (the step's
    # lineage record), so the map can say what depends on what
    add <- function(col, kind, from = NULL) {
      if (paste(s$file, s$line, col) %in% bad) return(invisible())
      entry$sites[[col]] <<- c(entry$sites[[col]], list(list(loc = loc, verb = verb, kind = kind, call = s$call,
                                                             from = as.character(from %||% character()))))
    }
    for (u in s$used) add(u, "used")
    for (o in s$overwritten %||% character()) add(o, "overwritten", s$derived[[o]])
    # on a partly known input ("at least these") what a step adds or drops
    # is not known either; only its uses are (step_created, step_dropped)
    for (cn in step_created(s)) add(cn, "created", s$derived[[cn]])
    for (cn in step_dropped(s)) add(cn, "dropped")
    sources[[key]] <- entry
  }
  for (k in names(sources)) if (is.null(sources[[k]]$origin)) sources[[k]]$origin <- sources[[k]]$columns
  # a column assertion on an unknown frame is not data: listed apart, as what the code asserts
  is_assertion <- startsWith(names(sources) %||% character(), "assertion:")
  asserted <- sources[is_assertion]
  sources <- sources[!is_assertion]
  labels <- vapply(sources, `[[`, "", "key")
  for (k in names(sources)) if (sum(labels == sources[[k]]$key) > 1L && startsWith(k, "file:")) {
    sources[[k]]$key <- relative_path(sub("^file:", "", k), root)
  }
  sources <- sources[order(vapply(sources, `[[`, "", "first"), method = "radix")]
  # the files and tables the code writes, with the sources that read them
  # (a reader resolved from a writer has the key "<path> (written by file:line)")
  sinks <- lapply(attr(findings, "sinks") %||% list(), function(s) {
    rel <- relative_path(s$file, root)
    key <- sprintf("sink:%s:%s:%d", s$path, s$file, s$line)
    readers <- if (!is.null(sources[[key]])) sources[[key]]$files else character()
    list(path = s$path, table = isTRUE(s$table), loc = sprintf("%s:%d", rel, s$line), fn = s$fn, columns = s$columns,
         open = isTRUE(s$open), read_by = readers)
  })
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
  "Static, with r-colcheck's tracking: frames from sources it cannot open (no local file, no cached",
  "schema) and steps past the point where a column set becomes unknowable are not here;",
  "`r-colcheck discover` lists the sources to learn.")

colmap_header <- function(map, what = NULL) {
  st <- map$stats
  c(sprintf("r-colmap: %d data source%s read by %d file%s of %s%s; %d of %d pipeline steps followed.",
            length(map$sources), if (length(map$sources) == 1L) "" else "s",
            length(unique(unlist(lapply(map$sources, `[[`, "files")))),
            if (length(unique(unlist(lapply(map$sources, `[[`, "files")))) == 1L) "" else "s",
            map$root, if (is.null(what)) "" else paste0(", ", what),
            st$checked + st$partial, st$checked + st$partial + st$skipped),
    colmap_limits)
}

site_short <- function(x, with_call = FALSE) {
  call <- if (nchar(x$call) > 60L) paste0(substr(x$call, 1L, 57L), "...") else x$call
  if (with_call) sprintf("%s (%s)", x$loc, call) else sprintf("%s (%s)", x$loc, x$verb)
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
    # --source NAME: the sources whose key contains NAME (a file name, "cached schema", a target)
    keep <- vapply(map$sources, function(s) any(vapply(sources, function(q) grepl(q, s$key, fixed = TRUE), TRUE)), TRUE)
    if (!any(keep)) return(c(out, "", sprintf("No source matches %s. Sources: %s", paste(sources, collapse = ", "),
                                              paste(vapply(map$sources, `[[`, "", "key"), collapse = "; "))))
    map$sources <- map$sources[keep]
    map$sinks <- list()
    map$notes <- list()
  }
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
        if (is.null(sites)) { out <- c(out, sprintf("  %s   a column of the data, never used", src$key)); next }
        by_kind <- split(sites, vapply(sites, `[[`, "", "kind"))
        out <- c(out, sprintf("  %s%s", src$key, if (in_origin) "" else "   (not in the data: created by the code)"))
        for (k in intersect(c("created", "used", "overwritten", "dropped"), names(by_kind))) {
          shown <- head(by_kind[[k]], 12L)
          out <- c(out, sprintf("    %s at %s%s", k, paste(vapply(shown, site_short, "", with_call = TRUE), collapse = ", "),
                                if (length(by_kind[[k]]) > 12L) sprintf(", and %d more", length(by_kind[[k]]) - 12L) else ""))
        }
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
  for (src in map$sources) {
    out <- c(out, "", sprintf("%s   first used at %s   %d columns: %s", src$key, src$first, length(src$origin),
                              paste(src$origin, collapse = ", ")))
    kinds <- lapply(src$sites, function(sites) vapply(sites, `[[`, "", "kind"))
    for (cn in src$origin) {
      sites <- src$sites[[cn]]
      if (is.null(sites)) { out <- c(out, sprintf("  %-20s never used", cn)); next }
      if (!any(vapply(sites, function(x) x$kind %in% c("used", "overwritten"), TRUE))) {
        out <- c(out, sprintf("  %-20s never used; dropped at %s", cn, paste(vapply(head(sites, 3L), site_short, ""), collapse = ", ")))
        next
      }
      used <- Filter(function(x) x$kind == "used", sites)
      over <- Filter(function(x) x$kind == "overwritten", sites)
      dropped <- Filter(function(x) x$kind == "dropped", sites)
      parts <- c(if (length(used)) sprintf("used %d%s: %s%s", length(used), if (length(used) == 1L) " time" else " times",
                                           paste(vapply(head(used, 4L), site_short, ""), collapse = ", "),
                                           if (length(used) > 4L) ", ..." else ""),
                 if (length(over)) sprintf("overwritten at %s", paste(vapply(head(over, 3L), site_short, ""), collapse = ", ")),
                 if (length(dropped)) sprintf("dropped at %s", paste(vapply(head(dropped, 3L), site_short, ""), collapse = ", ")))
      out <- c(out, sprintf("  %-20s %s", cn, paste(parts, collapse = "; ")))
    }
    created <- setdiff(names(src$sites), src$origin)
    created <- created[vapply(created, function(cn) any(kinds[[cn]] == "created"), TRUE)]
    if (length(created) > 24L && !length(sources)) {
      # the whole-project view of a busy source: the names, not a line each
      out <- c(out, sprintf("  created by the code: %d columns (%s); `r-colmap --source %s` lists each with its sites", length(created),
                            paste(head(created, 12L), collapse = ", "), shQuote(src$key)))
    } else if (length(created)) {
      out <- c(out, "  created by the code:")
      for (cn in created) {
        sites <- src$sites[[cn]]
        made <- Filter(function(x) x$kind == "created", sites)
        read <- Filter(function(x) x$kind == "used", sites)
        from <- derived_from(made)
        out <- c(out, sprintf("  %-20s at %s%s%s", cn, paste(vapply(head(made, 3L), site_short, ""), collapse = ", "),
                              if (length(from)) sprintf(", from %s", paste(from, collapse = ", ")) else "",
                              if (length(read)) sprintf("; used %d %s", length(read), if (length(read) == 1L) "time" else "times") else "; never used afterwards"))
      }
    }
  }
  if (length(map$asserted) && !length(sources)) {
    out <- c(out, "", "Asserted by the code (a frame the checker could not open, given its columns by an assertion):")
    for (a in map$asserted) out <- c(out, sprintf("  %s   %s", a$first, paste(a$origin, collapse = ", ")))
  }
  if (length(map$sinks)) {
    out <- c(out, "", "Written by the code:")
    for (w in map$sinks) {
      out <- c(out, sprintf("  %s   written at %s (%s)%s%s", if (w$table) paste("table", w$path) else w$path, w$loc, w$fn,
                            if (is.null(w$columns)) "   columns unknown" else sprintf("   %s%d columns: %s", if (w$open) "at least " else "", length(w$columns), paste(w$columns, collapse = ", ")),
                            if (length(w$read_by)) sprintf("   read by %s", paste(w$read_by, collapse = ", ")) else ""))
    }
  }
  if (length(map$notes)) {
    out <- c(out, "", sprintf("Not mapped: %d frame%s from sources the checker cannot open (r-colcheck discover lists them with the command to learn each).",
                              length(map$notes), if (length(map$notes) == 1L) "" else "s"))
  }
  out
}

colmap_sinks_json <- function(map) {
  lapply(map$sinks, function(w) list(path = w$path, table = w$table, file = sub(":[0-9]+$", "", w$loc), line = as.integer(sub("^.*:", "", w$loc)),
                                     writer = w$fn, columns = if (is.null(w$columns)) NULL else I(w$columns), open = w$open, read_by = I(w$read_by)))
}

colmap_json <- function(map, names = NULL) {
  lapply(unname(map$sources), function(src) {
    cols <- if (length(names)) intersect(names, c(src$origin, names(src$sites))) else union(src$origin, names(src$sites))
    if (length(names) && !length(cols)) return(NULL)
    list(source = src$key, first = src$first, files = I(src$files), columns = I(src$origin),
         unused = I(setdiff(src$origin, names(src$sites))),
         uses = lapply(cols, function(cn) list(column = cn, in_data = cn %in% src$origin,
                                                derived_from = I(derived_from(src$sites[[cn]] %||% list())),
                                                feeds = I(as.character(fed_by(src, cn)$direct)),
                                                sites = lapply(src$sites[[cn]] %||% list(), function(x) list(
                                                  file = sub(":[0-9]+$", "", x$loc), line = as.integer(sub("^.*:", "", x$loc)),
                                                  verb = x$verb, kind = x$kind, call = x$call, from = I(x$from %||% character()))))))
  })
}
