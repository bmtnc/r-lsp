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
# reported like any other.
#
# Entry points: colmap_build(root, files) -> map; colmap_format(map, names)
#               -> character lines; colmap_json(map, names) -> list.

# "cached schema \"k\", observed 2025-01-15" and "cached schema \"k\"" are one source.
source_key <- function(src) sub(", observed [0-9-]+$", "", src)

is_parameter_source <- function(key) startsWith(key, "parameter ")

# verbs that add the columns they name even on a partly known input
creating_verbs <- c("mutate", "transmute", "summarise", "summarize", "reframe", "count", "add_count", "tally",
                    "add_tally", "add_column", "rownames_to_column", "rowid_to_column", "unite", "pivot_longer",
                    "mutate_at", "mutate_if", "mutate_all", "group_by", "nest")

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
  bad <- character()
  for (x in findings) if (identical(x$kind %||% "column", "column") && !is.null(x$statement)) {
    bad <- c(bad, paste(x$file, x$statement, x$name))
  }
  sources <- list()
  entry_for <- function(key, loc, file) {
    sources[[key]] %||% list(key = key, kind = if (is_parameter_source(key)) "parameter" else "data",
                             origin = NULL, first = loc, columns = character(), sites = list(), fates = list(),
                             n_steps = 0L, files = character())
  }
  literal_key <- function(key, file, line) {
    # one literal is one source: tibble() at R/a.R:3 and tibble() at R/b.R:9 differ
    if (!is.null(line) && !is.na(line) && grepl(" literal$", key)) sprintf("%s at %s:%d", key, relative_path(file, root), line) else key
  }
  for (s in attr(findings, "steps")) {
    src <- s$source
    if (is.null(src) || is.na(src) || !nzchar(src)) next
    key <- literal_key(source_key(src), s$file, s$source_line)
    loc <- sprintf("%s:%d", relative_path(s$file, root), s$line)
    entry <- entry_for(key, loc, s$file)
    # the loader's columns: the input of the first step that has one (a
    # parameter's are worked out at the end, from what the body reads)
    if (is.null(entry$origin) && !is.null(s$before) && entry$kind == "data") entry$origin <- s$before
    entry$n_steps <- entry$n_steps + 1L
    entry$files <- union(entry$files, relative_path(s$file, root))
    entry$columns <- union(entry$columns, c(s$before, s$columns))
    verb <- sub("\\(.*$", "", s$call)
    bare_verb <- sub("^.*::", "", verb)
    add <- function(col, kind, ...) {
      if (paste(s$file, s$line, col) %in% bad) return(invisible())
      entry$sites[[col]] <<- c(entry$sites[[col]], list(list(loc = loc, verb = verb, kind = kind, call = s$call, ...)))
    }
    if (!is.null(s$fate)) {
      # where the frame went: every column it carried shares the fate
      entry$fates <- c(entry$fates, list(list(loc = loc, kind = s$fate, fn = s$fate_fn, call = s$call, columns = s$before)))
      sources[[key]] <- entry
      next
    }
    for (u in s$used) add(u, "used")
    brought <- s$joined$brought %||% character()
    if (is.null(s$columns) && !is.null(s$before) && bare_verb != "ggplot" && !grepl("^\\. \\+ ", verb)) {
      # a verb the checker could not follow: tracking of this frame ends here
      # (a plot is not a frame; nothing was lost there)
      entry$fates <- c(entry$fates, list(list(loc = loc, kind = "lost", fn = verb, call = s$call, columns = s$before)))
    }
    # what a step adds or drops is known on a known input; on a partly known
    # one ("at least these") only what a creating verb names
    if (!is.null(s$columns) && !is.null(s$before) && !isTRUE(s$listed)) {
      new <- setdiff(setdiff(s$columns, s$before), c(s$groups_before %||% character(), brought))
      if (!isTRUE(s$open_before)) {
        # a grouping column of the input that summarise() keeps was not created
        # here (it may be a reported mistake that never entered the column set)
        for (cn in new) add(cn, "created")
        # a result with no columns is not a frame (deframe(), pull()): nothing was dropped
        if (length(s$columns)) for (cn in setdiff(s$before, s$columns)) add(cn, "dropped")
      } else if (bare_verb %in% creating_verbs) {
        # a name the step read was a column of the input, not one it made
        for (cn in setdiff(new, s$used)) add(cn, "created")
      }
    }
    if (length(brought)) {
      from <- if (!is.null(s$joined$source)) literal_key(source_key(s$joined$source), s$file, s$joined$source_line) else NULL
      for (cn in brought) add(cn, "joined", from = from %||% "another frame")
      if (!is.null(from)) {
        # the other frame's own entry: its keys were read here, its other
        # columns carried into this frame's pipeline
        other <- entry_for(from, loc, s$file)
        if (is.null(other$origin) && other$kind == "data") other$origin <- s$joined$columns
        other$n_steps <- other$n_steps + 1L
        other$files <- union(other$files, relative_path(s$file, root))
        other$columns <- union(other$columns, s$joined$columns)
        for (k in s$joined$keys) other$sites[[k]] <- c(other$sites[[k]], list(list(loc = loc, verb = verb, kind = "used", call = s$call)))
        for (cn in setdiff(s$joined$columns, s$joined$keys)) {
          other$sites[[cn]] <- c(other$sites[[cn]], list(list(loc = loc, verb = verb, kind = "joined", call = s$call, into = key)))
        }
        sources[[from]] <- other
        entry <- sources[[key]] %||% entry
      }
    }
    sources[[key]] <- entry
  }
  for (k in names(sources)) {
    e <- sources[[k]]
    # a site reached twice (a body walked on its own and followed from a caller) is one site
    e$sites <- lapply(e$sites, function(sites) sites[!duplicated(vapply(sites, function(x) paste(x$loc, x$kind, x$call, x$from, x$into), ""))])
    if (e$kind == "parameter") {
      # what the body reads of the frame it was given, less what it made itself
      kinds <- lapply(e$sites, function(sites) vapply(sites, `[[`, "", "kind"))
      made <- names(kinds)[vapply(kinds, function(x) "created" %in% x || "joined" %in% x, TRUE)]
      e$origin <- setdiff(names(kinds)[vapply(kinds, function(x) "used" %in% x, TRUE)], made)
    } else if (is.null(e$origin)) e$origin <- e$columns
    sources[[k]] <- e
  }
  # a parameter whose body reads and adds nothing the map can name says nothing
  sources <- Filter(function(e) e$kind == "data" || length(e$origin) || length(e$sites), sources)
  first_file <- vapply(sources, function(e) sub(":[0-9]+$", "", e$first), "")
  first_line <- vapply(sources, function(e) as.integer(sub("^.*:", "", e$first)), 1L)
  ord <- order(vapply(sources, function(e) e$kind == "parameter", TRUE), first_file, first_line, method = "radix")
  sources <- sources[ord]
  list(root = root, sources = sources, n_files = length(files), stats = attr(findings, "stats"),
       notes = Filter(function(x) identical(x$kind, "note"), findings))
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
  data <- data_sources(map)
  params <- parameter_sources(map)
  files <- unique(unlist(lapply(data, `[[`, "files")))
  c(sprintf("r-colmap: %d data source%s read by %d file%s of %s%s%s; %d of %d pipeline steps followed.",
            length(data), if (length(data) == 1L) "" else "s", length(files), if (length(files) == 1L) "" else "s", map$root,
            if (length(params)) sprintf(", %d function parameter%s used as frames", length(params), if (length(params) == 1L) "" else "s") else "",
            if (is.null(what)) "" else paste0(", ", what),
            st$checked + st$partial, st$checked + st$partial + st$skipped),
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
  fates <- column_fates(src, cn)
  paste0("; ", unused, "; ",
         if (length(fates)) paste(vapply(head(fates, 3L), fate_short, ""), collapse = ", ")
         else "no later step carries it")
}

# The lines for the columns a frame gained from the code: created, or joined
# in from another source.
gained_lines <- function(src, indent = "  ") {
  out <- character()
  kinds <- lapply(src$sites, function(sites) vapply(sites, `[[`, "", "kind"))
  gained <- setdiff(names(src$sites), src$origin)
  created <- gained[vapply(gained, function(cn) "created" %in% kinds[[cn]], TRUE)]
  if (length(created)) {
    out <- c(out, paste0(indent, "created by the code:"))
    for (cn in created) {
      sites <- src$sites[[cn]]
      made <- Filter(function(x) x$kind == "created", sites)
      out <- c(out, sprintf("%s%-20s at %s%s", indent, cn, paste(vapply(head(made, 3L), site_short, ""), collapse = ", "),
                            use_verdict(src, cn, sites)))
    }
  }
  joined <- gained[vapply(gained, function(cn) "joined" %in% kinds[[cn]] && !("created" %in% kinds[[cn]]), TRUE)]
  if (length(joined)) {
    by_join <- split(joined, vapply(joined, function(cn) {
      j <- Filter(function(x) x$kind == "joined", src$sites[[cn]])[[1L]]
      sprintf("joined from %s at %s", j$from, site_short(j))
    }, ""))
    for (head_line in names(by_join)) {
      out <- c(out, paste0(indent, head_line, ":"))
      for (cn in by_join[[head_line]]) out <- c(out, sprintf("%s%-20s %s", indent, cn, sub("^; ", "", use_verdict(src, cn, src$sites[[cn]]))))
    }
  }
  out
}

source_lines <- function(src) {
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
    return(c(out, gained_lines(src)))
  }
  out <- c(out, "", sprintf("%s   first used at %s   %d columns: %s", src$key, src$first, length(src$origin),
                            paste(src$origin, collapse = ", ")))
  for (cn in src$origin) {
    sites <- src$sites[[cn]]
    if (is.null(sites)) {
      fates <- column_fates(src, cn)
      out <- c(out, sprintf("  %-20s never named by the code%s", cn,
                            if (length(fates)) paste0("; ", paste(vapply(head(fates, 3L), fate_short, ""), collapse = ", ")) else ""))
      next
    }
    used <- Filter(function(x) x$kind == "used", sites)
    dropped <- Filter(function(x) x$kind == "dropped", sites)
    carried <- Filter(function(x) x$kind == "joined", sites)
    parts <- c(if (length(used)) sprintf("used %d%s: %s%s", length(used), if (length(used) == 1L) " time" else " times",
                                         paste(vapply(head(used, 4L), site_short, ""), collapse = ", "),
                                         if (length(used) > 4L) ", ..." else ""),
               if (length(dropped)) sprintf("dropped at %s", paste(vapply(head(dropped, 3L), site_short, ""), collapse = ", ")),
               if (length(carried)) sprintf("carried into %s at %s", paste(unique(vapply(carried, `[[`, "", "into")), collapse = " and "),
                                            paste(head(unique(vapply(carried, site_short, "")), 3L), collapse = ", ")))
    out <- c(out, sprintf("  %-20s %s", cn, paste(parts, collapse = "; ")))
  }
  c(out, gained_lines(src))
}

colmap_format <- function(map, names = NULL) {
  out <- colmap_header(map, if (length(names)) paste("around", paste(names, collapse = ", ")) else NULL)
  if (!length(map$sources)) return(c(out, "", "No frame with known columns: nothing to map."))
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
        label <- if (src$kind == "parameter") "" else if (in_origin) "" else if (!is.null(by_kind$joined) && is.null(by_kind$created))
          sprintf("   (not in the data: joined from %s)", by_kind$joined[[1L]]$from) else "   (not in the data: created by the code)"
        out <- c(out, sprintf("  %s%s", src$key, label))
        for (k in intersect(c("created", "joined", "used", "dropped"), names(by_kind))) {
          shown <- head(by_kind[[k]], 12L)
          shown_k <- if (k == "joined" && !is.null(shown[[1L]]$into)) "carried into another frame" else k
          out <- c(out, sprintf("    %s at %s%s", shown_k, paste(vapply(shown, site_short, "", with_call = TRUE), collapse = ", "),
                                if (length(by_kind[[k]]) > 12L) sprintf(", and %d more", length(by_kind[[k]]) - 12L) else ""))
        }
        if (length(fates)) out <- c(out, sprintf("    then %s", paste(vapply(head(fates, 4L), fate_short, ""), collapse = ", ")))
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
  for (src in data_sources(map)) out <- c(out, source_lines(src))
  params <- parameter_sources(map)
  if (length(params)) {
    out <- c(out, "", "Function parameters used as frames (what each body reads of the frame it is given, and what it adds; the callers decide the rest):")
    for (src in params) out <- c(out, source_lines(src))
  }
  if (length(map$notes)) {
    out <- c(out, "", sprintf("Not mapped: %d frame%s from sources the checker cannot open (r-colcheck discover lists them with the command to learn each).",
                              length(map$notes), if (length(map$notes) == 1L) "" else "s"))
  }
  out
}

colmap_json <- function(map, names = NULL) {
  site_json <- function(x) {
    out <- list(file = sub(":[0-9]+$", "", x$loc), line = as.integer(sub("^.*:", "", x$loc)), verb = x$verb, kind = x$kind, call = x$call)
    if (!is.null(x$from)) out$from <- x$from
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
                                                sites = lapply(src$sites[[cn]] %||% list(), site_json),
                                                fates = lapply(column_fates(src, cn), fate_json))))
  })
}
