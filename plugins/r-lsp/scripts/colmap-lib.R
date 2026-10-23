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
  for (s in attr(findings, "steps")) {
    src <- s$source
    if (is.null(src) || is.na(src) || !nzchar(src)) next
    key <- source_key(src)
    # one literal is one source: tibble() at R/a.R:3 and tibble() at R/b.R:9 differ
    if (!is.null(s$source_line) && !is.na(s$source_line) && grepl(" literal$", key)) {
      key <- sprintf("%s at %s:%d", key, relative_path(s$file, root), s$source_line)
    }
    loc <- sprintf("%s:%d", relative_path(s$file, root), s$line)
    entry <- sources[[key]] %||% list(key = key, origin = NULL, first = loc, columns = character(),
                                      sites = list(), n_steps = 0L, files = character())
    # the loader's columns: the input of the first step that has one
    if (is.null(entry$origin) && !is.null(s$before)) entry$origin <- s$before
    entry$n_steps <- entry$n_steps + 1L
    entry$files <- union(entry$files, relative_path(s$file, root))
    entry$columns <- union(entry$columns, c(s$before, s$columns))
    verb <- sub("\\(.*$", "", s$call)
    add <- function(col, kind) {
      if (paste(s$file, s$line, col) %in% bad) return(invisible())
      entry$sites[[col]] <<- c(entry$sites[[col]], list(list(loc = loc, verb = verb, kind = kind, call = s$call)))
    }
    for (u in s$used) add(u, "used")
    # on a partly known input ("at least these") what a step adds or drops
    # is not known either; only its uses are
    if (!is.null(s$columns) && !is.null(s$before) && !isTRUE(s$listed) && !isTRUE(s$open_before)) {
      # a grouping column of the input that summarise() keeps was not created
      # here (it may be a reported mistake that never entered the column set)
      for (cn in setdiff(setdiff(s$columns, s$before), s$groups_before %||% character())) add(cn, "created")
      for (cn in setdiff(s$before, s$columns)) add(cn, "dropped")
    }
    sources[[key]] <- entry
  }
  for (k in names(sources)) if (is.null(sources[[k]]$origin)) sources[[k]]$origin <- sources[[k]]$columns
  sources <- sources[order(vapply(sources, `[[`, "", "first"), method = "radix")]
  list(root = root, sources = sources, n_files = length(files), stats = attr(findings, "stats"),
       notes = Filter(function(x) identical(x$kind, "note"), findings))
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

colmap_format <- function(map, names = NULL) {
  out <- colmap_header(map, if (length(names)) paste("around", paste(names, collapse = ", ")) else NULL)
  if (!length(map$sources)) return(c(out, "", "No frame with known columns: nothing to map."))
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
        for (k in intersect(c("created", "used", "dropped"), names(by_kind))) {
          shown <- head(by_kind[[k]], 12L)
          out <- c(out, sprintf("    %s at %s%s", k, paste(vapply(shown, site_short, "", with_call = TRUE), collapse = ", "),
                                if (length(by_kind[[k]]) > 12L) sprintf(", and %d more", length(by_kind[[k]]) - 12L) else ""))
        }
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
      used <- Filter(function(x) x$kind == "used", sites)
      dropped <- Filter(function(x) x$kind == "dropped", sites)
      parts <- c(if (length(used)) sprintf("used %d%s: %s%s", length(used), if (length(used) == 1L) " time" else " times",
                                           paste(vapply(head(used, 4L), site_short, ""), collapse = ", "),
                                           if (length(used) > 4L) ", ..." else ""),
                 if (length(dropped)) sprintf("dropped at %s", paste(vapply(head(dropped, 3L), site_short, ""), collapse = ", ")))
      out <- c(out, sprintf("  %-20s %s", cn, paste(parts, collapse = "; ")))
    }
    created <- setdiff(names(src$sites), src$origin)
    created <- created[vapply(created, function(cn) any(kinds[[cn]] == "created"), TRUE)]
    if (length(created)) {
      out <- c(out, "  created by the code:")
      for (cn in created) {
        sites <- src$sites[[cn]]
        made <- Filter(function(x) x$kind == "created", sites)
        read <- Filter(function(x) x$kind == "used", sites)
        out <- c(out, sprintf("  %-20s at %s%s", cn, paste(vapply(head(made, 3L), site_short, ""), collapse = ", "),
                              if (length(read)) sprintf("; used %d %s", length(read), if (length(read) == 1L) "time" else "times") else "; never used afterwards"))
      }
    }
  }
  if (length(map$notes)) {
    out <- c(out, "", sprintf("Not mapped: %d frame%s from sources the checker cannot open (r-colcheck discover lists them with the command to learn each).",
                              length(map$notes), if (length(map$notes) == 1L) "" else "s"))
  }
  out
}

colmap_json <- function(map, names = NULL) {
  lapply(unname(map$sources), function(src) {
    cols <- if (length(names)) intersect(names, c(src$origin, names(src$sites))) else union(src$origin, names(src$sites))
    if (length(names) && !length(cols)) return(NULL)
    list(source = src$key, first = src$first, files = I(src$files), columns = I(src$origin),
         unused = I(setdiff(src$origin, names(src$sites))),
         uses = lapply(cols, function(cn) list(column = cn, in_data = cn %in% src$origin,
                                                sites = lapply(src$sites[[cn]] %||% list(), function(x) list(
                                                  file = sub(":[0-9]+$", "", x$loc), line = as.integer(sub("^.*:", "", x$loc)),
                                                  verb = x$verb, kind = x$kind, call = x$call)))))
  })
}
