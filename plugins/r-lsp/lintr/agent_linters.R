# Agent lint profile for the r-lsp plugin.
#
# lintr falls back to this profile when a project has no `.lintr` of its own
# (and there is no `~/.lintr`). The wrapper points lintr at it through
# R_USER_CONFIG_DIR. It reports likely bugs only, never style, and it
# understands tidyverse code:
#
#   - library(tidyverse) (and other meta-packages) attach their core packages,
#     so mutate(), ggplot(), %>% etc. are known;
#   - bare column names inside dplyr / tidyr / ggplot2 calls are not reported
#     as undefined variables, while undefined *functions* inside those calls
#     still are;
#   - calls to deprecated, defunct or superseded functions are reported,
#     judged against the installed (renv-pinned) package versions;
#   - undefined function calls are caught in top-level script code, which
#     lintr's object_usage_linter never looks at.
#
# Everything here fails quiet: on an internal error a linter returns lintr's
# own result, or no lints, never a broken diagnostics run. Package facts are
# cached for the life of the helper process that runs lintr.
#
# The value of this file is the list of linters.

local({
  # Packages whose functions evaluate arguments as data columns or captured
  # code. Bare symbols inside calls to their functions are not reported.
  nse_packages <- c(
    "dplyr", "tidyr", "ggplot2", "tidyselect", "dbplyr", "dtplyr", "duckplyr",
    "data.table", "rlang", "recipes", "tsibble", "fable", "feasts", "gt",
    "plotly", "arrow", "sparklyr", "tidyquant", "timetk", "janitor"
  )
  nse_base <- c(
    "with", "within", "subset", "transform", "quote", "bquote", "substitute",
    "expression", "evalq", "alist"
  )
  # Calls whose arguments are never evaluated as ordinary code.
  quoting_calls <- c(
    "quote", "bquote", "substitute", "expression", "alist", "expr", "exprs",
    "quo", "quos", "enquo", "enquos", "vars"
  )
  # Calls that put names in scope in ways static analysis cannot follow.
  # A file using any of them is skipped by undefined_function_linter.
  dynamic_scope_calls <- c("attach", "sys.source", "list2env", "sourceCpp")
  dynamic_scope_packages <- c("box", "import", "modules")

  # Per-process cache, kept across lint runs in the same worker session.
  cache <- getOption("r_lsp.lint_cache")
  if (!is.environment(cache)) {
    cache <- new.env(parent = emptyenv())
    options(r_lsp.lint_cache = cache)
  }
  memo <- function(key, compute) {
    if (!exists(key, envir = cache, inherits = FALSE)) {
      assign(key, compute(), envir = cache)
    }
    get(key, envir = cache, inherits = FALSE)
  }
  quiet <- function(expr, default = NULL) {
    tryCatch(suppressWarnings(expr), error = function(e) default)
  }
  `%||%` <- function(a, b) if (is.null(a)) b else a

  # --- the project's packages ------------------------------------------------
  # Package facts come from the copy installed for the *project* (its renv
  # library first), read from disk. The copy loaded in this process can be a
  # different version: the server's own dependencies (purrr, stringr, rlang,
  # cli, ...) are loaded from the global library, and R cannot load two
  # versions of a package at once.
  project_libs <- function() {
    libs <- .libPaths()
    server_lib <- Sys.getenv("R_LSP_SERVER_LIB")
    if (nzchar(server_lib)) {
      libs <- libs[normalizePath(libs, mustWork = FALSE) !=
        normalizePath(server_lib, mustWork = FALSE)]
    }
    libs
  }

  pkg_path <- function(pkg) {
    memo(paste0("path:", pkg), function() {
      path <- find.package(pkg, lib.loc = project_libs(), quiet = TRUE)
      if (length(path)) path[[1L]] else NA_character_
    })
  }

  is_installed <- function(pkg) !is.na(pkg_path(pkg))

  pkg_version <- function(pkg) {
    memo(paste0("version:", pkg), function() {
      path <- pkg_path(pkg)
      if (is.na(path)) return(NA_character_)
      quiet(read.dcf(file.path(path, "DESCRIPTION"), fields = "Version")[1L, 1L],
        NA_character_)
    })
  }

  # Read one object from the project's installed copy of `pkg`.
  project_object <- function(pkg, name) {
    path <- pkg_path(pkg)
    if (is.na(path)) return(NULL)
    if (isNamespaceLoaded(pkg) &&
        identical(as.character(getNamespaceVersion(pkg)), pkg_version(pkg))) {
      return(quiet(get0(name, envir = asNamespace(pkg), inherits = FALSE)))
    }
    env <- new.env(parent = emptyenv())
    quiet(lazyLoad(file.path(path, "R", pkg), envir = env,
      filter = function(n) n == name))
    quiet(get0(name, envir = env, inherits = FALSE))
  }

  pkg_exports <- function(pkg) {
    memo(paste0("exports:", pkg), function() {
      path <- pkg_path(pkg)
      if (is.na(path)) return(NULL)
      info <- quiet(readRDS(file.path(path, "Meta", "nsInfo.rds")))
      if (is.null(info)) return(NULL)
      exports <- unlist(info$exports, use.names = FALSE)
      if (length(info$exportPatterns)) {
        env <- new.env(parent = emptyenv())
        quiet(lazyLoad(file.path(path, "R", pkg), envir = env))
        objs <- ls(env, all.names = TRUE)
        for (pattern in info$exportPatterns) exports <- c(exports, grep(pattern, objs, value = TRUE))
      }
      lazydata <- new.env(parent = emptyenv())
      if (file.exists(file.path(path, "data", "Rdata.rdx"))) {
        quiet(lazyLoad(file.path(path, "data", "Rdata"), envir = lazydata))
      }
      unique(c(exports, ls(lazydata, all.names = TRUE)))
    })
  }

  # Exports of the copy of `pkg` loaded in this process, if that is a
  # different version from the project's copy.
  loaded_exports_if_different <- function(pkg) {
    if (!isNamespaceLoaded(pkg)) return(character())
    if (identical(as.character(getNamespaceVersion(pkg)), pkg_version(pkg))) return(character())
    getNamespaceExports(pkg)
  }

  # Packages attached along with `pkg`: its Depends, plus the `core` vector
  # that meta-packages built on the tidyverse template (tidyverse,
  # tidymodels, ...) attach from .onAttach.
  pkg_companions <- function(pkg) {
    memo(paste0("companions:", pkg), function() {
      path <- pkg_path(pkg)
      if (is.na(path)) return(character())
      deps <- quiet(read.dcf(file.path(path, "DESCRIPTION"), fields = "Depends")[1L, 1L], NA)
      deps <- if (is.na(deps)) character() else {
        trimws(sub("\\(.*", "", strsplit(deps, ",", fixed = TRUE)[[1]]))
      }
      core <- project_object(pkg, "core")
      if (!is.character(core)) core <- character()
      setdiff(unique(c(core, deps)), c("R", ""))
    })
  }

  node_text <- function(node, lines) {
    l1 <- as.integer(xml2::xml_attr(node, "line1"))
    c1 <- as.integer(xml2::xml_attr(node, "col1"))
    l2 <- as.integer(xml2::xml_attr(node, "line2"))
    c2 <- as.integer(xml2::xml_attr(node, "col2"))
    if (l1 == l2) return(substr(lines[[l1]], c1, c2))
    paste(c(substring(lines[[l1]], c1), if (l2 > l1 + 1L) lines[(l1 + 1L):(l2 - 1L)],
      substr(lines[[l2]], 1L, c2)), collapse = "\n")
  }

  # Packages the file attaches, in attach order, and whether any attach call
  # could not be resolved statically.
  attached_packages <- function(xml, lines) {
    calls <- xml2::xml_find_all(xml, paste0(
      "//SYMBOL_FUNCTION_CALL[text() = 'library' or text() = 'require'",
      " or text() = 'p_load']/parent::expr/parent::expr"
    ))
    pkgs <- character()
    unresolved <- FALSE
    for (node in calls) {
      call <- quiet(str2lang(node_text(node, lines)))
      if (!is.call(call)) {
        unresolved <- TRUE
        next
      }
      head <- all.names(call[[1L]])
      head <- head[length(head)]
      if (head == "p_load") {
        args <- as.list(call)[-1L]
        nms <- names(args) %||% rep("", length(args))
        found <- vapply(args[!nzchar(nms)], function(a) {
          if (is.symbol(a) || is.character(a)) as.character(a) else NA_character_
        }, character(1L))
        if (anyNA(found) || isTRUE(args[["character.only"]])) unresolved <- TRUE
        pkgs <- c(pkgs, found[!is.na(found)])
        next
      }
      fun <- if (head == "library") base::library else base::require
      matched <- quiet(match.call(fun, call))
      if (is.null(matched) || is.null(matched$package)) next
      if (!isFALSE(matched$character.only) && !is.null(matched$character.only)) {
        unresolved <- TRUE
        next
      }
      pkg <- matched$package
      if (is.symbol(pkg) || is.character(pkg)) {
        pkgs <- c(pkgs, as.character(pkg))
      } else {
        unresolved <- TRUE
      }
    }
    list(packages = unique(pkgs), unresolved = unresolved)
  }

  expand_packages <- function(pkgs) {
    out <- character()
    add <- function(p) {
      if (p %in% out) return(invisible())
      out <<- c(out, p)
      for (q in pkg_companions(p)) add(q)
    }
    for (p in pkgs) add(p)
    out
  }

  # Project root: nearest ancestor with a project marker, else the file's dir.
  project_root <- function(path) {
    dir <- normalizePath(dirname(path), mustWork = FALSE)
    start <- dir
    home <- normalizePath("~", mustWork = FALSE)
    markers <- c("DESCRIPTION", ".git", "renv.lock", "_targets.R", ".here")
    while (nzchar(dir) && dir != dirname(dir) && dir != home) {
      if (any(file.exists(file.path(dir, markers))) ||
          length(list.files(dir, pattern = "\\.Rproj$"))) {
        return(dir)
      }
      dir <- dirname(dir)
    }
    start
  }

  # Names assigned anywhere in the project's R code, so helpers defined in a
  # script that is not source()d here are not reported as undefined.
  # Returns NULL when the project is too large to scan.
  project_definitions <- function(root) {
    key <- paste0("defs:", root)
    hit <- get0(key, envir = cache, inherits = FALSE)
    if (!is.null(hit) && difftime(Sys.time(), hit$time, units = "secs") < 10) {
      return(hit$names)
    }
    files <- quiet(list.files(root, pattern = "\\.(R|r|Rmd|rmd|qmd|Rprofile)$",
      recursive = TRUE, full.names = TRUE, all.files = TRUE), character())
    files <- files[!grepl("/(renv|packrat|\\.git|node_modules|\\.Rproj\\.user)/", files)]
    defs <- if (length(files) > 3000L) NULL else {
      patterns <- c(
        "^\\s*`?([A-Za-z.][A-Za-z0-9._]*)`?\\s*(<<?-|=)",
        "assign\\(\\s*[\"']([A-Za-z.][A-Za-z0-9._]*)[\"']"
      )
      unique(unlist(lapply(files, function(f) {
        lines <- quiet(readLines(f, warn = FALSE), character())
        unlist(lapply(patterns, function(pattern) {
          m <- regmatches(lines, regexec(pattern, lines))
          vapply(m[lengths(m) > 0L], `[[`, character(1L), 2L)
        }))
      }), use.names = FALSE))
    }
    assign(key, list(time = Sys.time(), names = defs), envir = cache)
    defs
  }

  # Names an R package project makes visible to its own code and tests:
  # NAMESPACE imports, Depends, and testthat for files under tests/.
  # `unresolved` is TRUE when one of those packages cannot be read.
  package_scope <- function(root, filename) {
    ns_file <- file.path(root, "NAMESPACE")
    desc_file <- file.path(root, "DESCRIPTION")
    if (!file.exists(desc_file)) return(list(names = character(), unresolved = FALSE))
    in_tests <- startsWith(normalizePath(filename, mustWork = FALSE),
      file.path(normalizePath(root, mustWork = FALSE), "tests"))
    pkgs <- if (in_tests) "testthat" else character()
    names_out <- character()
    if (file.exists(ns_file)) {
      directives <- quiet(parse(ns_file, keep.source = FALSE), list())
      for (d in as.list(directives)) {
        if (!is.call(d)) next
        args <- vapply(as.list(d)[-1L], function(a) paste(as.character(a), collapse = ""),
          character(1L))
        head <- as.character(d[[1L]])
        if (head == "import") pkgs <- c(pkgs, args)
        if (head == "importFrom" && length(args) > 1L) names_out <- c(names_out, args[-1L])
      }
    }
    deps <- quiet(read.dcf(desc_file, fields = "Depends")[1L, 1L], NA)
    if (!is.na(deps)) {
      pkgs <- c(pkgs, trimws(sub("\\(.*", "", strsplit(deps, ",", fixed = TRUE)[[1]])))
    }
    exports <- lapply(setdiff(unique(pkgs), c("R", "")), pkg_exports)
    list(names = c(names_out, unlist(exports, use.names = FALSE)),
      unresolved = any(vapply(exports, is.null, logical(1L))))
  }

  # Everything the linters need to know about one file.
  file_context <- function(source_expression) {
    last <- get0("last_context", envir = cache, inherits = FALSE)
    if (!is.null(last) && identical(last$filename, source_expression$filename) &&
        identical(last$lines, source_expression$file_lines)) {
      return(last$ctx)
    }
    ctx <- build_context(source_expression)
    assign("last_context", list(filename = source_expression$filename,
      lines = source_expression$file_lines, ctx = ctx), envir = cache)
    ctx
  }

  build_context <- function(source_expression) {
    xml <- source_expression$full_xml_parsed_content
    lines <- source_expression$file_lines
    att <- attached_packages(xml, lines)
    expanded <- expand_packages(att$packages)
    exports <- lapply(expanded, pkg_exports)
    names(exports) <- expanded
    root <- project_root(source_expression$filename)
    scope <- package_scope(root, source_expression$filename)
    # An attached package we cannot read could define any name.
    unresolved <- att$unresolved || scope$unresolved ||
      any(vapply(exports, is.null, logical(1L)))
    list(
      xml = xml,
      lines = lines,
      attached = expanded,
      attached_rev = rev(expanded),
      exports = exports,
      known = unique(c(unlist(exports, use.names = FALSE), scope$names)),
      unresolved = unresolved,
      root = root
    )
  }

  # Package that a bare call to `fun` resolves to, by R's search order.
  resolve_package <- function(fun, ctx) {
    for (p in ctx$attached_rev) {
      if (fun %in% ctx$exports[[p]]) return(p)
    }
    NA_character_
  }

  # Name and package of each call node's function, e.g. dplyr::mutate(...).
  call_head <- function(call_expr) {
    fn <- xml2::xml_find_first(call_expr, "./expr[1]/SYMBOL_FUNCTION_CALL")
    if (inherits(fn, "xml_missing")) return(NULL)
    pkg <- xml2::xml_find_first(call_expr, "./expr[1]/SYMBOL_PACKAGE")
    list(fun = xml2::xml_text(fn),
      pkg = if (inherits(pkg, "xml_missing")) NA_character_ else xml2::xml_text(pkg))
  }

  in_nse_call <- function(node, ctx) {
    calls <- xml2::xml_find_all(node, "ancestor::expr[expr[1]/SYMBOL_FUNCTION_CALL]")
    for (call_expr in calls) {
      head <- call_head(call_expr)
      if (is.null(head)) next
      if (head$fun %in% nse_base) return(TRUE)
      pkg <- if (is.na(head$pkg)) resolve_package(head$fun, ctx) else head$pkg
      if (!is.na(pkg) && pkg %in% nse_packages) return(TRUE)
    }
    FALSE
  }

  flatten_lints <- function(x) {
    if (inherits(x, "lint")) return(list(x))
    if (!is.list(x)) return(list())
    unlist(lapply(x, flatten_lints), recursive = FALSE)
  }

  # A linter that errors aborts the whole lintr run, so each one here is
  # wrapped: on an internal error it reports `fallback()` instead.
  safe_linter <- function(fun, fallback = function(source_expression) list()) {
    lintr::Linter(linter_level = "file", function(source_expression) {
      tryCatch(fun(source_expression), error = function(e) fallback(source_expression))
    })
  }

  # Run a lintr linter, then keep only the lints `keep(lint, source_expression)`
  # accepts.
  filtered_linter <- function(inner, keep) {
    safe_linter(fallback = inner, function(source_expression) {
      lints <- flatten_lints(inner(source_expression))
      if (!length(lints)) return(list())
      ok <- vapply(lints, function(l) isTRUE(quiet(keep(l, source_expression), TRUE)), logical(1L))
      structure(lints[ok], class = "lints")
    })
  }

  token_at <- function(xml, lint, tokens) {
    node <- xml2::xml_find_first(xml, sprintf("//*[(%s) and @line1 = %d and @col1 = %d]",
      paste0("self::", tokens, collapse = " or "), lint$line_number, lint$column_number))
    if (inherits(node, "xml_missing")) NA_character_ else
      gsub("^[`'\"]|[`'\"]$", "", xml2::xml_text(node))
  }

  # Names interpolated into glue / cli strings: "{x}", "{.val {x}}".
  interpolated_names <- function(xml) {
    strings <- xml2::xml_text(xml2::xml_find_all(xml, "//STR_CONST[contains(text(), '{')]"))
    inner <- unlist(regmatches(strings, gregexpr("\\{[^{}]*\\}", strings)))
    unique(unlist(regmatches(inner, gregexpr("[A-Za-z.][A-Za-z0-9._]*", inner))))
  }

  quoted_symbol <- function(message) {
    m <- regmatches(message, regexec("'([^']+)'", message))[[1L]]
    if (length(m) > 1L) m[[2L]] else NA_character_
  }

  # --- object_usage_linter, tidyverse-aware ---------------------------------
  inner_usage <- lintr::object_usage_linter()
  object_usage_linter <- safe_linter(fallback = inner_usage, function(source_expression) {
    lints <- flatten_lints(inner_usage(source_expression))
    if (!length(lints)) return(list())
    ctx <- quiet(file_context(source_expression))
    if (is.null(ctx)) return(lints)
    defs <- project_definitions(ctx$root)
    interpolated <- NULL
    keep <- vapply(lints, function(l) {
      msg <- l$message
      # missing_package_linter reports uninstalled packages more clearly.
      if (startsWith(msg, "Could not find exported symbols for package")) return(FALSE)
      sym <- quoted_symbol(msg)
      if (is.na(sym)) return(TRUE)
      if (startsWith(msg, "no visible global function definition")) {
        return(!(sym %in% ctx$known) && !(sym %in% defs))
      }
      # glue / cli interpolation uses variables inside strings, which
      # codetools cannot see: cli::format_inline("{.val {n}} rows").
      if (grepl("^local variable '.*' assigned but may not be used", msg)) {
        if (is.null(interpolated)) interpolated <<- interpolated_names(ctx$xml)
        return(!(sym %in% interpolated))
      }
      if (startsWith(msg, "no visible binding for global variable")) {
        if (sym %in% ctx$known) return(FALSE)
        node <- xml2::xml_find_first(ctx$xml, sprintf(
          "//SYMBOL[@line1 = %d and @col1 = %d]", l$line_number, l$column_number))
        if (inherits(node, "xml_missing")) return(TRUE)
        return(!isTRUE(quiet(in_nse_call(node, ctx), FALSE)))
      }
      TRUE
    }, logical(1L))
    structure(lints[keep], class = "lints")
  })

  # --- undefined_function_linter --------------------------------------------
  # object_usage_linter only checks code inside function bodies. This checks
  # the rest: top-level calls to functions that exist nowhere in scope. It
  # only looks at *calls*, never bare symbols, so column names in tidyverse
  # pipelines cannot trigger it.
  #
  # It also catches, anywhere in the file, calls to functions that exist in a
  # newer version of an attached package than the one installed for the
  # project (e.g. purrr::list_rbind() with purrr 0.3 pinned by renv).
  undefined_function_linter <- safe_linter(function(source_expression) {
    ctx <- quiet(file_context(source_expression))
    if (is.null(ctx) || ctx$unresolved) return(list())
    xml <- ctx$xml
    all_calls <- xml2::xml_find_all(xml, "//SYMBOL_FUNCTION_CALL")
    if (any(xml2::xml_text(all_calls) %in% dynamic_scope_calls)) return(list())
    ns_used <- xml2::xml_text(xml2::xml_find_all(xml, "//SYMBOL_PACKAGE"))
    if (any(ns_used %in% dynamic_scope_packages)) return(list())
    defs <- project_definitions(ctx$root)
    if (is.null(defs)) return(list())
    in_file <- unique(gsub("^[`'\"]|[`'\"]$", "", xml2::xml_text(xml2::xml_find_all(
      xml, "//SYMBOL | //SYMBOL_FORMALS | //STR_CONST"))))
    # Functions only a different (loaded) version of an attached package has.
    other_version <- list()
    for (pkg in ctx$attached) {
      extra <- setdiff(loaded_exports_if_different(pkg), c(ctx$known, in_file, defs))
      for (fun in extra) other_version[[fun]] <- pkg
    }
    quoting <- paste(sprintf("text() = '%s'", quoting_calls), collapse = " or ")
    candidate <- paste0(
      "//SYMBOL_FUNCTION_CALL[",
      "not(preceding-sibling::NS_GET or preceding-sibling::NS_GET_INT)",
      " and not(preceding-sibling::OP-DOLLAR or preceding-sibling::OP-AT)",
      " and not(ancestor::expr[OP-TILDE])",
      # quote(), rlang::expr() and the like: never evaluated.
      " and not(ancestor::expr[expr[1]/SYMBOL_FUNCTION_CALL[", quoting, "]])"
    )
    in_function <- "ancestor::expr[FUNCTION or OP-LAMBDA]"
    # Inside function bodies object_usage_linter already reports undefined
    # calls, except for the other-version case it cannot see.
    top <- xml2::xml_find_all(xml, paste0(candidate, " and not(", in_function, ")]"))
    nested <- if (length(other_version)) {
      xml2::xml_find_all(xml, paste0(candidate, " and ", in_function, "]"))
    }
    top_names <- xml2::xml_text(top)
    is_undefined <- function(fun) {
      !(fun %in% in_file) && !(fun %in% ctx$known) && !(fun %in% defs) &&
        !exists(fun, envir = globalenv())
    }
    unique_names <- unique(top_names)
    undefined_names <- unique_names[vapply(unique_names, is_undefined, logical(1L))]
    undefined <- top_names %in% c(undefined_names, names(other_version))
    nodes <- top
    if (length(nested)) {
      nodes <- c(as.list(top), as.list(nested))
      undefined <- c(undefined, xml2::xml_text(nested) %in% names(other_version))
    }
    bad <- as.list(nodes)[undefined]
    # pkg::fun() where only a different version of pkg exports fun.
    qualified <- xml2::xml_find_all(xml, "//SYMBOL_FUNCTION_CALL[preceding-sibling::NS_GET]")
    for (node in qualified) {
      fun <- xml2::xml_text(node)
      pkg <- xml2::xml_text(xml2::xml_find_first(node, "preceding-sibling::SYMBOL_PACKAGE"))
      if (is_installed(pkg) && !(fun %in% pkg_exports(pkg)) &&
          fun %in% loaded_exports_if_different(pkg)) {
        other_version[[fun]] <- pkg
        bad <- c(bad, list(node))
      }
    }
    if (!length(bad)) return(list())
    lints <- lapply(bad, function(node) {
      fun <- xml2::xml_text(node)
      pkg <- other_version[[fun]]
      message <- if (is.null(pkg)) sprintf('could not find function "%s"', fun) else {
        sprintf("%s() is not in %s %s, the version installed for this project (it exists in %s %s).",
          fun, pkg, pkg_version(pkg), pkg, as.character(getNamespaceVersion(pkg)))
      }
      lintr::xml_nodes_to_lints(node, source_expression = source_expression,
        lint_message = message, type = "warning")
    })
    structure(lints, class = "lints")
  })

  # --- lifecycle_linter -------------------------------------------------------
  # Reads each called function's lifecycle stage from the installed package:
  # first from the lifecycle::signal_stage() / deprecate_*() call that
  # tidyverse functions make as their first statement, then from the
  # lifecycle badge in the function's help page.
  arg_value <- function(call, name, position) {
    args <- as.list(call)[-1L]
    nms <- names(args) %||% rep("", length(args))
    value <- if (name %in% nms) args[[name]] else {
      unnamed <- args[!nzchar(nms)]
      if (length(unnamed) >= position) unnamed[[position]] else NULL
    }
    if (is.call(value) && identical(value[[1L]], as.name("I"))) value <- value[[2L]]
    if (is.character(value) && length(value) == 1L) value else NULL
  }

  body_stage <- function(fn) {
    b <- body(fn)
    if (!is.call(b)) return(NULL)
    stmts <- if (identical(b[[1L]], as.name("{"))) as.list(b)[-1L] else list(b)
    for (s in stmts) {
      if (!is.call(s)) next
      heads <- all.names(s[[1L]])
      head <- heads[length(heads)]
      if (head %in% c("deprecate_warn", "deprecate_soft", "deprecate_warn0", "deprecate_soft0")) {
        return(list(stage = "deprecated", when = arg_value(s, "when", 1L),
          with = arg_value(s, "with", 3L)))
      }
      if (head %in% c("deprecate_stop", "deprecate_stop0") || grepl("defunct", head)) {
        return(list(stage = "defunct", when = arg_value(s, "when", 1L),
          with = arg_value(s, "with", 3L)))
      }
      if (head == "signal_stage") {
        stage <- arg_value(s, "stage", 1L)
        if (!is.null(stage) && stage %in% c("superseded", "deprecated")) {
          return(list(stage = stage, with = arg_value(s, "with", 3L)))
        }
      }
      if (head == "signal_superseded") return(list(stage = "superseded"))
    }
    NULL
  }

  help_stage <- function(pkg, fun) {
    path <- pkg_path(pkg)
    if (is.na(path)) return(NULL)
    aliases <- quiet(readRDS(file.path(path, "help", "aliases.rds")))
    if (is.null(aliases) || is.na(aliases[fun])) return(NULL)
    rd <- quiet(tools:::fetchRdDB(file.path(path, "help", pkg), aliases[[fun]]))
    if (is.null(rd)) return(NULL)
    desc <- Filter(function(x) identical(attr(x, "Rd_tag"), "\\description"), rd)
    text <- gsub("\\s+", " ", paste(unlist(desc), collapse = ""))
    # A help page shared by several functions may badge only some of them:
    # "[Superseded]: coord_polar() has been in favour of coord_radial()".
    # There, only functions named before the replacement count.
    shared <- sum(aliases == aliases[[fun]]) > 1L
    for (stage in c("defunct", "deprecated", "superseded")) {
      at <- gregexpr(paste0("lifecycle-", stage), text, fixed = TRUE)[[1L]]
      if (at[[1L]] < 0L) next
      if (!shared) return(list(stage = stage))
      for (i in at) {
        after <- sub("^.*?\\[[A-Za-z]+\\]:?", "", substr(text, i, i + 300L), perl = TRUE)
        sentence <- sub("\\.(\\s.*)?$", "", after)
        subject <- sub(paste0("(?i)(in favou?r of|replaced|instead|recommend|please|",
          "\\buse\\b|\\bby\\b|\\bwith\\b).*$"), "", sentence, perl = TRUE)
        named <- regmatches(subject, gregexpr("[A-Za-z.][A-Za-z0-9._]*(?=\\(\\))",
          subject, perl = TRUE))[[1L]]
        if (fun %in% named) return(list(stage = stage))
      }
    }
    NULL
  }

  lifecycle_stage <- function(pkg, fun) {
    # The deprecation helpers themselves (lifecycle::deprecate_stop(),
    # a package's own *_defunct() wrappers) are not deprecated.
    if (pkg == "lifecycle" || grepl("deprecat|defunct|^signal_stage$", fun)) return(NULL)
    memo(paste0("stage:", pkg, "::", fun), function() {
      fn <- project_object(pkg, fun)
      stage <- if (is.function(fn)) quiet(body_stage(fn))
      if (is.null(stage)) stage <- quiet(help_stage(pkg, fun))
      if (!is.null(stage)) {
        stage$pkg <- pkg
        stage$version <- pkg_version(pkg)
      }
      stage
    })
  }

  stage_message <- function(fun, st) {
    with <- if (!is.null(st$with)) sprintf("; use %s instead", st$with) else
      sprintf("; see ?%s::%s for the replacement", st$pkg, fun)
    switch(st$stage,
      superseded = sprintf("%s() is superseded in %s %s: it still works, but new code should not use it%s.",
        fun, st$pkg, st$version, with),
      deprecated = sprintf("%s() is deprecated%s (installed: %s %s)%s.", fun,
        if (!is.null(st$when)) sprintf(" as of %s %s", st$pkg, st$when) else "",
        st$pkg, st$version, with),
      defunct = sprintf("%s() is defunct in %s %s and errors when called%s.",
        fun, st$pkg, st$version, with)
    )
  }

  lifecycle_linter <- safe_linter(function(source_expression) {
    ctx <- quiet(file_context(source_expression))
    if (is.null(ctx)) return(list())
    xml <- ctx$xml
    defined <- unique(xml2::xml_text(xml2::xml_find_all(xml, paste0(
      "//expr[LEFT_ASSIGN or EQ_ASSIGN]/expr[1]/SYMBOL",
      " | //equal_assign/expr[1]/SYMBOL | //expr_or_assign_or_help/expr[1]/SYMBOL",
      " | //SYMBOL_FORMALS"))))
    nodes <- xml2::xml_find_all(xml, "//SYMBOL_FUNCTION_CALL[not(ancestor::expr[OP-TILDE])]")
    if (!length(nodes)) return(list())
    funs <- xml2::xml_text(nodes)
    pkg_nodes <- xml2::xml_find_first(nodes, "preceding-sibling::SYMBOL_PACKAGE")
    pkgs <- xml2::xml_text(pkg_nodes)
    # Bare calls resolve through the attached packages, unless the file
    # defines a function of that name itself.
    bare <- is.na(pkgs) & !(funs %in% defined)
    resolved <- vapply(unique(funs[bare]), resolve_package, character(1L), ctx = ctx)
    pkgs[bare] <- resolved[funs[bare]]
    keys <- unique(paste(pkgs, funs, sep = "::")[!is.na(pkgs)])
    stages <- lapply(keys, function(key) {
      parts <- strsplit(key, "::", fixed = TRUE)[[1L]]
      if (!is_installed(parts[[1L]])) return(NULL)
      quiet(lifecycle_stage(parts[[1L]], parts[[2L]]))
    })
    names(stages) <- keys
    found <- lapply(seq_along(nodes), function(i) {
      if (is.na(pkgs[[i]])) return(NULL)
      st <- stages[[paste(pkgs[[i]], funs[[i]], sep = "::")]]
      if (is.null(st)) return(NULL)
      list(node = nodes[[i]], message = stage_message(funs[[i]], st),
        type = switch(st$stage, superseded = "style", deprecated = "warning", defunct = "error"))
    })
    found <- Filter(Negate(is.null), found)
    if (!length(found)) return(list())
    lints <- lapply(found, function(f) {
      lintr::xml_nodes_to_lints(f$node, source_expression = source_expression,
        lint_message = f$message, type = f$type)
    })
    structure(lints, class = "lints")
  })

  bug_linters <- lintr::linters_with_tags(
    tags = c("correctness", "common_mistakes"),
    exclude_tags = "deprecated"
  )
  # cli-style message bullets repeat names: c(i = "...", i = "..."), also
  # through a package's own wrapper functions.
  bullet_names <- c("i", "x", "v", "!", "*", " ", ">", "")
  bug_linters$duplicate_argument_linter <- filtered_linter(
    lintr::duplicate_argument_linter(),
    function(lint, source_expression) {
      name <- token_at(source_expression$full_xml_parsed_content, lint,
        c("SYMBOL_SUB", "STR_CONST"))
      is.na(name) || !(name %in% bullet_names)
    }
  )
  # "Don't use `::` to access x, which is already imported" is style.
  bug_linters$namespace_linter <- filtered_linter(
    lintr::namespace_linter(),
    function(lint, source_expression) !startsWith(lint$message, "Don't use `::`")
  )
  # Empty arguments are how rlang builds formals, pairlist2(x = ), and
  # tidyverse functions built on rlang dots accept a trailing comma.
  bug_linters$missing_argument_linter <- lintr::missing_argument_linter(except = c(
    "alist", "quote", "switch", "pairlist2", "exprs", "quos", "list2", "dots_list",
    "tibble", "tribble", "lst", "mutate", "transmute", "summarise", "summarize",
    "reframe", "select", "filter", "arrange", "group_by", "rename", "across",
    "pick", "case_when", "case_match", "if_any", "if_all", "expand_grid",
    "crossing", "nesting", "unite", "aes"))
  bug_linters$object_usage_linter <- object_usage_linter
  bug_linters$undefined_function_linter <- undefined_function_linter
  bug_linters$lifecycle_linter <- lifecycle_linter
  bug_linters
})
