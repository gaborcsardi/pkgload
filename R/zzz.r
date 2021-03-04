.onLoad <- function(libname, pkgname) {
  # These withr functions are used in load_all() so need to exist in the
  # devtools namespace so the withr namespace is not prematurely loaded by `::`
  # during a load_all() call
  env <- asNamespace(pkgname)
  load_private_package("withr")
  load_private_package("desc")
  assign("withr_with_dir", pkg_data$ns$withr$with_dir, envir = env)
  assign("withr_with_collate", pkg_data$ns$withr$with_collate, envir = env)
  assign("withr_with_envvar", pkg_data$ns$withr$with_envvar, envir = env)
  assign("desc_desc", pkg_data$ns$desc$desc, envir = env)
  assign("desc_desc_get", pkg_data$ns$desc$desc_get, envir = env)
  assign("desc_desc_get_version", pkg_data$ns$desc$desc_get_version, envir = env)
  if (is_installed("testthat")) {
    assign("testthat_source_test_helpers", testthat::source_test_helpers, envir = env)
  } else {
    assign("testthat_source_test_helpers", function(...) TRUE, envir = env)
  }


  nms <- environment(onload_assign)$names
  funs <- environment(onload_assign)$funs
  for (i in seq_along(nms)) {
    assign(nms[[i]], eval(funs[[i]], envir = env), envir = env)
  }

  invisible()
}

pkg_data <- new.env(parent = emptyenv())

load_private_package <- function(package, reg_prefix = "") {

  if (!is.null(pkg_data$ns[[package]])) return()

  ## Load the R code
  pkg_env <- new.env(parent = asNamespace(.packageName))
  pkg_dir <- normalizePath(find.package(package))
  pkg_env[[".packageName"]] <- package
  pkg_env[["__pkg-dir__"]] <- pkg_dir

  reg.finalizer(pkg_env, onexit = TRUE, function(x) {
    tryCatch({
      pkg_dir <- pkg_env[["__pkg-dir__"]]
      if (!is.null(pkg_dir)) pkg_dir <- suppressWarnings(normalizePath(pkg_dir))
      if (!is.null(pkg_env[[".onUnload"]])) {
        tryCatch(pkg_env[[".onUnload"]](pkg_dir), error = function(e) e)
      }
      libs <- .dynLibs()
      paths <- suppressWarnings(normalizePath(vcapply(libs, "[[", "path")))
      matchidx <- grepl(pkg_dir, paths, fixed = TRUE)
      if (any(matchidx)) {
        pkglibs <- libs[matchidx]
        for (lib in pkglibs) dyn.unload(lib[["path"]])
        .dynLibs(libs[!matchidx])
      }
      unlink(dirname(pkg_dir), recursive = TRUE, force = TRUE)
    }, error = function(e) e)
  })

  lazyLoad(file.path(pkg_dir, "R", package), envir = pkg_env)

  ## Reset environments
  set_function_envs(pkg_env, pkg_env)
  ## Sometimes a package refers to its env, this is one known instance.
  ## We could also walk the whole tree, but probably not worth it.
  if (!is.null(pkg_env$err$.internal$package_env)) {
    pkg_env$err$.internal$package_env <- pkg_env
  }

  ## Load shared library
  dll_file <- file.path(pkg_dir, "libs", .Platform$r_arch,
                        paste0(package, .Platform$dynlib.ext))
  if (file.exists(dll_file)) {
    dll <- dyn.load(dll_file)
    dll[["name"]] <- paste0("pkg-", dll[["name"]])
    .dynLibs(c(.dynLibs(), list(dll)))
    natfuns <- getDLLRegisteredRoutines(dll)$.Call
    for (natfun in natfuns) {
      pkg_env[[paste0(reg_prefix, natfun$name)]] <- natfun
    }
  }

  pkg_env[["::"]] <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    if (pkg %in% names(pkg_data$ns)) {
      pkg_data$ns[[pkg]][[name]]
    } else {
      getExportedValue(pkg, name)
    }
  }
  environment(pkg_env[["::"]]) <- pkg_env

  pkg_env[["asNamespace"]] <- function(ns, ...) {
    if (ns %in% names(pkg_data$ns)) {
      pkg_data$ns[[ns]]
    } else {
      base::asNamespace(ns, ...)
    }
  }
  environment(pkg_env[["asNamespace"]]) <- pkg_env

  pkg_env[["UseMethod"]] <- function(generic, object) {
    base::UseMethod(generic, object)
  }
  environment(pkg_env[["UseMethod"]]) <- pkg_env

  ## We add the env before calling .onLoad, because .onLoad might refer
  ## to the package env via asNamespace(), e.g. the ps package does that.
  ## In theory we should handle errors in .onLoad...
  pkg_data$ns[[package]] <- pkg_env
  if (".onLoad" %in% names(pkg_env)) {
    if (package == "callr") {
      px <- pkg_data$ns$processx[["__pkg-dir__"]]
      Sys.setenv(CALLR_PROCESSX_CLIENT_LIB = px)
    }
    withCallingHandlers(
      pkg_env$.onLoad(dirname(pkg_dir), package),
      error = function(e) pkg_data$ns[[package]] <<- NULL
    )
  }

  invisible()
}

set_function_envs <- function(within, new) {
  old <- .libPaths()
  .libPaths(character())
  on.exit(.libPaths(old), add = TRUE)
  nms <- names(within)

  is_target_env <- function(x) {
    identical(x, base::.GlobalEnv) || environmentName(x) != ""
  }

  suppressWarnings({
    for (nm in nms) {
      if (is.function(within[[nm]])) {
        if (is_target_env(environment(within[[nm]])))  {
          environment(within[[nm]]) <- new
        } else if (is_target_env(parent.env(environment(within[[nm]])))) {
          parent.env(environment(within[[nm]])) <- new
        }
      } else if ("R6ClassGenerator" %in% class(within[[nm]])) {
        within[[nm]]$parent_env <- new
        for (mth in names(within[[nm]]$public_methods)) {
          environment(within[[nm]]$public_methods[[mth]]) <- new
        }
        for (mth in names(within[[nm]]$private_methods)) {
          environment(within[[nm]]$private_methods[[mth]]) <- new
        }
      }
    }
  })

  invisible()
}
