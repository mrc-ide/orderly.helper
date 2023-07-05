##' Activate orderly for the current repository.  Run this from
##' anywhere within an orderly archive (i.e., a path containing
##' orderly_config.yml; or where one of its parent directories do). We
##' check what the required orderly minimum version is and set up the
##' orderly namespace for the appropriate version.
##'
##' @title Activate orderly for current reposotory
##' @return Nothing.
##' @author Richard Fitzjohn
activate <- function(verbose = NULL) {
  version <- detect_orderly_version(getwd())
  create_orderly_ns(version, verbose)
}


##' Set up orderly based on your global preferences. This will look at
##' the R option (`orderly.version`) and the environment variable
##' `ORDERLY_VERSION`, in that order, falling back on the second
##' version. Or pass in a version explicitly.
##'
##' @title Set orderly version
##'
##' @param version Either `NULL` (in which case we use global
##'   preferences) or a number `1` or `2`.
##'
##' @export
##' @return Nothing, called for side effects only.
use <- function(version = NULL, verbose = NULL) {
  version <- guess_orderly_version(version)
  create_orderly_ns(version, verbose)
}


##' Deactivate any orderly helper
##'
##' @title Deactivate orderly helper
##' @return Nothing, called for its side effect
##' @export
deactivate <- function() {
  tryCatch(pkgload::unload("orderly"), error = function(e) NULL)
  current$name <- NULL
  current$version <- NULL
  invisible()
}


##' Return information about the state of orderly1, orderly2 and the helper
##'
##' @title Return information about packages
##' @return A list
##' @export
sitrep <- function() {
  loaded <- loadedNamespaces()
  attached <- sub("^package:", "", search())

  f <- function(p) {
    version <- tryCatch(utils::packageVersion(p), error = function(e) NULL)
    list(version = version,
         is_installed = !is.null(version),
         is_loaded = p %in% loaded,
         is_attached = p %in% attached)
  }

  pkg <- c("orderly", "orderly1", "orderly2")
  ret <- lapply(pkg, f)
  names(ret) <- pkg

  if (ret$orderly$is_installed) {
    is_installed <- any(
      file.exists(file.path(.libPaths(), "orderly", "DESCRIPTION")))
    if (!is_installed) {
      ret$orderly <- list(version = NULL,
                          is_installed = FALSE,
                          is_loaded = FALSE,
                          is_attached = FALSE)
    }
  }

  ret$current <- list(version = current$version, name = current$name)

  ret
}


## Persistant package state goes here
current <- new.env(parent = emptyenv())

create_orderly_ns <- function(version, verbose) {
  check_sitrep()
  name <- sprintf("orderly%d", version)
  verbose <- orderly_helper_verbose(verbose)
  if (identical(version, current$version)) {
    if (verbose) {
      message(sprintf("Already using %s", orderly_version_str(version)))
    }
    return(invisible())
  }
  if (verbose) {
    message(sprintf("Using %s", orderly_version_str(version)))
  }
  path <- find.package(name)

  desc_contents <- readLines(file.path(path, "DESCRIPTION"))
  i <- grep("^Package:", desc_contents)
  desc_contents[[i]] <- "Package: orderly"

  exports <- getNamespaceExports(asNamespace(name))
  ns_contents <- c(sprintf('import("%s")', name),
                   sprintf('export("%s")', exports))

  tmp <- temp_package_dir(name)
  writeLines(desc_contents, file.path(tmp, "DESCRIPTION"))
  writeLines(ns_contents, file.path(tmp, "NAMESPACE"))
  res <- pkgload::load_all(tmp, attach = FALSE, quiet = !verbose)

  ## Lastly, we might wire up the help too:
  ##
  ## pkgload:::dev_help(topic_str, package_str) ->
  ##   utils::help(topic_str, "orderly1")
  ##
  ## also system.file and vignette need dealing with; these might be
  ## somewhat trickier though, and devtools/pkgload don't try and pull
  ## it off, so we can't rely on assistance there.
  ##
  ## Also don't support ':::' access; that's reasonable though.

  current$name <- name
  current$version <- version

  invisible()
}


check_sitrep <- function(info = sitrep()) {
  if (info$orderly$is_installed) {
    stop(paste("You have 'orderly' installed; please uninstall it first and",
               "install 'orderly1' and/or 'orderly2' instead"))
  }
  if (info$orderly1$is_attached && info$orderly2$is_attached) {
    stop(paste("You have 'orderly1' and 'orderly2' attached; please",
               "restart your session"))
  }
  invisible(info)
}


temp_package_dir <- function(name) {
  if (is.null(current$path)) {
    current$path <- tempfile()
  }
  path <- file.path(current$path, name)
  dir.create(path, FALSE, TRUE)
  path
}


orderly_helper_verbose <- function(verbose) {
  verbose %||% getOption("orderly.helper.verbose", TRUE)
}


detect_orderly_version <- function(path) {
  root <- find_orderly_root(path)
  if (is.null(root)) {
    stop(sprintf("Did not find orderly root above '%s'", path))
  }
  d <- yaml::yaml.load_file(file.path(root, "orderly_config.yml"))
  version_str <- d$minimum_orderly_version
  if (is.null(version_str)) {
    stop(sprintf("Failed to read required orderly version from '%s'", path))
  }
  version <- numeric_version(version_str)
  if (version < numeric_version("1.99.0")) 1 else 2
}


guess_orderly_version <- function(version) {
  if (!is.null(version)) {
    return(validate_orderly_version(version, "argument 'version'", FALSE))
  }
  
  version <- getOption("orderly.version", NULL)
  if (!is.null(version)) {
    return(validate_orderly_version(version, "option 'orderly.version'"))
  }
  version <- Sys.getenv("ORDERLY_VERSION", NA_character_)
  if (!is.na(version)) {
    return(validate_orderly_version(version,
                                    "environment variable 'ORDERLY_VERSION'",
                                    TRUE))
  }
  2
}


validate_orderly_version <- function(value, name, from_character = FALSE) {
  if (from_character) {
    if (!grepl("^[0-9]$", value)) {
      stop(sprintf("Expected 'version' to be a number (from %s)", name))
    }
    value <- as.numeric(value)
  } else {
    if (!(is.numeric(value) && length(value) == 1 && !is.na(value))) {
      stop(sprintf("Expected 'version' to be scalar number (from %s)", name))
    }
  }
  if (!(value %in% 1:2)) {
    stop(sprintf("Invalid version '%s', expected '1' or '2' (from %s)",
                 value, name))
  }
  value
}


orderly_version_str <- function(major) {
  name <- sprintf("orderly%d", major)
  version <- tryCatch(utils::packageVersion(name), error = function(e) "???")
  sprintf("orderly %d (%s)", major, as.character(version))
}
