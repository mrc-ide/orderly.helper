##' Activate orderly for the current repository.
##'
##' @title Activate orderly for current reposotory
##' @return
##' @author Richard Fitzjohn
activate <- function() {
  ## read back to find orderly_config.yml
  ## read version
  ## activate correct version
}


activate_orderly1 <- function() {
  orderly_ns_env("orderly1")
}


activate_orderly2 <- function() {
  orderly_ns_env("orderly2")
}


orderly_ns_env <- function(name_real) {
  name <- "orderly"
  env_real <- pkgload:::ns_env(name_real)
  if (pkgload:::is_loaded(name)) {
    rlang::env_unlock(pkgload:::ns_env(name))
  } else {
    path_real <- find.package(name_real)
    version <- packageVersion(name_real)
    env <- pkgload:::makeNamespace(name, version)
    methods::setPackageName(name, env)
    pkgload:::create_dev_meta(name)
    setNamespaceInfo(env, "path", path_real)
    pkgload:::setup_ns_imports(path_real)
  }
  env <- pkgload:::ns_env(name)
  exports <- names(env_real)
  namespaceExport(env, exports)
  ## for (nm in exports) {
  ##   env[[nm]] <- env_real[[nm]]
  ##   lockBinding(nm, env)
  ## }
  ## lockEnvironment(env)
  env
}







## orderly cannot exist: require that people have not installed it,
## and definitely not loaded it.

sitrep <- function() {
  loaded <- loadedNamespaces()
  attached <- sub("^package:", "", search())


  f <- function(p) {
    version <- tryCatch(utils::packageVersion(p), error = function(e) NULL)
    list(name = p,
         version = version,
         is_installed = !is.null(version),
         is_loaded = p %in% loaded,
         is_attached = p %in% attached)
  }

  pkg <- c("orderly", "orderly1", "orderly2")
  ret <- lapply(pkg, f)
  names(ret) <- pkg
  ret
}


check <- function(info = sitrep()) {
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
