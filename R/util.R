pkg_file <- function(..., package = "thesisNord", mustWork = FALSE) {
  if (is.null(devtools_meta(package))) {
    system.file(..., package = package, mustWork = mustWork)
  } else {
    # used only if package has been loaded with devtools or pkgload
    file.path(getNamespaceInfo(package, "path"), "inst", ...)
  }
}
