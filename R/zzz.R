.onLoad <- function(libname, pkgname, where) {
    require("methods", quietly=TRUE) || stop("Package methods not available")
    capable <- capabilities()
    if (!capable["tcltk"]) {
        stop("The tcltk package is not working.  Please consult the R FAQ")
    }
}

.onAttach <- function(libname, pkgname) {
    if (.Platform$OS.type == "windows")
      suppressWarnings(require("Biobase")) && addVigs2WinMenu("widgetTools")
}


