# This function loads the required libraries and initializes the classes

.First.lib <- function(libname, pkgname, where) {
    require(methods, quietly=TRUE) ||
                                 stop("Package methods unavailable!")
    require(tkWidgets, quietly = TRUE) ||
                                 stop("Package tkWidgets unavailable!")

    where <- match(paste("package:", pkgname, sep=""), search())
    .initBasicPW(where)
    .initWidgetView(where)
    .initWidget(where)
}
