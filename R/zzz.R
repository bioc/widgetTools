# This function initializes the classes needed

.First.lib <- function(libname, pkgname, where) {
    require(methods, quietly=TRUE)
    where <- match(paste("package:", pkgname, sep=""), search())
    .initPWidget(where)
}
