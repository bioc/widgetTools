# This function initializes the classes needed

.First.lib <- function(libname, pkgname, where) {
    require(methods, quietly=TRUE)
    require(tcltk, quietly = TRUE)
    require(tkWidgets, quietly = TRUE)

    where <- match(paste("package:", pkgname, sep=""), search())
    .initBasicPW(where)
    .initButton(where)
    .initSelect(where)
    .initTextContainer(where)
    .initWidgetView(where)
    .initWidget(where)
}
