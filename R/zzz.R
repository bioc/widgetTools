# This function loads the required libraries and initializes the classes

.First.lib <- function(libname, pkgname, where) {
    require(methods, quietly=TRUE) ||
                                 stop("Package methods unavailable!")
    capable <- capabilities()
    if(!capable["tcltk"]){
        warning("The tcl/tk library is not available")
    }else{
        if(interactive()){
            require(tcltk, quietly = TRUE) ||
                                 stop("Package tcltk unavailable!")
        }
    }

    if(require(Biobase)){
        if(interactive()){
            if(require(tcltk, quietly = TRUE)){ 
                addVig2Menu("widgetTools")
            }
        }
    }

    where <- match(paste("package:", pkgname, sep=""), search())
    .initBasicPW(where)
    .initWidgetView(where)
    .initWidget(where)
}
