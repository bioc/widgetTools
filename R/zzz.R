# This function loads the required libraries and initializes the classes

.First.lib <- function(libname, pkgname, where) {
    require(methods, quietly=TRUE) ||
                                 stop("Package methods unavailable!")
    capable <- capabilities()
    if(!capable["tcltk"]){
        warning("The tcl/tk library is not available")
    }else{
        if(interactive()){
            out <- paste("Package tcltk not able to be loaded!")
            if (.Platform$OS.type == "windows")
                out <- paste(out,"\nThe most likely cause of this",
                             "is that your Tcl/Tk installation is",
                             "misconfigured\nPlease see the R",
                             "Windows FAQ, question 3.6:\n",
                             "http://cran.r-project.org/bin/windows/contrib/rw-FAQ.html#Package%20TclTk%20does%20not%20work.")

            require(tcltk, quietly = TRUE) ||
                                 stop(out)
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
