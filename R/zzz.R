# This function loads the required libraries and initializes the classes

.onLoad <- function(libname, pkgname, where) {
    capable <- capabilities()
    if(!capable["tcltk"]){
        stop(paste("The tcl/tk library is not available in your system.",
                   "Download/install the tcltk library from",
                   "www.tcl.tk/software/tcltk/"))
    }else{
        if(interactive()){
            out <- paste("Package tcltk not able to be loaded!")
            if (.Platform$OS.type == "windows")
                out <- paste(out,"\nThe most likely cause of this",
                             "is that your Tcl/Tk installation is",
                             "misconfigured\nPlease see the R",
                             "Windows FAQ, question 3.6:\n",
                             "http://cran.r-project.org/bin/windows/contrib/rw-FAQ.html#Package%20TclTk%20does%20not%20work.")

            require("tcltk", character.only = TRUE) || stop(out)
        }
    }

    require("methods", character.only = TRUE, quietly = TRUE) ||
    stop("Package methods not available")
    require("tcltk", quietly = TRUE) || stop("Package tcltk unavailalbe!")

 if((.Platform$OS.type == "windows") && ("Biobase" %in% installed.packages()[,"Package"])
    && (interactive()) && (.Platform$GUI ==  "Rgui")){
     if (require("Biobase"))
         addVigs2WinMenu("widgetTools")
 }
}
