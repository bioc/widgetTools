dropdownList <- function(base, options, textvariable, width = 10, default){

    upDateEntry <- function(){
        tkconfigure(entry, state = "normal")
        writeList(entry, getListOption(entry, options), clear = TRUE)
        tkconfigure(entry, state = "disabled")
    }
    if(!missing(default)){
        tclvalue(textvariable) <- default
    }else{
        tclvalue(textvariable) <- options[1]
    }
    dropFrame <- tkframe(base, borderwidth = 2, relief = "sunken")
    entry <- tkentry(dropFrame, width = width, textvariable = textvariable,
                     borderwidth = 1, state = "disabled")
    tkpack(entry, side = "left", expand = TRUE, fill = "both")
    dropBut <- tkbutton(dropFrame, width = 1, text = "v", font = "bold",
                        command = upDateEntry)
    tkpack(dropBut, side = "left", expand = FALSE, fill = "both")

    return(dropFrame)
}

getListOption <- function(targetWidget, options){
    newEntry <- NULL
    end <- function(){
        newEntry <<- as.character(tclObj(selection))[as.integer(
                                                 tkcurselection(list)) + 1]
        tkdestroy(base)
    }
    listWidth <- max(unlist(sapply(options, nchar)))
    selection <- tclVar()
    tclObj(selection) <- options
    tipX <- as.numeric(tkwinfo("rootx", targetWidget))
    tipY <- as.numeric(tkwinfo("rooty", targetWidget)) +
        as.numeric(tkwinfo("height", targetWidget))
                                        # Takes out the frame and title bar
    tkwm.overrideredirect(base <- tktoplevel(), TRUE)
    on.exit(tkdestroy(base))
                                        # Put the TW in the right place
    tkwm.geometry(base, paste("+", tipX, "+", tipY, sep = ""))
    list <- tklistbox(base, listvariable = selection,
                      height = length(options),
                      width = max(unlist(sapply(options, nchar))))
    tkbind(base, "<B1-ButtonRelease>", end)
    tkpack(list, expand = FALSE)

    tkwait.window(base)

    return(newEntry)
}
