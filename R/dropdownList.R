dropdownList <- function(base, options, textvariable, width = 10,
                         default, editable = FALSE){

    upDateEntry <- function(){
        if(!editable){
            tkconfigure(entry, state = "normal")
        }
        writeList(entry, getListOption(entry, options), clear = TRUE)
        if(!editable){
            tkconfigure(entry, state = "disabled")
        }
    }

    if(!missing(default)){
        if(!is.element(default, options)){
            tkmessageBox(title = "Data error warning",
                         message = paste("The default value \"",
                         default, "\" is not an element of ",
                         "the options \"", paste(options, sep = "",
                                   collapse = ","), "\"", sep = ""),
                         icon = "warning", type = "ok")
        }
        tclvalue(textvariable) <- default
    }else{
        tclvalue(textvariable) <- options[1]
    }
    dropFrame <- tkframe(base, borderwidth = 2, relief = "sunken")
    entry <- tkentry(dropFrame, width = width, textvariable = textvariable,
                     borderwidth = 1)
    if(!editable){
        tkconfigure(entry, state = "disabled")
    }
    tkpack(entry, side = "left", expand = TRUE, fill = "both")
    dropBut <- tkbutton(dropFrame, width = 1, text = "v", font = "bold",
                        command = upDateEntry)
    tkpack(dropBut, side = "left", expand = FALSE, fill = "both")
    tkpack(dropFrame)

    return(invisible())
}

getListOption <- function(targetWidget, options){
    newEntry <- NULL
    end <- function(){
        newEntry <<- as.character(tclObj(selection))[as.integer(
                                                 tkcurselection(list)) + 1]
        tkdestroy(base)
    }
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
