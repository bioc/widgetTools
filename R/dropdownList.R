dropdownList <- function(base, options, textvariable, width = 10,
                         default, editable = FALSE){

    upDateEntry <- function(){
        if(!editable){
            tkconfigure(entry, state = "normal")
        }
        options(show.error.messages = FALSE)
        opt <- try(getListOption(entry, options))
        options(show.error.messages = TRUE)
        if(!inherits(opt, "try-error")){
            writeList(entry, opt, clear = TRUE)
            if(!editable){
                tkconfigure(entry, state = "disabled")
            }
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
    tkpack(dropBut, side = "left", expand = FALSE)
    tkpack(dropFrame, expand = TRUE, fill = "x")

    return(invisible())
}

getListOption <- function(targetWidget, options, height, vScroll = FALSE){
    newEntry <- NULL
    end <- function(){
        newEntry <<- as.character(tclObj(selection))[as.integer(
                                                 tkcurselection(list)) + 1]
        tkgrab.release(base)
        tkdestroy(base)
    }
    if(missing(height)){
        height <- length(options)
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
    list <- tklistbox(base, listvariable = selection, height = height,
                      width = max(unlist(sapply(options, nchar))))
    if(vScroll){
        vScr <- tkscrollbar(base, orient = "vertical",
                    command = function(...) tkyview(list,...))
        tkconfigure(list,
                yscrollcommand = function(...) tkset(vScr, ...))
        tkpack(vScr, side = "right", fill = "y")
    }
    tkbind(list, "<Double-Button-1>", end)
    tkpack(list, expand = FALSE)

    tkgrab.set(base)

    tkwait.window(base)

    return(newEntry)
}
