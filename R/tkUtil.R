# This functions provide common operations that may be performed on
# tcltk widgets.

writeText <- function(widget, value, clear = TRUE){
    if(clear){
        tkdelete(widget, "0.0", "end")
    }
    tkinsert(widget, "end", value)
}

writeList <- function(widget, value, clear = TRUE){
    if(clear){
        tkdelete(widget, 0, "end")
    }
    tkinsert(widget, "end", value)
}

getListValue <- function(which){
    return(as.character(tkget(which, tkcurselection(which))))
}

getTextValue <- function(which){
    return(tclvalue(tkget(which, "0.0", "end")))
}

getEntryValue <- function(which){
    return(tclvalue(tkget(which)))
}

