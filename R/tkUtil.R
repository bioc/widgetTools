# This functions provide common operations that may be performed on
# tcltk widgets.

writeText <- function(widget, value, clear = TRUE){
    if(is.list(value) || is.data.frame(value)){
        value <- capture.output(print(value))
    }
    if(clear){
        tkdelete(widget, "0.0", "end")
    }
    tkinsert(widget, "end", value)
}

writeList <- function(widget, value, clear = TRUE){
    if(is.list(value) || is.data.frame(value)){
        value <- capture.output(print(value))
    }
    if(clear){
        tkdelete(widget, 0, "end")
    }
    do.call(tkinsert, c(list(widget, "end"), as.list(value)))
}

getListValue <- function(which){
    listValue <- NULL
    index <- as.numeric(tkcurselection(which))
    for(i in index){
        listValue <- c(listValue, as.character(tkget(which, i)))
    }
    return(listValue)
}

getTextValue <- function(which){
    return(tclvalue(tkget(which, "0.0", "end")))
}

getEntryValue <- function(which){
    return(tclvalue(tkget(which)))
}

