# This functions provide common operations that may be performed on
# tcltk widgets.

updateText <- function(widget, value, clear = TRUE){
    if(clear){
        tkdelete(widget, "0.0", "end")
    }
    tkinsert(widget, "end", value)
}

updateList <- function(widget, value, clear = TRUE){
    if(clear){
        tkdelete(widget, 0, "end")
    }
    tkinsert(widget, "end", value)
}

getOptionValue <- function(winid){
    return(as.character(tkget(winid,(tkcurselection(winid)))))
}

