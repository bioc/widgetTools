# This function makes a viewer that shows the content of an object.
# A viewer is a list box with a scroll bar attached.

makeViewer <- function (target, vWidth = NULL, vHeight = NULL,
                        hScroll = FALSE, vScroll = TRUE,
                        what = "list", side = "left", text = ""){

    if(!is.null(vWidth)){
        if(vWidth <= 0){
            stop("Invalid width value!")
        }
    }else{
        if(!is.null(vHeight)){
            if(vWidth <= 0){
                stop("Invalid height value!")
            }
        }
    }

    aViewer <- .getViewer(target, vWidth, vHeight, what, text)

    if(vScroll){
        vScr <- tkscrollbar(target, orient = "vertical",
                    command = function(...) tkyview(aViewer,...))
        tkconfigure(aViewer,
                yscrollcommand = function(...) tkset(vScr, ...))
        tkpack(vScr, side = "right", fill = "y")
    }
    if(hScroll){
       hScr <- tkscrollbar(target, orient = "horizontal",
                    command = function(...) tkxview(aViewer,...))
       tkconfigure(aViewer,
                xscrollcommand = function(...) tkset(hScr, ...))
       tkpack(hScr, side = "bottom", fill = "x")
    }
    tkpack(aViewer, side = side, fill = "both", expand = TRUE)

    aViewer
}

.getViewer <- function(target, vWidth, vHeight, what, text){

    switch(tolower(what),
           "canvas" = aViewer <- tkcanvas(target),
           "text" = aViewer <- .doText(target, text = text),
           "list" = aViewer <- .doList(target, text = text),
           stop("Wrong viwer definition"))
    if(!is.null(vWidth)){
        tkconfigure(aViewer, width = vWidth)
    }
    if(!is.null(vHeight)){
        tkconfigure(aViewer, height = vHeight)
    }

   return(aViewer)
}

.doList <- function(target, text){
    temp <- tklistbox(target, font = "courier 11")
    writeList(temp, text, clear = TRUE)
    return(temp)
}

.doText <- function(target, text){
    temp <- tktext(target, wrap = "none", font = "courier 11")
    tkinsert(temp, "end", text)
    return(temp)
}



