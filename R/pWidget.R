# This group of functions construct a pWidget object using the
# parameters provided. Parameters will be checked against the
# requirements dictated by the type of tk widget to be constructed. We
# define a pWidget to be a tk widget such as a button, entry box, ....
#
# name - a tcltk widget corresponding to the pWidget. R tcltk treat a
# tcltk widget;
#
# type - type of a tk widget (e. g. "text" for text box, "list" for
# list box, ...;
#
# parent - the window or frame ... where a given pWidget resides;
#
# text - the text feature of a pWidget whose role varies depending on
# the type of widget. If pWidget is a text box, text will be
# shown in the text box rendered. If pWidget is a button, text will be
# the letters that appear on the rendered clikable button ...;
#
# variable - a variable that will be associated with the given
# pWidget. Most useable for radio buttons or select boxes;
#
# width - the physical width of the pWidget to be rendered. Applicable
# to certain pWidgets;
#
# height - the physical height of the pWidget to be rendered;
#
# vSCroll - set to be TRUE if the pWidget will have a vertical scroll
# bar;
#
# hScroll - set to be TRUE if the pWidget will habe a horizontal
# scroll bar;
#
# preFun - the function defining the operations to be performed on the
# text of the pWidget before rendering the text;
#
# postFun - the function defining the operations to be performed on
# the text of the pWdiget upon existing;
#
#
pWidget <- function(name, type, text, value = "", variable = tclVar(),
                    width = 20, height = 10, vScroll = FALSE,
                    hScroll = FALSE, observers = list(), funs = list(),
                    preFun = function (x) x, postFun = function(x) x){

    .checkArgs(type, text, variable)

    new("pWidget", name = name, type = type,
        text = text, variable = variable, width = width,
        height = height, vScroll = vScroll, hScroll = hScroll,
        observers = observers, funs = funs, preFun = preFun,
        postFun = postFun)
}

.checkArgs <- function(type, text, variable = NULL){
    if(is.null(type) || any(is.na(type), type == "")){
        stop("Invalid argument for \"parent\" and/or \"type\"")
    }
    if(is.null(text) || is.na(text) || text == ""){
        if(any(tolower(type) == c("button", "label", "radio", "select"))){
            stop("Invalid input for \"text\"")
        }
    }
    if((is.null(variable)|| is.na(variable) || variable == "") &&
       any(tolower(type) == c("radio", "select"))){
        stop("Invalid input for \"variable\"")
    }
}

# This function constructs a notifier object
#
# subNObse - a list of list to keep observer names to be notified for
# various views.
#

notifier <- function(view){
     new("notifier", view = view)
}

# This function constructs a tkWidget class with default methods
#
# title <- a character string for the title of the tkWidget to be
# created
#

tkWidget <- function(title, name, updater){
    temp <- new("tkWidget", title = title, name = name, updater = updater)
    base <- tktoplevel()
    tktitle(base) <- title
    winid(temp) <- base
    return(temp)
}

# This function constructs an updater object.
updater <- function(pWidgets = list(), end = list(), views = list()){
    new("updater", pWidgets = pWidgets, end = end, views = views)
}

# This function constructs a widget object with default values if not
# supplied.
# pWidgets - a list of lists with each element being a pWidget object;
# funs - a list of functions that will be associated with buttons on
# the interface of the tcltk widget to be created. The name of the
# function will be the text appears on the button and the function
# will be executed when the button is pressed;
# preFun - a function that will be executed when the tcltk widget is
# constructed;
# postFun - a function that will be executed when the tcltk widget is
# destroyed.
widget <- function(title, pWidgets, funs = list(),
                   preFun = function() "Hello", postFun = function() "Bye"){
    # Keep a copy of the original data
    ORIG <- pWidgets

    updater <- updater(pWidgets = pWidgets, end = list(aWidget = FALSE))
    aWidget <- tkWidget(title = title, name = "aWidget", updater = updater)

    # A Clear, Cancel, and Finish are the default buttons
    canBut <- function(){
        end(updater) <- list(aWidget, FALSE)
        killTK(aWidget)
    }

    finBut <- function(){
        end(updater) <- list(aWidget, TRUE)
        killTK(aWidget)
    }
    cancel <- pWidget(name = "cancel", type = "button", text = "Cancel",
                      width = 12,  funs = list(command = canBut))
    finish <- pWidget(name = "finish", type = "button", text = "finish",
                      width = 12,  funs = list(command = finBut))
    clear <- pWidget(name = "clear", type = "button", text = "Clear",
                     width = 12, funs = list(sclick = function(){
                         tkmessageBox(title = "I will be ready soon",
                                      message = "Do not click me now!")}))
    defaultFuns <- list(clear = clear, cancel = cancel, finish = finish)

    # Make all the widget elements and store the tkwin ids in tkWidget
    widgetids(aWidget) <- .putTKWidgets(pWidgets, funs, defaultFuns, aWidget)
    # Construct a notifier
    notifier <- notifier(aWidget)
    # Wait until the user clicks the Cancel or Finish button
    TKWait(aWidget)
    # Act accordingly based on either the Cancel or Finish botton was
    # clicked
    # construct a widget object
#    new("widget", title = title, pWidgets = pWidgets, funs = funs,
#        preFun = preFun, postFun = postFun)

}

.putTKWidgets <- function(pWidgets, funs, default, aWidget){
    widgetids <- list()
    # Add user defined and default buttons
    if(length(funs) > 0){
        pWidgets[["user"]] <- funs
    }
    pWidgets[["default"]] <- default
    for(i in 1:length(pWidgets)){
        if(length(pWidgets[[i]]) > 1){
            frame1 <- createFrame(aWidget)
            for(j in names(pWidgets[[i]])){
                if(any(getType(pWidgets[[i]][[j]]) ==
                       c("text", "list", "canvas"))){
                    frame2 <- createFrame(frame1)
                    widgetids[[j]] <- createPWidget(aWidget,
                                          pWidgets[[i]][[j]], frame2)
                    doPack(frame2, "left")
                }else{
                    widgetids[[j]] <- createPWidget(aWidget,
                                           pWidgets[[i]][[j]], frame1)
                    doPack(widgetids[[j]], "left")
                    doPack(frame1, "top")
                }
            }
        }else{
            if(any(getType(pWidgets[[i]]) == c("text", "list", "canvas"))){
                frame2 <- createFrame(aWidget)
                widgetids[[i]] <- createPWidget(aWidget,
                                                pWidgets[[i]], frame2)
                doPack(frame2, "top")
            }else{
                widgetids[[i]] <- createPWidget(aWidget, pWidgets[[i]],
                                               getWinid(aWidget))
                doPack(aWidget, "top")
            }
        }
    }
    return(widgetids)
}


# This function is not in use now
.maketkWidget <- function(pWidgets, funs, default, aWidget){
    widgetids <- list()
    # Add user defined and default buttons
    if(length(funs) > 0){
        pWidgets[["user"]] <- funs
    }
    pWidgets[["default"]] <- default

    for(i in 1:length(pWidgets)){
        if(length(pWidgets[[i]]) > 1){
            frame1 <- createFrame(aWidget)
            for(j in names(pWidgets[[i]])){
                if(any(getType(pWidgets[[i]][[j]]) ==
                       c("text", "list", "canvas"))){
                    frame2 <- createFrame(frame1)
                    pWidgets[[i]][[j]] <- createPWidget(pWidgets[[i]][[j]],
                                                        frame2)
                    doPack(frame2, "left")
                }else{
                    pWidgets[[i]][[j]] <- createPWidget(pWidgets[[i]][[j]],
                                                        frame1)
                    doPack(getWinid(pWidgets[[i]][[j]]), "left")
                    doPack(frame1, "top")
                }
            }
        }else{
            if(any(getType(pWidgets[[i]]) == c("text", "list", "canvas"))){
                frame2 <- createFrame(aWidget)
                pWidgets[[i]] <- createPWidget(pWidgets[[i]], frame2)
                doPack(frame2, "top")
            }else{
                pWidgets[[i]] <- createPWidget(pWidgets[[i]],
                                               getWinid(aWidget))
                doPack(aWidget, "top")
            }
        }
    }
}


