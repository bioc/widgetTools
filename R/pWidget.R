# This group of functions are constructors of objects of the classes
# defined by init.R.
#

entryBox <- function(wName, wEnv, wValue = "", wWidth = 50, wHeight = 0,
                     wFuns = list(), wNotify = list(),
                     wPreFun = function (x) x,
                     wPostFun = function(x) x, wView = new("widgetView")){

    .nameGood(name = wName)
    new("basicPW", wName = wName, wType = "entry", wValue = wValue,
        wWidth = wWidth, wHeight = wHeight, wFuns = wFuns, wNotify = wNotify,
        wPreFun = wPreFun, wPostFun = wPostFun, wEnv = wEnv, wView = wView)
}

textBox <- function(wName, wEnv, wValue = "", wWidth = 25, wHeight = 12,
                    wFuns = list(), wNotify = list(),
                    wPreFun = function (x) x,
                    wPostFun = function(x) x, wView = new("widgetView")){

    .nameGood(name = wName)
    new("basicPW", wName = wName, wType = "text", wValue = wValue,
        wWidth = wWidth, wHeight = wHeight, wFuns = wFuns, wNotify = wNotify,
        wPreFun = wPreFun, wPostFun = wPostFun, wEnv = wEnv, wView = wView)
}

listBox <- function(wName, wEnv, wValue = "", wWidth = 25, wHeight = 10,
                    wFuns = list(), wNotify = list(),
                    wPreFun = function (x) x,
                    wPostFun = function(x) x, wView = new("widgetView")){

    .nameGood(name = wName)
    new("basicPW", wName = wName, wType = "list", wValue = wValue,
        wWidth = wWidth, wHeight = wHeight, wFuns = wFuns, wNotify = wNotify,
        wPreFun = wPreFun, wPostFun = wPostFun, wEnv = wEnv, wView = wView)
}

checkButton <- function(wName, wEnv, wValue, wWidth = 50,
                        wFuns = list(), wNotify = list(),
                        wPreFun = function (x) x,
                        wPostFun = function(x) x, wView = new("widgetView")){

    .nameGood(name = wName)
    new("basicPW", wName = wName, wType = "check", wValue = wValue,
        wWidth = wWidth, wFuns = wFuns, wNotify = wNotify,
        wPreFun = wPreFun, wPostFun = wPostFun, wEnv = wEnv, wView = wView)
}

radioButton <- function(wName, wEnv, wValue, wWidth = 50,
                        wFuns = list(), wNotify = list(),
                        wPreFun = function (x) x,
                        wPostFun = function(x) x, wView = new("widgetView")){

    .nameGood(name = wName)
    new("basicPW", wName = wName, wType = "radio", wValue = wValue,
        wWidth = wWidth, wFuns = wFuns, wNotify = wNotify,
        wPreFun = wPreFun, wPostFun = wPostFun, wEnv = wEnv, wView = wView)
}

label <- function(wName, wEnv, wValue = "", wWidth = 0, wHeight = 0,
                  wFuns = list(), wNotify = list(),
                  wPreFun = function (x) x,
                  wPostFun = function(x) x, wView = new("widgetView")){

    .nameGood(name = wName)
    new("basicPW", wName = wName, wType = "label", wValue = wValue,
        wWidth = wWidth, wHeight = wHeight, wFuns = wFuns, wNotify = wNotify,
        wPreFun = wPreFun, wPostFun = wPostFun, wEnv = wEnv, wView = wView)
}

button <- function(wName, wEnv, wValue = "", wWidth = 12, wHeight = 0,
                   wFuns = list(), wNotify = list(), wPreFun = function (x) x,
                   wPostFun = function(x) x, wView = new("widgetView") ){

    .nameGood(name = wName)
    new("basicPW", wName = wName, wType = "button", wValue = wValue,
        wWidth = wWidth, wHeight = wHeight, wFuns = wFuns, wNotify = wNotify,
        wPreFun = wPreFun, wPostFun = wPostFun, wEnv = wEnv, wView = wView)
}

.nameGood <- function(name){
    if(name == "" || is.null(name) || is.na(name)){
        stop("Invalid name!")
    }
}

widgetView <- function(WVTitle, vName, widgetids = list(),
                       theWidget = new("widget"), winid = NULL){
    if(is.null(winid)){
         winid <- ""
         class(winid) <- "tkwin"
    }
    temp <- new("widgetView", WVTitle = WVTitle, vName = vName,
                widgetids = widgetids, theWidget = theWidget, winid = winid)
    base <- tktoplevel()
    tktitle(base) <- WVTitle
    winid(temp) <- base
    return(temp)
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

widget <- function(wTitle, pWidgets, funs = list(),
                   preFun = function() print("Hello"),
                   postFun = function() print("Bye"), env,
                   defaultNames = c("Finish", "Cancel")){
    # Execute the function that is supposed to run first
    preFun()
    # A variable to keep track of the status
    END <- FALSE
    # A local copy of pWidgets to work on
    localPWs <- pWidgets
    # Construct a widgetView object
    widgetView <- widgetView(WVTitle = wTitle, vName = "widget1")
    # Construct a widget object and assign it to widgetView
    temp <- new("widget", wTitle = wTitle, env = env)
    theWidget(widgetView) <- temp
    # A Clear, Cancel, and Finish are the default buttons
    cancelBut <- function(){
        killWin(widgetView)
    }
    finishBut <- function(){
        END <<-  TRUE
        killWin(widgetView)
    }
    clearBut <- function(){
        .putPW2Env(localPWs, widgetView)
        renewView(widgetView, pWidgets)
    }
    tkcmd("tk_focusFollowsMouse")
    finish <- button(wName = "finish", wValue = defaultNames[1], wWidth = 8,
                     wFuns = list(command = finishBut), wEnv = new.env())
    cancel <- button(wName = "cancel", wValue = defaultNames[2], wWidth = 8,
        wFuns = list(command = cancelBut), wEnv = new.env())
#    clear <- button(wName = "clear", wValue = "Clear", wWidth = 8,
#        wFuns = list(command = clearBut), wEnv = new.env())
    defaultFuns <- list(finish = finish, cancel = cancel)
    if(length(funs) > 0){
        userFun
        s <- list()
        for(i in names(funs)){
            temp <- button(name = i, text = i, width = 12,
                           command = funs[[i]])
            userFuns[[i]] <- temp
        }
        localPWs[["userFuns"]] <- userFuns
    }
    localPWs[["default"]] <- defaultFuns
    # Render the widgets using the local copy
    widgetids(widgetView) <-
        renderWidgets(widgetView, localPWs)
    # Keep a copy of pWidgets and the widgetView in a specified
    # environment
    .putPW2Env(localPWs, widgetView)

    winWait(widgetView)
    # Execute the function to be run at the end
    postFun()
    # Act accordingly based on either the Cancel or Finish botton was
    # clicked
    if(END){
        pWidgets(temp) <- .getChanges(pWidgets)
        return(temp)
    }else{
        #pWidgets(temp) <- pWidgets
        return(NULL)
    }
#    return(temp)
}
# Write the value of the primary widgets to the enviroment
.putPW2Env <- function(pWidgets, widgetView){
    putOne <- function(pWidget){
        if(!is.null(widgetView)){
            wView(pWidget) <- widgetView
        }
        assign(wName(pWidget), pWidget, env = wEnv(pWidget))
    }

    for(i in names(pWidgets)){
        if(length(pWidgets[[i]]) > 1){
            lapply(pWidgets[[i]], putOne)
        }else{
            putOne(pWidgets[[i]])
        }
    }
}
# Gets the values for each primary widget object stored in the
# environment and uses the values to update the values of the a list
# primary widgets passed as an argument.
.getChanges <- function(pWidgets){
    for(i in names(pWidgets)){
        if(length(pWidgets[[i]]) > 0){
            for(j in names(pWidgets[[i]])){
                pWidgets[[i]][[j]] <- get(wName(pWidgets[[i]][[j]]),
                                            env = wEnv(pWidgets[[i]][[j]]))
            }
        }else{
            pWidgets[[i]] <- get(wName(pWidgets[[i]]),
                                            env = wEnv(pWidgets[[i]]))
        }
    }
    return(pWidgets)
}
