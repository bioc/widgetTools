# This group of functions are constructors of objects of the classes
# defined by init.R.
#

entryBox <- function(name, env, value = "", width = 50, height = 0,
                     funs = list(), notify = list(), preFun = function (x) x,
                     postFun = function(x) x, view = new("widgetView")){

    .nameGood(name = name)
    new("basicPW", name = name, type = "entry", value = value,
        width = width, height = height, funs = funs, notify = notify,
        preFun = preFun, postFun = postFun, env = env, view = view)
}

textBox <- function(name, env, value = "", width = 25, height = 12,
                     funs = list(), notify = list(), preFun = function (x) x,
                     postFun = function(x) x, view = new("widgetView")){

    .nameGood(name = name)
    new("basicPW", name = name, type = "text", value = value,
        width = width, height = height, funs = funs, notify = notify,
        preFun = preFun, postFun = postFun, env = env, view = view)
}

listBox <- function(name, env, value = "", width = 25, height = 10,
                     funs = list(), notify = list(), preFun = function (x) x,
                     postFun = function(x) x, view = new("widgetView")){

    .nameGood(name = name)
    new("basicPW", name = name, type = "list", value = value,
        width = width, height = height, funs = funs, notify = notify,
        preFun = preFun, postFun = postFun, env = env, view = view)
}

checkButton <- function(name, env, value, width = 50,
                     funs = list(), notify = list(), preFun = function (x) x,
                     postFun = function(x) x, view = new("widgetView")){

    .nameGood(name = name)
    new("basicPW", name = name, type = "check", value = value,
        width = width, funs = funs, notify = notify,
        preFun = preFun, postFun = postFun, env = env, view = view)
}

radioButton <- function(name, env, value, width = 50,
                     funs = list(), notify = list(), preFun = function (x) x,
                     postFun = function(x) x, view = new("widgetView")){

    .nameGood(name = name)
    new("basicPW", name = name, type = "radio", value = value,
        width = width, funs = funs, notify = notify,
        preFun = preFun, postFun = postFun, env = env, view = view)
}

label <- function(name, env, value = "", width = 0, height = 0,
                  funs = list(), notify = list(), preFun = function (x) x,
                  postFun = function(x) x, view = new("widgetView")){

    .nameGood(name = name)
    new("basicPW", name = name, type = "label", value = value,
        width = width, height = height, funs = funs, notify = notify,
        preFun = preFun, postFun = postFun, env = env, view = view)
}

button <- function(name, env, value = "", width = 12, height = 0,
                   funs = list(), notify = list(), preFun = function (x) x,
                   postFun = function(x) x, view = new("widgetView") ){

    .nameGood(name = name)
    new("basicPW", name = name, type = "button", value = value,
        width = width, height = height, funs = funs, notify = notify,
        preFun = preFun, postFun = postFun, env = env, view = view)
}

.nameGood <- function(name){
    if(name == "" || is.null(name) || is.na(name)){
        stop("Invalid name!")
    }
}

widgetView <- function(WVTitle, name, widgetids = list(),
                       theWidget = new("widget"), winid = NULL){
    if(is.null(winid)){
         winid <- ""
         class(winid) <- "tkwin"
    }
    temp <- new("widgetView", WVTitle = WVTitle, name = name,
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
                    postFun = function() print("Bye"), env){
    # Execute the function that is supposed to run first
    preFun()
    # A variable to keep track of the status
    END <- FALSE
    # A local copy of pWidgets to work on
    localPWs <- pWidgets
    # Construct a widgetView object
    widgetView <- widgetView(WVTitle = wTitle, name = "widget1")
    # Construct a widget object and assign it to widgetView
    temp <- new("widget", wTitle = wTitle, env = env)
    theWidget(widgetView) <- temp
    # A Clear, Cancel, and Finish are the default buttons
    cancelBut <- function(){
        END <<-  FALSE
        killWin(widgetView)
    }
    finishBut <- function(){
        END <<-  TRUE
        killWin(widgetView)
    }
    clearBut <- function(){
        putPW2Env(localPWs, widgetView)
        renewView(widgetView, pWidgets)
    }
    tkcmd("tk_focusFollowsMouse")
    cancel <- button(name = "cancel", value = "Cancel", width = 8,
        funs = list(command = cancelBut), env = new.env())
    finish <- button(name = "finish", value = "Finish", width = 8,
        funs = list(command = finishBut), env = new.env())
    clear <- button(name = "clear", value = "Clear", width = 8,
        funs = list(command = clearBut), env = new.env())
    defaultFuns <- list(clear = clear, cancel = cancel, finish =
        finish)
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
            view(pWidget) <- widgetView
        }
        assign(name(pWidget), pWidget, env = env(pWidget))
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
                pWidgets[[i]][[j]] <- get(name(pWidgets[[i]][[j]]),
                                            env = env(pWidgets[[i]][[j]]))
            }
        }else{
            pWidgets[[i]] <- get(name(pWidgets[[i]]),
                                            env = env(pWidgets[[i]]))
        }
    }
    return(pWidgets)
}
