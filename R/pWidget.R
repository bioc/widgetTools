# This group of functions are constructors of objects of the classes
# defined by init.R.
#

entryBox <- function(name, env, value = "", width = 50, funs = list(),
                  notify = list(), preFun = function (x) x,
                  postFun = function(x) x){

    .checkEntry(name = name, NULL)
    new("basicPW", name = name, type = "entry", value = value,
        width = width, funs = funs, notify = notify, preFun = preFun,
        postFun = postFun, env = env)
}

textBox <- function(name, env, value = "", width = 20, height = 10,
                  funs = list(), notify = list(), preFun = function(x) x,
                  postFun = function(x) x){
    .checkEntry(name, NULL)
    new("textContainer", name = name, type = "text", value = value,
        width = width, height = height, funs = funs, notify = notify,
        preFun = preFun, postFun = postFun, env = env)
}

listBox <- function(name, env, value = "", width = 20, height = 10,
                  funs = list(), notify = list(), preFun = function(x) x,
                  postFun = function(x) x){

    .checkEntry(name = name, NULL)
    new("textContainer", name = name, type = "list", value = value,
        width = width, height = height, funs = funs, notify = notify,
        preFun = preFun, postFun = postFun, env = env)
}

checkButton <- function(name, text, env, default = name[1],
                        command = function(){}, funs = list(),
                        notify = list(), preFun = function(x) x,
                        postFun = function(x) x){

    .checkEntry(name = name, text = text)
    new("selectBox", name = name, text = text, type = "check",
        default = default, notify = list(), preFun = function(x) x,
        command = command, postFun = function(x) x, env = env)
}

radioButton <- function(name, text, env, default = name[1],
                        command = function() {}, funs = list(),
                        notify = list(), preFun = function(x) x,
                        postFun = function(x) x){
    .checkEntry(name = name, text = text)
    new("selectBox", name = name, text = text, type = "radio",
         default = default, notify = list(), preFun = function(x) x,
         command = command, postFun = function(x) x, env = env)
}

label <- function(name, value, env, width = 0, notify = list(),
        preFun = function(x) x, postFun = function(x) x){
    .checkEntry(name, NULL)
    new("basicPW", name = name, value = value, type = "label",
        width = width, notify = list(), preFun = function(x) x,
                        postFun = function(x) x, env = env)
}

button <- function(name, text, command, env, width = 0, notify = list(),
        preFun = function(x) x, postFun = function(x) x){
    .checkEntry(name, NULL)
    new("button", name = name, type = "button", text = text,
        command = command, width = width, notify = notify,
        preFun = preFun, postFun = postFun, env = env)
}

.checkEntry <- function(name, text){
    if(name == "" || is.null(name) || is.na(name)){
        stop("Invalid name!")
    }
    if(!is.null(text) && length(name) != length(text)){
        stop("The name and text do not match!")
    }
}

widgetView <- function(title, name){
    temp <- new("widgetView", title = title, name = name)
    base <- tktoplevel()
    tktitle(base) <- title
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
widget <- function(title, pWidgets, funs = list(),
                   preFun = function() print("Hello"),
                    postFun = function() print("Bye")){
    # Execute the function that is supposed to run first
    preFun()
    # A variable to keep track of the status
    END <- FALSE
    # A local copy of pWidgets to work on
    localPWs <- pWidgets
    # Construct a widgetView object
    widget1 <- widgetView(title, name = "widget1")
        # A Clear, Cancel, and Finish are the default buttons
    cancelBut <- function(){
        END <<-  FALSE
        killWin(widget1)
    }
    finishBut <- function(){
        END <<-  TRUE
        killWin(widget1)
    }
    clearBut <- function(){
        tkmessageBox(title = "I will be ready soon",
                                      message = "Do not click me now!")
    }
    cancel <- button(name = "cancel", text = "Cancel", width = 12,
        command = cancelBut, env = new.env())
    finish <- button(name = "finish", text = "Finish", width = 12,
        command = finishBut, env = new.env())
    clear <- button(name = "clear", text = "Clear", width = 12,
        command = clearBut, env = new.env())
    defaultFuns <- list(clear = clear, cancel = cancel, finish =
        finish)
    if(length(funs) > 0){
        userFuns <- list()
        for(i in names(funs)){
            temp <- button(name = i, text = i, width = 12,
                           command = funs[[i]])
            userFuns[[i]] <- temp
        }
        localPWs[["userFuns"]] <- userFuns
    }
    localPWs[["default"]] <- defaultFuns
    # Render the widgets using the local copy
    widgetids(widget1) <-
        renderWidgets(widget1, localPWs)
    # Keep a copy of pWidgets and the widgetView in a specified
    # environment
    .put2Env(localPWs, widget1)

    winWait(widget1)
    # Execute the function to be run at the end
    postFun()
    # Act accordingly based on either the Cancel or Finish botton was
    # clicked
    if(END){
        return(new("widget", title = title,
                   pWidgets = .getChanges(pWidgets), funs = funs,
                   preFun = preFun, postFun = postFun))
    }else{
        new("widget", title = title, pWidgets = pWidgets,
            funs = funs, preFun = preFun, postFun = postFun)
    }
}

.put2Env <- function(pWidgets, widgetView){
    putOne <- function(pWidget){
        view(pWidget) <- widgetView
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
