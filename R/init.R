# This group of functions initialize all the classes needed for
# widgetTools that renders pWidgets on a widget that hosts several
# interactive tk widget elements. We define a pWidget to be a tk
# widget element such as a button, entry box, label....

# This cuntion initilizes a basic class that all the pWidgets
# contains.
# name - a vector of character string(s) for the names of the tk widget
# elements corresponding to the pWidgets to be created. The name is
# required and assumed to be unique and will be used as the identifier
# for the pWidget. The length of name should be one except for select
# boxes and radio buttons where more than one names can be given;
# type - a character string for the type (e. g. button, list, ...) of
# the pWidget. This slot will be populated automatically by the system;
# value - an optional character string for the value associated with
# the pWidget. If the pWidget is an entry box, a list box, or a text
# box, the value will also be displayed inside the tk widget element
# corresponding to the pWidget;
# width - an integer for the width of the tk widget element
# corresponding to the pWidget to be created;
# funs - an optional list of functions that are to be executed when a
# given action is performed on the tk widget element corresponding to
# the pWidget. The name for the element in the list defines the type
# of action. Currently, only sClick(a single click), dClick(a double
# click), and kPress(a key strick) are the valid action types;
# preFun - an optional function that is to be executed to format the
# string that will be used to set the value of the pWidget gets updated;
# postFun - an optional function that is to be executed to format the
# string stored as the value of the pWidget when it is reterived by
# any operation;
# notify - an optional list of functions that will be executed each
# time the value of the pWidget gets updated.
#

    setClass("basicPW", representation(name = "character",
                                       type = "character",
                                       value = "ANY",
                                       width = "numeric",
                                       height = "numeric",
                                       funs = "list",
                                       preFun = "function",
                                       postFun = "function",
                                       notify = "list",
                                       env = "environment",
                                       view = "widgetView"))
    # Set the get methods
    if(!isGeneric("name")){
        setGeneric("name",
                   function(object) standardGeneric("name"))
    }

    setMethod("name", "basicPW",
              function(object) object@name)

if(!isGeneric("type")){
        setGeneric("type",
                   function(object) standardGeneric("type"))
    }
    setMethod("type", "basicPW",
              function(object) object@type)
    if(!isGeneric("value")){
        setGeneric("value",
                   function(object) standardGeneric("value"))
    }
    setMethod("value", "basicPW",
              function(object) postFun(object)(object@value))
    if(!isGeneric("width")){
        setGeneric("width",
                   function(object) standardGeneric("width"))
    }
    setMethod("width", "basicPW",
              function(object) object@width)
    if(!isGeneric("height")){
        setGeneric("height",
                   function(object) standardGeneric("height"))
    }
    setMethod("height", "basicPW",
              function(object) object@height)
    if(!isGeneric("funs")){
        setGeneric("funs",
                   function(object) standardGeneric("funs"))
    }
    setMethod("funs", "basicPW",
              function(object) object@funs)
    if(!isGeneric("notify")){
        setGeneric("notify",
                   function(object) standardGeneric("notify"))
    }
    setMethod("notify", "basicPW",
              function(object) object@observers)
    if(!isGeneric("preFun")){
        setGeneric("preFun",
                   function(object) standardGeneric("preFun"))
    }
    setMethod("preFun", "basicPW",
              function(object) object@preFun)
    if(!isGeneric("postFun")){
        setGeneric("postFun",
                   function(object) standardGeneric("postFun"))
    }
    setMethod("postFun", "basicPW",
              function(object) object@postFun)
    if(!isGeneric("env")){
        setGeneric("env",
                   function(object) standardGeneric("env"))
    }
    setMethod("env", "basicPW",
              function(object) object@env)
    if(!isGeneric("view")){
        setGeneric("view",
                   function(object) standardGeneric("view"))
    }
    setMethod("view", "basicPW",
              function(object) object@view)
    # Define the replace methods
    if(!isGeneric("name<-")){
        setGeneric("name<-", function(object, value)
                   standardGeneric("name<-"))
    }
    setReplaceMethod("name", "basicPW", function(object, value){
                  object@name <- value; object})
    if(!isGeneric("type<-")){
        setGeneric("type<-", function(object, value)
                   standardGeneric("type<-"))
    }
    setReplaceMethod("type", "basicPW", function(object, value){
                  object@type <- value; object})
    if(!isGeneric("value<-")){
        setGeneric("value<-", function(object, value)
                   standardGeneric("value<-"))
    }
    setReplaceMethod("value", "basicPW", function(object, value){
                  object@value <- preFun(object)(value);
                  if(!is.null(view(object))){
                      updateDisplay(view(object), name(object),
                                    type(object), value)};
                  object})
    if(!isGeneric("width<-")){
        setGeneric("width<-", function(object, value)
                   standardGeneric("width<-"))
    }
    setReplaceMethod("width", "basicPW", function(object, value){
                  object@width <- value; object})
    if(!isGeneric("height<-")){
        setGeneric("height<-", function(object, value)
                   standardGeneric("height<-"))
    }
    setReplaceMethod("height", "basicPW", function(object, value){
                  object@height <- value; object})
    if(!isGeneric("funs<-")){
        setGeneric("funs<-", function(object, value)
                   standardGeneric("funs<-"))
    }
    setReplaceMethod("funs", "basicPW", function(object, value){
                  object@fun <- value; object})
    if(!isGeneric("notify<-")){
        setGeneric("notify<-", function(object, value)
                   standardGeneric("notify<-"))
    }
    setReplaceMethod("notify", "basicPW", function(object, value){
                  object@observers <- value; object})
    if(!isGeneric("preFun<-")){
        setGeneric("preFun<-", function(object, value)
                   standardGeneric("preFun<-"))
    }
    setReplaceMethod("preFun", "basicPW", function(object, value){
                  object@preFun <- value; object})
    if(!isGeneric("postFun<-")){
        setGeneric("postFun<-", function(object, value)
                   standardGeneric("postFun<-"))
    }
    setReplaceMethod("postFun", "basicPW", function(object, value){
                  object@postFun <- value; object})
    if(!isGeneric("env<-")){
        setGeneric("env<-", function(object, value)
                   standardGeneric("env<-"))
    }
    setReplaceMethod("env", "basicPW", function(object, value){
                  object@env <- value; object})
    if(!isGeneric("view<-")){
        setGeneric("view<-", function(object, value)
                   standardGeneric("view<-"))
    }
    setReplaceMethod("view", "basicPW", function(object, value){
                  object@view <- value; object})


# This function initilizes a win class with default functions
# title - a character string for the text to be displayed as the title
# of the widget to be created
# name - a character string for the name of window holding the widget
# elements;
# winid - a tkwin object holding the id for the window;
# widgetids - a list whose elements are the name and tkwin ids for the
# widget elements to be created.
#
    setClass("widgetView", representation(WVTitle = "character",
                                   name = "character",
                                   winid = "tkwin",
                                   widgetids = "list",
                                   theWidget = "widget"))
    # Set the get methods
    if(!isGeneric("name")){
        setGeneric("name",
                   function(object) standardGeneric("name"))
    }
    setMethod("name", "widgetView",
              function(object) object@name)
    if(!isGeneric("winid")){
        setGeneric("winid",
                   function(object) standardGeneric("winid"))
    }
    setMethod("winid", "widgetView",
              function(object) object@winid)
    if(!isGeneric("WVTitle")){
        setGeneric("WVTitle",
                   function(object) standardGeneric("WVTitle"))
    }
    setMethod("WVTitle", "widgetView",
              function(object) object@WVTitle)
    if(!isGeneric("widgetids")){
        setGeneric("widgetids",
                   function(object) standardGeneric("widgetids"))
    }
    setMethod("widgetids", "widgetView",
              function(object) object@widgetids)
    if(!isGeneric("theWidget")){
        setGeneric("theWidget",
                   function(object) standardGeneric("theWidget"))
    }
    setMethod("theWidget", "widgetView",
              function(object) object@theWidget)
    if(!isGeneric("name<-")){
        setGeneric("name<-", function(object, value)
                   standardGeneric("name<-"))
    }
    setReplaceMethod("name", "widgetView", function(object, value){
                  object@name <- value; object})
    if(!isGeneric("winid<-")){
        setGeneric("winid<-", function(object, value)
                   standardGeneric("winid<-"))
    }
    setReplaceMethod("winid", "widgetView", function(object, value){
                  object@winid <- value; object})
    if(!isGeneric("widgetids<-")){
        setGeneric("widgetids<-", function(object, value)
                   standardGeneric("widgetids<-"))
    }
    setReplaceMethod("widgetids", "widgetView", function(object, value){
                  object@widgetids <- value; object})
    if(!isGeneric("theWidget<-")){
        setGeneric("theWidget<-", function(object, value)
                   standardGeneric("theWidget<-"))
    }
    setReplaceMethod("theWidget", "widgetView", function(object, value){
                  object@theWidget <- value; object})
    if(!isGeneric("renderWidgets")){
        setGeneric("renderWidgets",
                   function(widgetView, pWidgets)
                   standardGeneric("renderWidgets"))
    }
    setMethod("renderWidgets", c("widgetView", "list"),
              function(widgetView, pWidgets)
              return(.doWidgets(widgetView, pWidgets)))
    if(!isGeneric("renewView")){
        setGeneric("renewView",
                   function(widgetView, pWidgets)
                   standardGeneric("renewView"))
    }
    setMethod("renewView", c("widgetView", "list"),
              function(widgetView, pWidgets)
                  .renew(widgetView, pWidgets))
    if(!isGeneric("updateDisplay")){
        setGeneric("updateDisplay",
                   function(widgetView, PWName, PWType, value)
                   standardGeneric("updateDisplay"))
    }
    setMethod("updateDisplay", "widgetView",
              function(widgetView, PWName, PWType, value){
                  widgetids <- widgetids(widgetView)
                  if(PWType == "entry"){
                      writeList(widgetids[[PWName]], value)
                  }else{
                      if(PWType == "text"){
                          writeText(widgetids[[PWName]], value)
                      }
                  }
              })
    if(!isGeneric("killWin")){
        setGeneric("killWin",
                   function(tkWidget) standardGeneric("killWin"))
    }
    setMethod("killWin", "widgetView",
              function(tkWidget) tkdestroy(winid(tkWidget)))
    if(!isGeneric("winWait")){
        setGeneric("winWait",
                   function(tkWidget) standardGeneric("winWait"))
    }
    setMethod("winWait", "widgetView",
              function(tkWidget) tkwait.window(winid(tkWidget)))


.doWidgets<- function(tkWidget, pWidgets){
    ENV <- parent.frame(1)
    funlist <- list()
    widgetids <- list()
    doOne <- function(pWidget, parent){
        if(any(type(pWidget) == c("radio", "check"))){
            tempFrame <- tkframe(parent)
            var <- tclVar(match(TRUE, value(pWidget)))
            for(i in 1:length(value(pWidget))){
                temp <- .getWidget(pWidget, tempFrame, i, var)
                fun <- function() {}
                if(type(pWidget) == "radio"){
                    body <- list(as.name("{"),
                             substitute(eval(tkfocus(k), env = ENV),
                                                       list(k = temp)),
                             substitute(eval(updateRadio(
                                    theWidget(tkWidget), name(pWidget),
                                    names(value(pWidget)[z])),
                                    env = ENV), list(z = i)))
                }else{
                    body <- list(as.name("{"),
                             substitute(eval(tkfocus(k), env = ENV),
                                                    list(k = temp)),
                             substitute(eval(updateCheck(
                                    theWidget(tkWidget), name(pWidget),
                                    names(value(pWidget)[z])),
                                    env = ENV), list(z = i)))
                }
                body(fun) <- as.call(body)
                assign(paste("cmd", value(pWidget)[i],sep=""), fun)
                tkconfigure(temp, command = get(paste("cmd",
                                            value(pWidget)[i],sep="")))
                tkpack(temp, side = "left", padx = 2, pady = 1)
                widgetids[[names(value(pWidget)[i])]] <<- temp
            }
            tkpack(tempFrame)
        }else if(any(type(pWidget) == c("list", "text", "entry"))){
            if(type(pWidget) == "entry"){
                temp <- .getWidget(pWidget, parent, 1)
                tkpack(temp, side = "left", padx = 2, pady = 1)
            }else{
                tempFrame <- tkframe(parent)
                temp <- .getWidget(pWidget, tempFrame, 1)
                tkpack(tempFrame, side = "left", padx = 2, pady = 1)
            }
            widgetids[[name(pWidget)]] <<- temp

            if(type(pWidget) == "list"){
                funlist[[name(pWidget)]] <- function(){
                    tkfocus(temp)
                    .getViewerCmd(tkWidget, pWidget, temp)
                }
                tkbind(temp, "<B1-ButtonRelease>", funlist[[name(pWidget)]])
            }else{
                funlist[[name(pWidget)]] <- function(){
                    .getViewerCmd(tkWidget, pWidget, temp)
                }
                tkbind(temp, "<FocusOut>", funlist[[name(pWidget)]])
            }
        }else{
            temp <- .getWidget(pWidget, parent, 1)
            tkpack(temp, side = "left", padx = 2, pady = 1)
            widgetids[[name(pWidget)]] <<- temp
        }
    }


    doRow <- function(aRow){
        if(length(aRow) > 1){
            tempFrame <- tkframe(winid(tkWidget))
            lapply(aRow, doOne, tempFrame)
            tkpack(tempFrame, padx = 2, pady = 1)
        }else{
            doOne(aRow,winid(tkWidget))
        }
    }

    lapply(pWidgets, doRow)
    return(widgetids)
}

.getWidget <- function(pWidget, parent, index = NULL, var = NULL){
    temp <- NULL
    switch(tolower(type(pWidget)),
           "entry" = temp <- .renderEntry(pWidget, parent),
           "text" = ,
           "list" = temp <- .renderViewer(pWidget, parent),
           "label" = temp <- .renderLabel(pWidget, parent),
           "radio" = temp <- .renderRadio(pWidget, parent, index, var),
           "button" = temp <- .renderButton(pWidget, parent),
           "check" = temp <- .renderCheck(pWidget, parent, index),
           stop("Invalid pWidget type"))

    return(temp)
}

.renderEntry <- function(pWidget, parent){
    temp <- tkentry(parent, width = width(pWidget), font = "courier 11")
    if(value(pWidget) != "" && !is.na(value(pWidget)) &&
       !is.null(value(pWidget))){
        writeText(temp, value(pWidget), FALSE)
    }
    return(temp)
}

.renderViewer <- function(pWidget, parent){
    tempFrame <- tkframe(parent)
    if(type(pWidget) == "list"){
        toShow <- names(value(pWidget))
    }else{
        toShow <- value(pWidget)
    }
    temp <- makeViewer(tempFrame, text = toShow,
                    vWidth = width(pWidget), vHeight = height(pWidget),
                    hScroll = TRUE, vScroll = TRUE, what = type(pWidget))
    if(type(pWidget) == "list"){
        tkconfigure(temp, selectmode = "extended")
    }
    tkpack(tempFrame)
    return(temp)
}

.renderRadio <- function(pWidget, parent, index, var){
    temp <- tkradiobutton(parent, text = names(value(pWidget)[index]),
                          value = index, variable = var)
    return(temp)
}

.renderLabel <-function(pWidget, parent){
    temp <- tklabel(parent, text = value(pWidget),
                    width = width(pWidget))
    return(temp)
}

.renderButton <- function(pWidget, parent){
    fun <- list()
    temp <- tkbutton(parent, text = value(pWidget),
                     width = width(pWidget))
    fun[[name(pWidget)]] <- function(){
        tkfocus(temp)
        funs(pWidget)[["command"]]()
    }
    tkconfigure(temp, command = fun[[name(pWidget)]])
    return(temp)
 }

.renderCheck <- function(pWidget, parent, index){
    temp <- tkcheckbutton(parent, text = names(value(pWidget)[index]))
    if(value(pWidget)[index]){
        tkselect(temp)
    }
    return(temp)
}

.getViewerCmd <- function(widgetView, pWidget, widget){
    if(type(pWidget) == "list"){
        tempValue <- getListValue(widget)
        updateRadio(theWidget(widgetView), name(pWidget), tempValue)
    }else if(type(pWidget) == "text"){
        tempValue <- getTextValue(widget)
        updateText(theWidget(widgetView), name(pWidget), tempValue)
    }else{
        tempValue <- getEntryValue(widget)
        updateText(theWidget(widgetView), name(pWidget), tempValue)
    }
}

.renew <- function(widgetView, pWidgets){
    renewOne <- function(pWidget){
        if(type(pWidget) == "radio"){
            tkselect(widgetids(widgetView)
                         [[names(value(pWidget)[value(pWidget) == TRUE])]])
        }else if(type(pWidget) == "check"){
            for(i in names(value(pWidget)[value(pWidget) == TRUE])){
                tkselect(widgetids(widgetView)[[i]])
            }
            for(i in names(value(pWidget)[value(pWidget) != TRUE])){
                tkdeselect(widgetids(widgetView)[[i]])
            }
        }else if(type(pWidget) == "text"){
            writeText(widgetids(widgetView)[[name(pWidget)]],
                                                     value(pWidget))
        }else if(type(pWidget) == "entry"){
            writeList(widgetids(widgetView)[[name(pWidget)]],
                                                     value(pWidget))
        }else if(type(pWidget) == "list"){
            writeList(widgetids(widgetView)[[name(pWidget)]],
                                               names(value(pWidget)))
        }
    }
    for(i in pWidgets){
        if(length(i) > 1){
            lapply(i, renewOne)
        }else{
            renewOne(i)
        }
    }
}

# This function initilizes the widget class and the associsted
# functions.
    setClass("widget", representation(wTitle = "character",
                                      pWidgets = "list",
                                      env = "environment",
                                      funs = "list",
                                      preFun = "function",
                                      postFun = "function"))
    # Set the get methods
    if(!isGeneric("wTitle")){
        setGeneric("wTitle",
                   function(object) standardGeneric("wTitle"))
    }
    setMethod("wTitle", "widget",
              function(object) object@wTitle)
    if(!isGeneric("pWidgets")){
        setGeneric("pWidgets",
                   function(object) standardGeneric("pWidgets"))
    }
    setMethod("pWidgets", "widget",
              function(object) object@pWidgets)
     if(!isGeneric("env")){
        setGeneric("env",
                   function(object) standardGeneric("env"))
    }
    setMethod("env", "widget",
              function(object) object@env)
    if(!isGeneric("funs")){
        setGeneric("funs",
                   function(object) standardGeneric("funs"))
    }
    setMethod("funs", "widget",
              function(object) object@funs)
    if(!isGeneric("preFun")){
        setGeneric("preFun",
                   function(object) standardGeneric("preFun"))
    }
    setMethod("preFun", "widget",
              function(object) object@preFun)
    if(!isGeneric("postFun")){
        setGeneric("postFun",
                   function(object) standardGeneric("postFun"))
    }
    setMethod("postFun", "widget",
              function(object) object@postFun)
    if(!isGeneric("wTitle<-")){
        setGeneric("wTitle<-", function(object, value)
                   standardGeneric("wTitle<-"))
    }
    setReplaceMethod("wTitle", "widget", function(object, value){
                  object@wTitle <- value; object})
    if(!isGeneric("pWidgets<-")){
        setGeneric("pWidgets<-", function(object, value)
                   standardGeneric("pWidgets<-"))
    }
    setReplaceMethod("pWidgets", "widget", function(object, value){
                  object@pWidgets <- value; object})
    if(!isGeneric("env<-")){
        setGeneric("env<-", function(object, value)
                   standardGeneric("env<-"))
    }
    setReplaceMethod("env", "widget", function(object, value){
                  object@env <- value; object})
    if(!isGeneric("funs<-")){
        setGeneric("funs<-", function(object, value)
                   standardGeneric("funs<-"))
    }
    setReplaceMethod("funs", "widget", function(object, value){
                  object@funs <- value; object})
    if(!isGeneric("preFuns<-")){
        setGeneric("preFuns<-", function(object, value)
                   standardGeneric("preFuns<-"))
    }
    setReplaceMethod("preFuns", "widget", function(object, value){
                  object@preFuns <- value; object})
    if(!isGeneric("postFuns<-")){
        setGeneric("postFuns<-", function(object, value)
                   standardGeneric("postFuns<-"))
    }
    setReplaceMethod("postFuns", "widget", function(object, value){
                  object@postFuns <- value; object})

    # Set the interface methods
    if(!isGeneric("updateRadio")){
        setGeneric("updateRadio",
                   function(object, PWName, bName)
                   standardGeneric("updateRadio"))
    }
    setMethod("updateRadio", "widget",
              function(object, PWName, bName) {
                  tempPW <- get(PWName, env = env(object))
                  tempValue <- value(tempPW)
                  tempValue[1:length(tempValue)] <- FALSE
                  tempValue[bName] <- TRUE
                  value(tempPW) <- tempValue
                  assign(name(tempPW), tempPW, env = env(tempPW))
              })
    if(!isGeneric("updateList")){
        setGeneric("updateList",
                   function(object, PWName, opts)
                   standardGeneric("updateList"))
    }
    setMethod("updateList", "widget",
              function(object, PWName, opts) {
                  tempPW <- get(PWName, env = env(object))
                  tempValue <- value(tempPW)
                  tempValue[1:length(tempValue)] <- FALSE
                  tempValue[bName] <- TRUE
                  value(tempPW) <- tempValue
                  assign(name(tempPW), tempPW, env = env(tempPW))
              })
    if(!isGeneric("updateCheck")){
        setGeneric("updateCheck",
                   function(object, PWName, bName)
                   standardGeneric("updateCheck"))
    }
    setMethod("updateCheck", "widget",
              function(object, PWName, bName) {
                  tempPW <- get(PWName, env = env(object))
                  tempValue <- value(tempPW)
                  if(tempValue[bName]){
                      tempValue[bName] <- FALSE
                  }else{
                      tempValue[bName] <- TRUE
                  }
                  value(tempPW) <- tempValue
                  assign(name(tempPW), tempPW, env = env(tempPW))
              })
    if(!isGeneric("updateText")){
        setGeneric("updateText",
                   function(object, PWName, value)
                   standardGeneric("updateText"))
    }
    setMethod("updateText", "widget",
              function(object, PWName, value) {
                  tempPW <- get(PWName, env = env(object))
                  value(tempPW) <- value
                  assign(name(tempPW), tempPW, env = env(tempPW))
              })



