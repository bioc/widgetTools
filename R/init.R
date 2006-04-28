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
     if(!isGeneric("wEnv")){
        setGeneric("wEnv",
                   function(object) standardGeneric("wEnv"))
    }
    setMethod("wEnv", "widget",
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
                  tempPW <- get(PWName, env = wEnv(object))
                  tempValue <- wValue(tempPW)
                  tempValue[1:length(tempValue)] <- FALSE
                  tempValue[bName] <- TRUE
                  wValue(tempPW) <- tempValue
                  assign(wName(tempPW), tempPW, env = wEnv(tempPW))
              })
    if(!isGeneric("updateList")){
        setGeneric("updateList",
                   function(object, PWName, opts)
                   standardGeneric("updateList"))
    }
    setMethod("updateList", "widget",
              function(object, PWName, opts) {
                  tempPW <- get(PWName, env = wEnv(object))
                  tempValue <- wValue(tempPW)
                  tempValue[1:length(tempValue)] <- FALSE
                  tempValue[bName] <- TRUE
                  wValue(tempPW) <- tempValue
                  assign(wName(tempPW), tempPW, env = wEnv(tempPW))
              })
    if(!isGeneric("updateCheck")){
        setGeneric("updateCheck",
                   function(object, PWName, bName)
                   standardGeneric("updateCheck"))
    }
    setMethod("updateCheck", "widget",
              function(object, PWName, bName) {
                  tempPW <- get(PWName, env = wEnv(object))
                  tempValue <- wValue(tempPW)
                  if(tempValue[bName]){
                      tempValue[bName] <- FALSE
                  }else{
                      tempValue[bName] <- TRUE
                  }
                  wValue(tempPW) <- tempValue
                  assign(wName(tempPW), tempPW, env = wEnv(tempPW))
              })
    if(!isGeneric("updateText")){
        setGeneric("updateText",
                   function(object, PWName, value)
                   standardGeneric("updateText"))
    }
    setMethod("updateText", "widget",
              function(object, PWName, value) {
                  tempPW <- get(PWName, env = wEnv(object))
                  wValue(tempPW) <- value
                  assign(wName(tempPW), tempPW, env = wEnv(tempPW))
              })


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
                                      vName = "character",
                                      winid = "ANY",
                                      widgetids = "list",
                                      theWidget = "widget"))
## Set the get methods
if(!isGeneric("vName")){
    setGeneric("vName",
               function(object) standardGeneric("vName"))
}
setMethod("vName", "widgetView",
          function(object) object@vName)
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
if(!isGeneric("vName<-")){
    setGeneric("vName<-", function(object, value)
               standardGeneric("vName<-"))
}
setReplaceMethod("vName", "widgetView", function(object, value){
    object@vName <- value; object})
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


# This group of functions initialize all the classes needed for
# widgetTools that renders pWidgets on a widget that hosts several
# interactive tk widget elements. We define a pWidget to be a tk
# widget element such as a button, entry box, label....

# This funtion initializes a basic class that all the pWidgets
# contain.
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

setClass("basicPW", representation(wName = "character",
                                   wType = "character",
                                   wValue = "ANY",
                                   wWidth = "numeric",
                                   wHeight = "numeric",
                                   wFuns = "list",
                                   wPreFun = "function",
                                   wPostFun = "function",
                                   wNotify = "list",
                                   wEnv = "environment",
                                   wView = "widgetView"))
## Set the get methods
if(!isGeneric("wName")){
    setGeneric("wName",
               function(object) standardGeneric("wName"))
}

setMethod("wName", "basicPW",
          function(object) object@wName)

if(!isGeneric("wType")){
    setGeneric("wType",
               function(object) standardGeneric("wType"))
}
setMethod("wType", "basicPW",
          function(object) object@wType)
if(!isGeneric("wValue")){
    setGeneric("wValue",
               function(object) standardGeneric("wValue"))
}
setMethod("wValue", "basicPW",
          function(object) wPostFun(object)(object@wValue))
if(!isGeneric("wWidth")){
    setGeneric("wWidth",
               function(object) standardGeneric("wWidth"))
}
setMethod("wWidth", "basicPW",
          function(object) object@wWidth)
if(!isGeneric("wHeight")){
    setGeneric("wHeight",
               function(object) standardGeneric("wHeight"))
}
setMethod("wHeight", "basicPW",
          function(object) object@wHeight)
if(!isGeneric("wFuns")){
    setGeneric("wFuns",
               function(object) standardGeneric("wFuns"))
}
setMethod("wFuns", "basicPW",
          function(object) object@wFuns)
if(!isGeneric("wNotify")){
    setGeneric("wNotify",
               function(object) standardGeneric("wNotify"))
}
setMethod("wNotify", "basicPW",
          function(object) object@wNotify)
if(!isGeneric("wPreFun")){
    setGeneric("wPreFun",
               function(object) standardGeneric("wPreFun"))
}
setMethod("wPreFun", "basicPW",
          function(object) object@wPreFun)
if(!isGeneric("wPostFun")){
    setGeneric("wPostFun",
               function(object) standardGeneric("wPostFun"))
}
setMethod("wPostFun", "basicPW",
          function(object) object@wPostFun)
if(!isGeneric("wEnv")){
    setGeneric("wEnv",
               function(object) standardGeneric("wEnv"))
}
setMethod("wEnv", "basicPW",
          function(object) object@wEnv)
if(!isGeneric("wView")){
    setGeneric("wView",
               function(object) standardGeneric("wView"))
}
setMethod("wView", "basicPW",
          function(object) object@wView)
## Define the replace methods
if(!isGeneric("wName<-")){
    setGeneric("wName<-", function(object, value)
               standardGeneric("wName<-"))
}
setReplaceMethod("wName", "basicPW", function(object, value){
    object@wName <- value; object})
if(!isGeneric("wType<-")){
    setGeneric("wType<-", function(object, value)
               standardGeneric("wType<-"))
}
setReplaceMethod("wType", "basicPW", function(object, value){
    object@wType <- value; object})
if(!isGeneric("wValue<-")){
    setGeneric("wValue<-", function(object, value)
               standardGeneric("wValue<-"))
}
setReplaceMethod("wValue", "basicPW", function(object, value){
    object@wValue <- wPreFun(object)(value);
    if(!is.null(wView(object))){
        updateDisplay(wView(object), wName(object),
                      wType(object), value)};
    object})
if(!isGeneric("wWidth<-")){
    setGeneric("wWidth<-", function(object, value)
               standardGeneric("wWidth<-"))
}
setReplaceMethod("wWidth", "basicPW", function(object, value){
    object@wWidth <- value; object})
if(!isGeneric("wHeight<-")){
    setGeneric("wHeight<-", function(object, value)
               standardGeneric("wHeight<-"))
}
setReplaceMethod("wHeight", "basicPW", function(object, value){
    object@wHeight <- value; object})
if(!isGeneric("wFuns<-")){
    setGeneric("wFuns<-", function(object, value)
               standardGeneric("wFuns<-"))
}
setReplaceMethod("wFuns", "basicPW", function(object, value){
    object@wFuns <- value; object})
if(!isGeneric("wNotify<-")){
    setGeneric("wNotify<-", function(object, value)
               standardGeneric("wNotify<-"))
}
setReplaceMethod("wNotify", "basicPW", function(object, value){
    object@wNotify <- value; object})
if(!isGeneric("wPreFun<-")){
    setGeneric("wPreFun<-", function(object, value)
               standardGeneric("wPreFun<-"))
}
setReplaceMethod("wPreFun", "basicPW", function(object, value){
    object@wPreFun <- value; object})
if(!isGeneric("wPostFun<-")){
    setGeneric("wPostFun<-", function(object, value)
               standardGeneric("wPostFun<-"))
}
setReplaceMethod("wPostFun", "basicPW", function(object, value){
    object@wPostFun <- value; object})
if(!isGeneric("wEnv<-")){
    setGeneric("wEnv<-", function(object, value)
               standardGeneric("wEnv<-"))
}
setReplaceMethod("wEnv", "basicPW", function(object, value){
    object@wEnv <- value; object})
if(!isGeneric("wView<-")){
    setGeneric("wView<-", function(object, value)
               standardGeneric("wView<-"))
}
setReplaceMethod("wView", "basicPW", function(object, value){
    object@wView <- value; object})


.doWidgets<- function(tkWidget, pWidgets){
    ENV <- parent.frame(1)
    funlist <- list()
    widgetids <- list()
    doOne <- function(pWidget, parent){
        if(any(wType(pWidget) == c("radio", "check"))){
            tempFrame <- tkframe(parent)
            tempVar <- tclVar(match(TRUE, wValue(pWidget)))
            for(i in 1:length(wValue(pWidget))){
                temp <- .getWidget(pWidget, tempFrame, i, tempVar)
                fun <- function() {}
                if(wType(pWidget) == "radio"){
                    body <- list(as.name("{"),
                                 substitute(eval(tkfocus(k), env = ENV),
                                            list(k = temp)),
                                 substitute(eval(updateRadio(theWidget(tkWidget), wName(pWidget),
                                                             names(wValue(pWidget)[z])),
                                                 env = ENV), list(z = i)))
                }else{
                    body <- list(as.name("{"),
                                 substitute(eval(tkfocus(k), env = ENV),
                                            list(k = temp)),
                                 substitute(eval(updateCheck(theWidget(tkWidget), wName(pWidget),
                                                             names(wValue(pWidget)[z])),
                                                 env = ENV), list(z = i)))
                }
                body(fun) <- as.call(body)
                assign(paste("cmd", wValue(pWidget)[i],sep=""), fun)
                tkconfigure(temp, command = get(paste("cmd",
                                  wValue(pWidget)[i],sep="")))
                tkpack(temp, side = "left", padx = 2, pady = 1)
                widgetids[[names(wValue(pWidget)[i])]] <<- temp
            }
            tkpack(tempFrame)
        }else if(any(wType(pWidget) == c("list", "text", "entry"))){
            if(wType(pWidget) == "entry"){
                temp <- .getWidget(pWidget, parent, 1)
                tkpack(temp, side = "left", padx = 2, pady = 1)
            }else{
                tempFrame <- tkframe(parent)
                temp <- .getWidget(pWidget, tempFrame, 1)
                tkpack(tempFrame, side = "left", padx = 2, pady = 1)
            }
            widgetids[[wName(pWidget)]] <<- temp

            if(wType(pWidget) == "list"){
                funlist[[wName(pWidget)]] <- function(){
                    tkfocus(temp)
                    .getViewerCmd(tkWidget, pWidget, temp)
                }
                tkbind(temp, "<B1-ButtonRelease>", funlist[[wName(pWidget)]])
            }else{
                funlist[[wName(pWidget)]] <- function(){
                    .getViewerCmd(tkWidget, pWidget, temp)
                }
                tkbind(temp, "<FocusOut>", funlist[[wName(pWidget)]])
            }
        }else{
            temp <- .getWidget(pWidget, parent, 1)
            tkpack(temp, side = "left", padx = 2, pady = 1)
            widgetids[[wName(pWidget)]] <<- temp
        }
    }


    doRow <- function(aRow){
        if(length(aRow) > 1){
            tempFrame <- tkframe(winid(tkWidget))
            lapply(aRow, doOne, tempFrame)
            tkpack(tempFrame, padx = 5, pady = 5)
        }else{
            tempFrame <- tkframe(winid(tkWidget))
            doOne(aRow[[1]], tempFrame)
            tkpack(tempFrame, padx = 5, pady = 5)
        }
    }

    lapply(pWidgets, doRow)
    return(widgetids)
}

.getWidget <- function(pWidget, parent, index = NULL, var = NULL){
    temp <- NULL
    switch(tolower(wType(pWidget)),
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
    temp <- tkentry(parent, width = wWidth(pWidget), font = "courier 11")
    if(wValue(pWidget) != "" && !is.na(wValue(pWidget)) &&
       !is.null(wValue(pWidget))){
        writeText(temp, wValue(pWidget), FALSE)
    }
    return(temp)
}

.renderViewer <- function(pWidget, parent){
    tempFrame <- tkframe(parent)
    if(wType(pWidget) == "list"){
        toShow <- names(wValue(pWidget))
    }else{
        toShow <- wValue(pWidget)
    }
    temp <- makeViewer(tempFrame, text = toShow,
                       vWidth = wWidth(pWidget), vHeight = wHeight(pWidget),
                       hScroll = TRUE, vScroll = TRUE, what = wType(pWidget))
    if(wType(pWidget) == "list"){
        tkconfigure(temp, selectmode = "extended")
    }
    tkpack(tempFrame)
    return(temp)
}

.renderRadio <- function(pWidget, parent, index, var){
    temp <- tkradiobutton(parent, text = names(wValue(pWidget)[index]),
                          value = index, variable = var)
    return(temp)
}

.renderLabel <-function(pWidget, parent){
    temp <- tklabel(parent, text = wValue(pWidget),
                    width = wWidth(pWidget))
    return(temp)
}

.renderButton <- function(pWidget, parent){
    fun <- list()
    temp <- tkbutton(parent, text = wValue(pWidget),
                     width = wWidth(pWidget))
    fun[[wName(pWidget)]] <- function(){
        tkfocus(temp)
        wFuns(pWidget)[["command"]]()
    }
    tkconfigure(temp, command = fun[[wName(pWidget)]])
    return(temp)
 }

.renderCheck <- function(pWidget, parent, index){
    tempVar <- basename(tempfile("var"))
    temp <- tkcheckbutton(parent, variable = tempVar,
                          text = names(wValue(pWidget)[index]))
    if(wValue(pWidget)[index]){
        tkselect(temp)
    }
    return(temp)
}

.getViewerCmd <- function(widgetView, pWidget, widget){
    if(wType(pWidget) == "list"){
        tempValue <- getListValue(widget)
        updateRadio(theWidget(widgetView), wName(pWidget), tempValue)
    }else if(wType(pWidget) == "text"){
        tempValue <- getTextValue(widget)
        updateText(theWidget(widgetView), wName(pWidget), tempValue)
    }else{
        tempValue <- getEntryValue(widget)
        updateText(theWidget(widgetView), wName(pWidget), tempValue)
    }
}

.renew <- function(widgetView, pWidgets){
    renewOne <- function(pWidget){
        if(wType(pWidget) == "radio"){
            tkselect(widgetids(widgetView)
                         [[names(wValue(pWidget)[wValue(pWidget) == TRUE])]])
        }else if(wType(pWidget) == "check"){
            for(i in names(wValue(pWidget)[wValue(pWidget) == TRUE])){
                tkselect(widgetids(widgetView)[[i]])
            }
            for(i in names(wValue(pWidget)[wValue(pWidget) != TRUE])){
                tkdeselect(widgetids(widgetView)[[i]])
            }
        }else if(wType(pWidget) == "text"){
            writeText(widgetids(widgetView)[[wName(pWidget)]],
                                                     wValue(pWidget))
        }else if(wType(pWidget) == "entry"){
            writeList(widgetids(widgetView)[[wName(pWidget)]],
                                                     wValue(pWidget))
        }else if(wType(pWidget) == "list"){
            writeList(widgetids(widgetView)[[wName(pWidget)]],
                                               names(wValue(pWidget)))
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

