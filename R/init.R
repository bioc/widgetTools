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

.initBasicPW <- function(where){
    setClass("basicPW", representation(name = "character",
                                       type = "character",
                                       value = "character",
                                       width = "numeric",
                                       funs = "list",
                                       preFun = "function",
                                       postFun = "function",
                                       notify = "list",
                                       env = "environment",
                                       view = "widgetView"),
                                       where = where)
    # Set the get methods
    if(!isGeneric("name")){
        setGeneric("name",
                   function(object) standardGeneric("name"),
                   where = where)
    }
    setMethod("name", "basicPW",
              function(object) object@name, where = where)
    if(!isGeneric("type")){
        setGeneric("type",
                   function(object) standardGeneric("type"),
                   where = where)
    }
    setMethod("type", "basicPW",
              function(object) object@type, where = where)
    if(!isGeneric("value")){
        setGeneric("value",
                   function(object) standardGeneric("value"),
                   where = where)
    }
    setMethod("value", "basicPW",
              function(object) postFun(object)(object@value),
              where = where)
    if(!isGeneric("width")){
        setGeneric("width",
                   function(object) standardGeneric("width"),
                   where = where)
    }
    setMethod("width", "basicPW",
              function(object) object@width, where = where)
    if(!isGeneric("funs")){
        setGeneric("funs",
                   function(object) standardGeneric("funs"),
                   where = where)
    }
    setMethod("funs", "basicPW",
              function(object) object@funs, where = where)
    if(!isGeneric("notify")){
        setGeneric("notify",
                   function(object) standardGeneric("notify"),
                   where = where)
    }
    setMethod("notify", "basicPW",
              function(object) object@observers, where = where)
    if(!isGeneric("preFun")){
        setGeneric("preFun",
                   function(object) standardGeneric("preFun"),
                   where = where)
    }
    setMethod("preFun", "basicPW",
              function(object) object@preFun, where = where)
    if(!isGeneric("postFun")){
        setGeneric("postFun",
                   function(object) standardGeneric("postFun"),
                   where = where)
    }
    setMethod("postFun", "basicPW",
              function(object) object@postFun, where = where)
    if(!isGeneric("env")){
        setGeneric("env",
                   function(object) standardGeneric("env"),
                   where = where)
    }
    setMethod("env", "basicPW",
              function(object) object@env, where = where)
    if(!isGeneric("view")){
        setGeneric("view",
                   function(object) standardGeneric("view"),
                   where = where)
    }
    setMethod("view", "basicPW",
              function(object) object@view, where = where)
    # Define the replace methods
    if(!isGeneric("name<-")){
        setGeneric("name<-", function(object, value)
                   standardGeneric("name<-"), where = where)
    }
    setReplaceMethod("name", "basicPW", function(object, value){
                  object@name <- value; object}, where = where)
    if(!isGeneric("type<-")){
        setGeneric("type<-", function(object, value)
                   standardGeneric("type<-"), where = where)
    }
    setReplaceMethod("type", "basicPW", function(object, value){
                  object@type <- value; object}, where = where)
    if(!isGeneric("value<-")){
        setGeneric("value<-", function(object, value)
                   standardGeneric("value<-"), where = where)
    }
    setReplaceMethod("value", "basicPW", function(object, value){
                  object@value <- preFun(object)(value);
                  updateDisplay(view(object), name(object),
                                type(object), value);
                  object}, where = where)
    if(!isGeneric("width<-")){
        setGeneric("width<-", function(object, value)
                   standardGeneric("width<-"), where = where)
    }
    setReplaceMethod("width", "basicPW", function(object, value){
                  object@width <- value; object}, where = where)
    if(!isGeneric("funs<-")){
        setGeneric("funs<-", function(object, value)
                   standardGeneric("funs<-"), where = where)
    }
    setReplaceMethod("funs", "basicPW", function(object, value){
                  object@fun <- value; object}, where = where)
    if(!isGeneric("notify<-")){
        setGeneric("notify<-", function(object, value)
                   standardGeneric("notify<-"), where = where)
    }
    setReplaceMethod("notify", "basicPW", function(object, value){
                  object@observers <- value; object}, where = where)
    if(!isGeneric("preFun<-")){
        setGeneric("preFun<-", function(object, value)
                   standardGeneric("preFun<-"), where = where)
    }
    setReplaceMethod("preFun", "basicPW", function(object, value){
                  object@preFun <- value; object}, where = where)
    if(!isGeneric("postFun<-")){
        setGeneric("postFun<-", function(object, value)
                   standardGeneric("postFun<-"), where = where)
    }
    setReplaceMethod("postFun", "basicPW", function(object, value){
                  object@postFun <- value; object}, where = where)
    if(!isGeneric("env<-")){
        setGeneric("env<-", function(object, value)
                   standardGeneric("env<-"), where = where)
    }
    setReplaceMethod("env", "basicPW", function(object, value){
                  object@env <- value; object}, where = where)
    if(!isGeneric("view<-")){
        setGeneric("view<-", function(object, value)
                   standardGeneric("view<-"), where = where)
    }
    setReplaceMethod("view", "basicPW", function(object, value){
                  object@view <- value; object}, where = where)
    return("Class basicPW initialized")
}

# This function initializes a radio button class by containing the
# select class and defining a new slot.
# var - a tclVar object that is used to group radio buttons together.
#
#.initRadio <- function(where){
#    setClass("radioButton", representation(var = "tclVar"),
#             contains = "selectBox", where = where)
    # Set the get methods
#    if(!isGeneric("getVar")){
#        setGeneric("getVar",
#                   function(object) standardGeneric("getVar"),
#                   where = where)
#    }
#    setMethod("getVar", "radioButton",
#              function(object) object@var, where = where)
#    if(!isGeneric("var<-")){
#        setGeneric("var<-", function(object, value)
#                   standardGeneric("var<-"), where = where)
#    }
#    setReplaceMethod("var", "radioButton", function(object, value){
#                  object@var <- value; object}, where = where)

#    return("Class radio initialized")
#}

# This function initializes a button class by containing the
# basicPW class and defining a new slot.
# text - an optional character that is going to be displayed on the button;
# command - a required function that defines the behavior of the
# button to be created.
#
.initButton <- function(where){
    setClass("button", representation(text = "character",
                                      command = "function"),
             contains = "basicPW", where = where)
    # Set the get methods
    if(!isGeneric("text")){
        setGeneric("text",
                   function(object) standardGeneric("text"),
                   where = where)
    }
    setMethod("text", "button",
              function(object) object@text, where = where)
    if(!isGeneric("command")){
        setGeneric("command",
                   function(object) standardGeneric("command"),
                   where = where)
    }
    setMethod("command", "button",
              function(object) object@command, where = where)
    if(!isGeneric("text<-")){
        setGeneric("text<-", function(object, value)
                   standardGeneric("text<-"), where = where)
    }
    setReplaceMethod("text", "button", function(object, value){
                  object@text <- value; object}, where = where)
    if(!isGeneric("command<-")){
        setGeneric("command<-", function(object, value)
                   standardGeneric("command<-"), where = where)
    }
    setReplaceMethod("command", "button", function(object, value){
                  object@command <- value; object}, where = where)

    return("Class button initialized")
}

# This function initializes select class by containing the basicPW
# class and defining two more new slots,
# text - a vector of character string(s) for name(s) of selections
# that will be available. The length of text should be the same as name;
# default - a character string for the name of the slections (select
# box or radio button) that is going to be the default.
#
.initSelect <- function(where){
    setClass("selectBox", representation(default = "character"),
             contains = "button", where = where)
    # Set the get methods
    if(!isGeneric("default")){
        setGeneric("default",
                   function(object) standardGeneric("default"),
                   where = where)
    }
    setMethod("default", "selectBox",
              function(object) object@default, where = where)
    if(!isGeneric("default<-")){
        setGeneric("default<-", function(object, value)
                   standardGeneric("default<-"), where = where)
    }
    setReplaceMethod("default", "selectBox", function(object, value){
                  object@default <- value; object}, where = where)

    return("Class select initialized")
}

# This function initializes a textContainer class by containing the
# basicPW class and defining a new slot.
# height - an integer for the height (number of lines) of the tk
# widget element corresponding to the pWidget.
#
.initTextContainer <- function(where){
    setClass("textContainer", representation(height = "numeric"),
             contains = "basicPW", where = where)
    # Set the get methods
    if(!isGeneric("height")){
        setGeneric("height",
                   function(object) standardGeneric("height"),
                   where = where)
    }
    setMethod("height", "textContainer",
              function(object) object@height, where = where)
    if(!isGeneric("height<-")){
        setGeneric("height<-", function(object, value)
                   standardGeneric("height<-"), where = where)
    }
    setReplaceMethod("height", "textContainer", function(object, value){
                  object@height <- value; object}, where = where)

    return("Class textContainer initialized")
}

# This function initilizes a win class with default functions
# title - a character string for the text to be displayed as the title
# of the widget to be created
# name - a character string for the name of window holding the widget
# elements;
# winid - a tkwin object holding the id for the window;
# widgetids - a list whose elements are the name and tkwin ids for the
# widget elements to be created.
#
.initWidgetView <- function(where){
    setClass("widgetView", representation(title = "character",
                                   name = "character",
                                   winid = "tkwin",
                                   widgetids = "list"), where = where)
    # Set the get methods
    if(!isGeneric("name")){
        setGeneric("name",
                   function(object) standardGeneric("name"),
                   where = where)
    }
    setMethod("name", "widgetView",
              function(object) object@name, where = where)
    if(!isGeneric("winid")){
        setGeneric("winid",
                   function(object) standardGeneric("winid"),
                   where = where)
    }
    setMethod("winid", "widgetView",
              function(object) object@winid, where = where)
    if(!isGeneric("title")){
        setGeneric("title",
                   function(object) standardGeneric("title"),
                   where = where)
    }
    setMethod("title", "widgetView",
              function(object) object@title, where = where)
    if(!isGeneric("widgetids")){
        setGeneric("widgetids",
                   function(object) standardGeneric("widgetids"),
                   where = where)
    }
    setMethod("widgetids", "widgetView",
              function(object) object@widgetids, where = where)
    if(!isGeneric("name<-")){
        setGeneric("name<-", function(object, value)
                   standardGeneric("name<-"), where = where)
    }
    setReplaceMethod("name", "widgetView", function(object, value){
                  object@name <- value; object}, where = where)
    if(!isGeneric("winid<-")){
        setGeneric("winid<-", function(object, value)
                   standardGeneric("winid<-"), where = where)
    }
    setReplaceMethod("winid", "widgetView", function(object, value){
                  object@winid <- value; object}, where = where)
    if(!isGeneric("widgetids<-")){
        setGeneric("widgetids<-", function(object, value)
                   standardGeneric("widgetids<-"), where = where)
    }
    setReplaceMethod("widgetids", "widgetView", function(object, value){
                  object@widgetids <- value; object}, where = where)
    if(!isGeneric("renderWidgets")){
        setGeneric("renderWidgets",
                   function(tkWidget, pWidgets)
                   standardGeneric("renderWidgets"), where = where)
    }
    setMethod("renderWidgets", c("widgetView", "list"),
              function(tkWidget, pWidgets)
              return(.doWidgets(tkWidget, pWidgets)),
              where = where)
    if(!isGeneric("updateDisplay")){
        setGeneric("updateDisplay",
                   function(widgetView, PWName, PWType, value)
                   standardGeneric("updateDisplay"), where = where)
    }
    setMethod("updateDisplay", "widgetView",
              function(widgetView, PWName, PWType, value){
                  widgetids <- widgetids(widgetView)
                  if(any(PWType == c("list", "entry"))){
                      updateList(widgetids[[PWName]], value)
                  }else{
                      if(PWType == "text"){
                          updateText(widgetids[[PWName]], value)
                      }else{
                          if(PWType == "entry"){
                              updateEntry(widgetids[[PWName]], value)
                          }
                      }
                  }
              }, where = where)
    if(!isGeneric("killWin")){
        setGeneric("killWin",
                   function(tkWidget) standardGeneric("killWin"),
                   where = where)
    }
    setMethod("killWin", "widgetView",
              function(tkWidget) tkdestroy(winid(tkWidget)), where = where)
    if(!isGeneric("winWait")){
        setGeneric("winWait",
                   function(tkWidget) standardGeneric("winWait"),
                   where = where)
    }
    setMethod("winWait", "widgetView",
              function(tkWidget) tkwait.window(winid(tkWidget)),
              where = where)

    return("Class tkWidget initialized")
}

.doWidgets<- function(tkWidget, pWidgets){
    widgetids <- list()
    doOne <- function(pWidget, parent){
        if(any(type(pWidget) == c("radio", "check"))){
            tempFrame <- tkframe(parent)
            var <- tclVar(name)
            for(i in name(pWidget)){
#                if(type(pWidget) == "radio"){
                    temp <- .getWidget(pWidget, tempFrame, var)
#                    tkconfigure(temp, command = radioCommand)
                    tkpack(temp, side = "left")
                    widgetids[[i]] <<- temp
#                }else{
#                    temp <- .getWidget(pWidget, tempFrame, i, NULL)
#                    tkpack(temp, side = "left")
#                    widgetids[[name(pWidget)[i]]] <<- temp
#                }
            }
            tkpack(tempFrame)
        }else{
            if(any(type(pWidget) == c("list", "text"))){
                temp <- .getWidget(pWidget, parent)
                widgetids[[name(pWidget)]] <<- temp
            }else{
                temp <- .getWidget(pWidget, parent, 1)
                tkpack(temp, side = "left")
                widgetids[[name(pWidget)]] <<- temp
            }
        }
    }

    doRow <- function(aRow){
        if(length(aRow) > 1){
            tempFrame <- tkframe(winid(tkWidget))
            lapply(aRow, doOne, tempFrame)
            tkpack(tempFrame)
        }else{
            doOne(aRow,winid(tkWidget))
        }
    }

    lapply(pWidgets, doRow)
    return(widgetids)
}

.getWidget <- function(pWidget, parent, index, var = NULL){
    temp <- NULL
    switch(tolower(type(pWidget)),
           "entry" = temp <- .renderEntry(pWidget, parent),
           "text" = ,
           "list" = temp <- .renderViewer(pWidget, parent),
           "label" = temp <- .renderLabel(pWidget, parent),
           "radio" = temp <- .renderRadio(pWidget, parent, var),
           "button" = temp <- .renderButton(pWidget, parent),
           "check" = temp <- .renderCheck(pWidget, parent, var),
           stop("Invalid pWidget type"))

    return(temp)
}

.renderEntry <- function(pWidget, parent){
    temp <- tkentry(parent, width = width(pWidget), font = "courier 11")
    if(value(pWidget) != "" && !is.na(value(pWidget)) &&
       !is.null(value(pWidget))){
        updateText(temp, value(pWidget), FALSE)
    }
    return(temp)
}

.renderViewer <- function(pWidget, parent){
    tempFrame <- tkframe(parent)
    temp <- makeViewer(tempFrame, text = value(pWidget),
                    vWidth = width(pWidget), vHeight = height(pWidget),
                    hScroll = TRUE, vScroll = TRUE, what = type(pWidget))
    tkpack(tempFrame)
    return(temp)
}

.renderRadio <- function(pWidget, parent, var){
    temp <- tkradiobutton(parent, text = name(pWidget),
                          value = name(pWidget), variable = var)
    return(temp)
}

.renderLabel <-function(pWidget, parent){
    temp <- tklabel(parent, text = value(pWidget),
                    width = width(pWidget))
    return(temp)
}

.renderButton <- function(pWidget, parent){
    temp <- tkbutton(parent, text = text(pWidget),
                     width = width(pWidget),
                     command = command(pWidget))
    return(temp)
}

.renderCheck <- function(pWidget, parent, var){
    temp <- tkcheckbutton(parent, text = name(pWidget),
                          value = name(pWidget), variable = var)
    return(temp)
}

# This function initilizes the widget class and the associsted
# functions.
.initWidget <- function(where){
    setClass("widget", representation(title = "character",
                                      pWidgets = "list",
                                      funs = "list",
                                      preFun = "function",
                                      postFun = "function"),
             where = where)
    # Set the get methods
    if(!isGeneric("title")){
        setGeneric("title",
                   function(object) standardGeneric("title"),
                   where = where)
    }
    setMethod("title", "widget",
              function(object) object@title, where = where)
    if(!isGeneric("pWidgets")){
        setGeneric("pWidgets",
                   function(object) standardGeneric("pWidgets"),
                   where = where)
    }
    setMethod("pWidgets", "widget",
              function(object) object@pWidgets, where = where)
    if(!isGeneric("funs")){
        setGeneric("funs",
                   function(object) standardGeneric("funs"),
                   where = where)
    }
    setMethod("funs", "widget",
              function(object) object@funs, where = where)
    if(!isGeneric("preFun")){
        setGeneric("preFun",
                   function(object) standardGeneric("preFun"),
                   where = where)
    }
    setMethod("preFun", "widget",
              function(object) object@preFun, where = where)
    if(!isGeneric("postFun")){
        setGeneric("postFun",
                   function(object) standardGeneric("postFun"),
                   where = where)
    }
    setMethod("postFun", "widget",
              function(object) object@postFun, where = where)
    if(!isGeneric("title<-")){
        setGeneric("title<-", function(object, value)
                   standardGeneric("title<-"), where = where)
    }
    setReplaceMethod("title", "widget", function(object, value){
                  object@title <- value; object}, where = where)
    if(!isGeneric("pWidgets<-")){
        setGeneric("pWidgets<-", function(object, value)
                   standardGeneric("pWidgets<-"), where = where)
    }
    setReplaceMethod("pWidgets", "widget", function(object, value){
                  object@pWidgets <- value; object}, where = where)
    if(!isGeneric("funs<-")){
        setGeneric("funs<-", function(object, value)
                   standardGeneric("funs<-"), where = where)
    }
    setReplaceMethod("funs", "widget", function(object, value){
                  object@funs <- value; object}, where = where)
    if(!isGeneric("preFuns<-")){
        setGeneric("preFuns<-", function(object, value)
                   standardGeneric("preFuns<-"), where = where)
    }
    setReplaceMethod("preFuns", "widget", function(object, value){
                  object@preFuns <- value; object}, where = where)
    if(!isGeneric("postFuns<-")){
        setGeneric("postFuns<-", function(object, value)
                   standardGeneric("postFuns<-"), where = where)
    }
    setReplaceMethod("postFuns", "widget", function(object, value){
                  object@postFuns <- value; object}, where = where)

    return("Class widget initialized")
}
