# This group of functions initialize all the classes needed for
# widgetTools that renders pWidgets on a widget that hosts several
# interactive tk widgets. We define a pWidget to be a tk widget such as a
# button, entry box, ....
#
# The first cuntion initilizes a pWidget class with default methods.
# parent - the window or frame ... where a given pWidget resides;
# name - a character string for the name of the pWidget;
# winid - a tcltk widget of class "tkwin" corresponding to the pWidget.
# type - type of a tk widget (e. g. "text" for text box, "list" for
# list box, ...;
# parent - the window or frame ... where a given pWidget resides;
# text - the text feature of a pWidget whose role varies depending on
# the type of widget. If pWidget is a text box, text will be shown
# shown in the text box rendered. If pWidget is a button, text will be
# the letters that appear on the rendered clikable button ...
# variable - a variable that will be associated with the given
# pWidget. Most useable for radio buttons or select boxes.
# width - the physical width of the pWidget to be rendered. Applicable
# to certain pWidgets.
# height - the physical height of the pWidget to be rendered.
# vSCroll - set to be TRUE if the pWidget will have a vertical scroll
# bar.
# hScroll - set to be TRUE if the pWidget will habe a horizontal
# scroll bar.
# preFun - the function defining the operations to be performed on the
# text of the pWidget before rendering the text.
# postFun - the function defining the operations to be performed on
# the text of the pWdiget upon existing.
# where - the environment along the search path where the class lives.
#

.initPWidget <- function(where){
    setClass("pWidget", representation(name = "character",
                                       winid = "tkwin",
                                       type = "character",
                                       text = "character",
                                       value = "character",
                                       variable = "tclVar",
                                       width = "numeric",
                                       height = "numeric",
                                       vScroll = "logical",
                                       hScroll = "logical",
                                       observers = "list",
                                       funs = "list",
                                       preFun = "function",
                                       postFun = "function",
                                       env = "environment"),
                                       where = where)
    # Set the get methods
    if(!isGeneric("getName")){
        setGeneric("getName",
                   function(object) standardGeneric("getName"),
                   where = where)
    }
    setMethod("getName", "pWidget",
              function(object) object@name, where = where)
    if(!isGeneric("getWinid")){
        setGeneric("getWinid",
                   function(object) standardGeneric("getWinid"),
                   where = where)
    }
    setMethod("getWinid", "pWidget",
              function(object) object@winid, where = where)
    if(!isGeneric("getType")){
        setGeneric("getType",
                   function(object) standardGeneric("getType"),
                   where = where)
    }
    setMethod("getType", "pWidget",
              function(object) object@type, where = where)
#    if(!isGeneric("getParent")){
#        setGeneric("getParent",
#                   function(object) standardGeneric("getParent"),
#                   where = where)
#    }
#    setMethod("getParent", "pWidget",
#              function(object) object@parent, where = where)
    if(!isGeneric("getText")){
        setGeneric("getText",
                   function(object) standardGeneric("getText"),
                   where = where)
    }
    setMethod("getText", "pWidget",
              function(object) object@text, where = where)
    if(!isGeneric("getValue")){
        setGeneric("getValue",
                   function(object) standardGeneric("getValue"),
                   where = where)
    }
    setMethod("getValue", "pWidget",
              function(object) getPostFun(object)(object@value), where = where)
    if(!isGeneric("getVariable")){
        setGeneric("getVariable",
                   function(object) standardGeneric("getVariable"),
                   where = where)
    }
    setMethod("getVariable", "pWidget",
              function(object) object@variable,
              where = where)
    if(!isGeneric("getWidth")){
        setGeneric("getWidth",
                   function(object) standardGeneric("getWidth"),
                   where = where)
    }
    setMethod("getWidth", "pWidget",
              function(object) object@width, where = where)
    if(!isGeneric("getHeight")){
        setGeneric("getHeight",
                   function(object) standardGeneric("getHeight"),
                   where = where)
    }
    setMethod("getHeight", "pWidget",
              function(object) object@height, where = where)
    if(!isGeneric("getVScroll")){
        setGeneric("getVScroll",
                   function(object) standardGeneric("getVScroll"),
                   where = where)
    }
    setMethod("getVScroll", "pWidget",
              function(object) object@vScroll, where = where)
    if(!isGeneric("getHScroll")){
        setGeneric("getHScroll",
                   function(object) standardGeneric("getHScroll"),
                   where = where)
    }
    setMethod("getHScroll", "pWidget",
              function(object) object@hScroll, where = where)
    if(!isGeneric("getFuns")){
        setGeneric("getFuns",
                   function(object) standardGeneric("getFuns"),
                   where = where)
    }
    setMethod("getFuns", "pWidget",
              function(object) object@funs, where = where)
    if(!isGeneric("getObservers")){
        setGeneric("getObservers",
                   function(object) standardGeneric("getObservers"),
                   where = where)
    }
    setMethod("getObservers", "pWidget",
              function(object) object@observers, where = where)
    if(!isGeneric("getPreFun")){
        setGeneric("getPreFun",
                   function(object) standardGeneric("getPreFun"),
                   where = where)
    }
    setMethod("getPreFun", "pWidget",
              function(object) object@preFun, where = where)
    if(!isGeneric("getFuns")){
        setGeneric("getFuns",
                   function(object) standardGeneric("getFuns"),
                   where = where)
    }
    setMethod("getFuns", "pWidget",
              function(object) object@funs, where = where)
    if(!isGeneric("getPostFun")){
        setGeneric("getPostFun",
                   function(object) standardGeneric("getPostFun"),
                   where = where)
    }
    setMethod("getPostFun", "pWidget",
              function(object) object@postFun, where = where)
    if(!isGeneric("where")){
        setGeneric("where",
                   function(object) standardGeneric("where"),
                   where = where)
    }
    setMethod("where", "pWidget",
              function(object) object@env, where = where)
    # Define the replace methods
    if(!isGeneric("name<-")){
        setGeneric("name<-", function(object, value)
                   standardGeneric("name<-"), where = where)
    }
    setReplaceMethod("name", "pWidget", function(object, value){
                  object@name <- value; object}, where = where)
     if(!isGeneric("winid<-")){
        setGeneric("winid<-", function(object, value)
                   standardGeneric("winid<-"), where = where)
    }
    setReplaceMethod("winid", "pWidget", function(object, value){
                  object@winid <- value; object}, where = where)
    if(!isGeneric("type<-")){
        setGeneric("type<-", function(object, value)
                   standardGeneric("type<-"), where = where)
    }
    setReplaceMethod("type", "pWidget", function(object, value){
                  object@type <- value; object}, where = where)
    if(!isGeneric("text<-")){
        setGeneric("text<-", function(object, value)
                   standardGeneric("text<-"), where = where)
    }
    setReplaceMethod("text", "pWidget", function(object, value){
                  object@text <- value; object}, where = where)
    if(!isGeneric("value<-")){
        setGeneric("value<-", function(object, value)
                   standardGeneric("value<-"), where = where)
    }
    setReplaceMethod("value", "pWidget", function(object, value){
                  object@value <- getPreFun(object)(value);
                  notify(notifier, getName(object), value);
                  object}, where = where)
    if(!isGeneric("variable<-")){
        setGeneric("variable<-", function(object, value)
                   standardGeneric("variable<-"), where = where)
    }
    setReplaceMethod("variable", "pWidget", function(object, value){
                  object@variable <- value; object}, where = where)
    if(!isGeneric("width<-")){
        setGeneric("width<-", function(object, value)
                   standardGeneric("width<-"), where = where)
    }
    setReplaceMethod("width", "pWidget", function(object, value){
                  object@width <- value; object}, where = where)
    if(!isGeneric("height<-")){
        setGeneric("height<-", function(object, value)
                   standardGeneric("height<-"), where = where)
    }
    setReplaceMethod("height", "pWidget", function(object, value){
                  object@height <- value; object}, where = where)
    if(!isGeneric("vScroll<-")){
        setGeneric("vScroll<-", function(object, value)
                   standardGeneric("vScroll<-"), where = where)
    }
    setReplaceMethod("vScroll", "pWidget", function(object, value){
                  object@vScroll <- value; object}, where = where)
    if(!isGeneric("hScroll<-")){
        setGeneric("hScroll<-", function(object, value)
                   standardGeneric("hScroll<-"), where = where)
    }
    setReplaceMethod("hScroll", "pWidget", function(object, value){
                  object@hScroll <- value; object}, where = where)
    if(!isGeneric("funs<-")){
        setGeneric("funs<-", function(object, value)
                   standardGeneric("funs<-"), where = where)
    }
    setReplaceMethod("funs", "pWidget", function(object, value){
                  object@fun <- value; object}, where = where)
    if(!isGeneric("observers<-")){
        setGeneric("observers<-", function(object, value)
                   standardGeneric("observers<-"), where = where)
    }
    setReplaceMethod("observers", "pWidget", function(object, value){
                  object@observers <- value; object}, where = where)
    if(!isGeneric("preFun<-")){
        setGeneric("preFun<-", function(object, value)
                   standardGeneric("preFun<-"), where = where)
    }
    setReplaceMethod("preFun", "pWidget", function(object, value){
                  object@preFun <- value; object}, where = where)
    if(!isGeneric("postFun<-")){
        setGeneric("postFun<-", function(object, value)
                   standardGeneric("postFun<-"), where = where)
    }
    setReplaceMethod("postFun", "pWidget", function(object, value){
                  object@postFun <- value; object}, where = where)
    # Set the update function
#    if(!isGeneric("attach")){
#        setGeneric("attach",
#                   function(view, subject, observer)
#                   standardGeneric("attach"), where = where)
#    }
#    setMethod("attach", "notifier",
#              function(notifier, view, subject, observer)
#              notifier@register(view, subject, observer),
#              where = where)
#    if(!isGeneric("detach")){
#        setGeneric("detach",
#                   function(view, subject, observer)
#                   standardGeneric("detach"), where = where)
#    }
#    setMethod("attach", "pWidget",
#              function(view, subject, observer)
#              notifier@unRegister(view, subject, observer),
#              where = where)
#    if(!isGeneric("update")){
#        setGeneric("update",
#                   function(object, value)
#                   standardGeneric("update"), where = where)
#   }
#    setMethod("update", "pWidget",
#             function(object, value) notifyView(object, value),
#              where = where)

    return("Class pWidget initialized")
}

# This function initilizes a notifier class with default methods.
# subNObes - an R environment object that contains R environment
# objects. The keys for the top level R environment object are the names
# of view objects the notifier knows and the keys for the second level
# R environment are names for pWidgets for the view defined by a key
# of the top level R environment.
#

.initNotifier <- function(where){
    setClass("notifier", representation(subNObse = "list"),
              where = where)
    # Set the get method
    if(!isGeneric("getSubNObse")){
        setGeneric("getSubNObse",
                   function(object) standardGeneric("getSubNObse"),
                   where = where)
    }
    setMethod("getSubNObse", "notifier",
              function(object) object@subNObse, where = where)

    # Set the set method
    if(!isGeneric("subNObse<-")){
        setGeneric("subNObse<-", function(object, value)
                   standardGeneric("subNObse<-"), where = where)
    }
    setReplaceMethod("subNObse", "notifier", function(object, value){
                  object@subNObse <- value; object}, where = where)

    # Set the interface functions
    if(!isGeneric("register")){
        setGeneric("register",
                   function(notifier, subject, observer)
                   standardGeneric("register"), where = where)
    }
    setMethod("register", c("notifier", "character", "pWidget"),
              function(notifier, subject, observer) {
                  notifier@subNObse <- .writeSubNObse(notifier@subNObse,
                                       subject, observer);
                                       return(notifier)}, where = where)
    if(!isGeneric("unRegister")){
        setGeneric("unRegister",
                   function(notifier, subject, observer)
                   standardGeneric("unRegister"), where = where)
    }
    setMethod("unRegister", c("notifier", "character", "pWidget"),
              function(notifier, subject, observer){
                  notifier@subNObse <- .modSubNObse(notifier@subNObse,
                                       subject, observer);
                                       return(notifier)}, where = where)
    if(!isGeneric("notify")){
        setGeneric("notify",
                   function(notifier, subject, value)
                   standardGeneric("notify"), where = where)
    }
    setMethod("notify", c("notifier", "character", "character"),
              function(notifier, subject, value){
                  if(!is.null(getSubNObse(notifier)[[subject]]))
                         .writeView(getSubNObse(notifier)[[subject]],
                                    value)}, where = where)
    return("Class notifier initialized")
}

.writeView <- function(observers, value){
    writeWidget <- function(widget, type, value){
        switch(tolower(type),
               "text" = updateText(widget, value),
               "entry" = ,
               "list" = updateList(widget, value),
               stop("Unknow widget type!"))
    }
    for(i in names(observers)){
        writeWidget(getWinid(observers[[i]]), getType(observers[[i]]),
                    value)
        updatePWidget(updater, getName(observers[[i]]), value)
    }
}

# Called by register of .initNotifier to register views, subjects, and
# observers
.writeSubNObse <- function(subNObse, subject, observer){
    if(is.null(subNObse[[subject]])){
        subNObse[[subject]][[getName(observer)]] <- observer
    }else{
        isNew <- TRUE
        for(i in names(subNObse[[subject]])){
            if(i == getName(observer)){
                isNew <- FALSE
            }
        }
        if(isNew){
            subNObse[[subject]][[getName(observer)]] <- observer
        }
    }
    return(subNObse)
}

# Called by unRegister of .initNotifier to unregister views, subjects, and
# observers
.modSubNObse <- function(subNObse, subject, observer){
    if(!is.null(subNObse[[subject]])){
        for(i in names(subNObse[[subject]])){
            if(i == getName(observer)){
                subNObse[[subject]] <-
                  subNObse[[subject]][names(subNObse[[subject]]) != i]
            }
        }
        if(length(subNObse[[subject]]) < 1){
            subNObse <- subNObse[names(subNObse) != subject]
        }
    }
    return(subNObse)
}

# This function initilizes tkWidget class with default functions
# title - a character string for the text to be displayed as the title
# of the widget to be created
# name - a "tkwin" object that is the toplevel of the widget to be created
#
.initTKWidget <- function(where){
    setClass("tkWidget", representation(title = "character",
                                        name = "tkwin",
                                        cancelled = "logical"), where = where)
    # Set the get methods
    if(!isGeneric("getName")){
        setGeneric("getName",
                   function(object) standardGeneric("getName"),
                   where = where)
    }
    setMethod("getName", "tkWidget",
              function(object) object@name, where = where)
    if(!isGeneric("getTitle")){
        setGeneric("getTitle",
                   function(object) standardGeneric("getTitle"),
                   where = where)
    }
    setMethod("getTitle", "tkWidget",
              function(object) object@title, where = where)
    if(!isGeneric("getCancelled")){
        setGeneric("getCancelled",
                   function(object) standardGeneric("getCancelled"),
                   where = where)
    }
    setMethod("getCancelled", "tkWidget",
              function(object) object@cancelled, where = where)
    if(!isGeneric("cancelled<-")){
        setGeneric("cancelled<-", function(object, value)
                   standardGeneric("cancelled<-"), where = where)
    }
    setReplaceMethod("cancelled", "tkWidget", function(object, value){
                  object@cancelled <- value; object}, where = where)
    # Create the interface methods
    if(!isGeneric("createFrame")){
        setGeneric("createFrame",
                   function(object)
                   standardGeneric("createFrame"), where = where)
    }
    setMethod("createFrame", "tkWidget",
              function(object)
              return(tkframe(getName(object))), where = where)
    if(!isGeneric("createPWidget")){
        setGeneric("createPWidget",
                   function(pWidget, parent)
                   standardGeneric("createPWidget"), where = where)
    }
    setMethod("createPWidget", "pWidget",
              function(pWidget, parent)
              return(.getAWidget(pWidget, parent)), where = where)
    if(!isGeneric("doPack")){
        setGeneric("doPack",
                   function(what, side = "left")
                   standardGeneric("doPack"), where = where)
    }
    if(!isGeneric("killTK")){
        setGeneric("killTK",
                   function(tkWidget) standardGeneric("killTK"),
                   where = where)
    }
    setMethod("killTK", "tkWidget",
              function(tkWidget) tkdestroy(getName(tkWidget)), where = where)

#    setMethod("doPack", "pWidget",
#              function(what, side = "left"){
#                  if(!any(getType(pWidget) == c("text", "list", "canvas")))
#              tkpack(getName(what), side = side)}, where = where)
    setMethod("doPack", "ANY",
              function(what, side = "left")
              tkpack(what, side = side), where = where)
    return("Class tkWidget initialized")
}

.getAWidget <- function(pWidget, parent){

    switch(tolower(getType(pWidget)),
           "entry" = temp <- tkentry(parent, text = getText(pWidget),
                            width = getWidth(pWidget)),
           "button" = temp <- tkbutton(parent, text = getText(pWidget),
                            width = getWidth(pWidget)),
           "text" = ,
           "canvas" = ,
           "list" = temp <- makeViewer(parent, text = getText(pWidget),
                            vWidth = getWidth(pWidget),
                            vHeight = getHeight(pWidget),
                            hScroll = getHScroll(pWidget),
                            vScroll = getVScroll(pWidget),
                            what = getType(pWidget)),
           "label" = temp <- tklabel(parent, text = getText(pWidget),
                             width = getWidth(pWidget)),
           "radio" = temp <- tkradiobutton(parent, text = getText(pWidget),
                             value = as.numeric(getValue(pWidget)),
                             variable = getVariable(pWidget)),
           stop("Invalid pWidget type"))

    funs <- getFuns(pWidget)

    for(i in names(funs)){
        switch(tolower(i),
                       "sclick" = act <- "<B1-ButtonRelease>",
                       "dclick" = act <- "<Double-Button-1>",
                       "type" = act <- "<KeyPress>",
                       stop("Unknow action"))

        tkbind(temp, act, funs[[i]])
    }

    winid(pWidget) <-  temp
    return(pWidget)
}

# This function initializes the updater class
.initUpdater <- function(where){

    setClass("updater", representation(pWidgets = "list"), where = where)
    # Set the new methods
    if(!isGeneric("ended")){
        setGeneric("ended",
                   function(object) standardGeneric("ended"),
                   where = where)
    }
    setMethod("ended", "updater",
              function(object) {print("will be working")},
              where = where)
    if(!isGeneric("cancelled")){
        setGeneric("cancelled",
                   function(object) standardGeneric("cancelled"),
                   where = where)
    }
    setMethod("cancelled", "updater",
              function(object) {print("will be working")},
              where = where)
    if(!isGeneric("clear")){
        setGeneric("clear",
                   function(object) standardGeneric("clear"),
                   where = where)
    }
    setMethod("clear", "updater",
              function(object) {print("will be working")},
              where = where)
    if(!isGeneric("updatePWidget")){
        setGeneric("updatePWidget",
                   function(updater, PWName, value)
                   standardGeneric("updatePWidget"), where = where)
    }
    setMethod("updatePWidget", "updater",
              function(updater, PWName, value) {
                  pWidget <- get(PWName);
                  value(pWidget) <- value;
                  assign(PWName, pWidget, env = where(pWidget))},
              where = where)

    return("Class updater initialized")
}

# This function defines the widget class and methods
.initWidget <- function(where){
    setClass("widget", representation(title = "character",
                                      pWidgets = "list",
                                      funs = "list",
                                      preFun = "function",
                                      postFun = "function"), where = where)
    # Set the new methods
    if(!isGeneric("getTitle")){
        setGeneric("getTitle",
                   function(object) standardGeneric("getTitle"),
                   where = where)
    }
    setMethod("getTitle", "widget",
              function(object) object@pWidgets, where = where)
    if(!isGeneric("getPWidgets")){
        setGeneric("getPWidgets",
                   function(object) standardGeneric("getPWidgets"),
                   where = where)
    }
    setMethod("getPWidgets", "widget",
              function(object) object@pWidgets, where = where)
    if(!isGeneric("getFuns")){
        setGeneric("getFuns",
                   function(object) standardGeneric("getFuns"),
                   where = where)
    }
    setMethod("getFuns", "widget",
              function(object) object@funs, where = where)
    if(!isGeneric("getPreFun")){
        setGeneric("getPreFun",
                   function(object) standardGeneric("getPreFun"),
                   where = where)
    }
    setMethod("getPreFun", "widget",
              function(object) object@preFun, where = where)
    if(!isGeneric("getPostFun")){
        setGeneric("getPostFun",
                   function(object) standardGeneric("getPostFun"),
                   where = where)
    }
    setMethod("getPostFun", "widget",
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
    if(!isGeneric("preFun<-")){
        setGeneric("preFun<-", function(object, value)
                   standardGeneric("preFun<-"), where = where)
    }
    setReplaceMethod("preFun", "widget", function(object, value){
                  object@preFun <- value; object}, where = where)
    if(!isGeneric("postFun<-")){
        setGeneric("postFun<-", function(object, value)
                   standardGeneric("postFun<-"), where = where)
    }
    setReplaceMethod("postFun", "widget", function(object, value){
                  object@postFun <- value; object}, where = where)

}
