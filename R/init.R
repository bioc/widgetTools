# This group of functions initialize all the classes needed for
# widgetTools that renders pWidgets on a widget that hosts several
# interactive tk widgets. We define a pWidget to be a tk widget such as a
# button, entry box, ....
#
# The first cuntion initilizes a pWidget class with default methods.
# parent - the window or frame ... where a given pWidget resides;
# name - a tcltk widget of class "tkwin" corresponding to the pWidget.
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
    setClass("pWidget", representation(name = "tkwin",
                                       type = "character",
                                       parent = "character",
                                       text = "character",
                                       variable = "character",
                                       width = "numeric",
                                       height = "numeric",
                                       vScroll = "logical",
                                       hScroll = "logical",
                                       preFun = "function",
                                       postFun = "function"),
                                       where = where)
    # Set the get methods
    if(!isGeneric("getName")){
        setGeneric("getName",
                   function(object) standardGeneric("getName"),
                   where = where)
    }
    setMethod("getName", "pWidget",
              function(object) object@name, where = where)
    if(!isGeneric("getType")){
        setGeneric("getType",
                   function(object) standardGeneric("getType"),
                   where = where)
    }
    setMethod("getType", "pWidget",
              function(object) object@type, where = where)
    if(!isGeneric("getParent")){
        setGeneric("getParent",
                   function(object) standardGeneric("getParent"),
                   where = where)
    }
    setMethod("getParent", "pWidget",
              function(object) object@parent, where = where)
    if(!isGeneric("getText")){
        setGeneric("getText",
                   function(object) standardGeneric("getText"),
                   where = where)
    }
    setMethod("getText", "pWidget",
              function(object) object@postFun(object@text), where = where)
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
    if(!isGeneric("getPreFun")){
        setGeneric("getPreFun",
                   function(object) standardGeneric("getPreFun"),
                   where = where)
    }
    setMethod("getPreFun", "pWidget",
              function(object) object@preFun, where = where)

    if(!isGeneric("getPostFun")){
        setGeneric("getPostFun",
                   function(object) standardGeneric("getPostFun"),
                   where = where)
    }
    setMethod("getPostFun", "pWidget",
              function(object) object@postFun, where = where)

    # Set the set functions
    if(!isGeneric("setName")){
        setGeneric("setName",
                   function(object, value) standardGeneric("setName"),
                   where = where)
    }
    setMethod("setName", "pWidget",
              function(object, value){object@name <- value; return(object)},
              where = where)
    if(!isGeneric("setType")){
        setGeneric("setType",
                   function(object, value) standardGeneric("setType"),
                   where = where)
    }
    setMethod("setType", "pWidget",
              function(object, value){object@type <- value; return(object)},
              where = where)
    if(!isGeneric("setParent")){
        setGeneric("setParent",
                   function(object, value) standardGeneric("setParent"),
                   where = where)
    }
    setMethod("setParent", "pWidget",
              function(object, value){object@parent <- value; return(object)},
              where = where)
    if(!isGeneric("setText")){
        setGeneric("setText",
                   function(object, value) standardGeneric("setText"),
                   where = where)
    }
    setMethod("setText", "pWidget",
              function(object, value){object@text <- object@preFun(value);
                                      return(object)}, where = where)
    if(!isGeneric("setVariable")){
        setGeneric("setVariable",
                   function(object, value) standardGeneric("setVariable"),
                   where = where)
    }
    setMethod("setVariable", "pWidget",
              function(object, value){object@variable <- value;
                                      return(object)}, where = where)
    if(!isGeneric("setWidth")){
        setGeneric("setWidth",
                   function(object, value) standardGeneric("setWidth"),
                   where = where)
    }
    setMethod("setWidth", "pWidget",
              function(object, value){object@width <- value; return(object)},
              where = where)
    if(!isGeneric("setHeight")){
        setGeneric("setHeight",
                   function(object, value) standardGeneric("setHeight"),
                   where = where)
    }
    setMethod("setHeight", "pWidget",
              function(object, value){object@height <- value;
                                      return(object)}, where = where)
    if(!isGeneric("setVScroll")){
        setGeneric("setVScroll",
                   function(object, value) standardGeneric("setVScroll"),
                   where = where)
    }
    setMethod("setVScroll", "pWidget",
              function(object, value){object@vScroll <- value;
                                      return(object)}, where = where)
    if(!isGeneric("setHScroll")){
        setGeneric("setHScroll",
                   function(object, value) standardGeneric("setHScroll"),
                   where = where)
    }
    setMethod("setHScroll", "pWidget",
              function(object, value){object@hScroll <- value;
                                      return(object)}, where = where)
    if(!isGeneric("setPreFun")){
        setGeneric("setPreFun",
                   function(object, value) standardGeneric("setPreFun"),
                   where = where)
    }
    setMethod("setPreFun", "pWidget",
              function(object, value){object@preFun <- value;
                                      return(object)}, where = where)
    if(!isGeneric("setPostFun")){
        setGeneric("setPostFun",
                   function(object, value) standardGeneric("setPostFun"),
                   where = where)
    }
    setMethod("setPostFun", "pWidget",
              function(object, value){object@postFun <- value;
                                      return(object)}, where = where)

    # Set the three function that interface with notifier
    if(!isGeneric("attach")){
        setGeneric("attach",
                   function(view, subject, observer)
                   standardGeneric("attach"), where = where)
    }
    setMethod("attach", "pWidget",
              function(view, subject, observer)
              notifier@register(view, subject, observer),
              where = where)
    if(!isGeneric("detach")){
        setGeneric("detach",
                   function(view, subject, observer)
                   standardGeneric("detach"), where = where)
    }
    setMethod("attach", "pWidget",
              function(view, subject, observer)
              notifier@unRegister(view, subject, observer),
              where = where)
    if(!isGeneric("update")){
        setGeneric("update",
                   function(object, value)
                   standardGeneric("update"), where = where)
    }
    setMethod("update", "pWidget",
             function(object, value) notifyView(object, value),
              where = where)

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
    if(!isGeneric("setSubNObse")){
        setGeneric("setSubNObse",
                   function(object, value) standardGeneric("setSubNObse"),
                   where = where)
    }
    setMethod("setSubNObse", "notifier",
              function(object, value){object@subNObse <- value;
                                      return(object)}, where = where)

    # Set the interface functions
    if(!isGeneric("registerView")){
        setGeneric("registerView",
                   function(view, subject, observer)
                   standardGeneric("registerView"), where = where)
    }
    setMethod("registerView", "notifier",
              function(view, subject, observer)
                  notifier@subNObse <- .writeSubNObse(notifier, view,
                                                      subject, observer),
                  where = where)
    if(!isGeneric("unRegisterView")){
        setGeneric("unRegisterView",
                   function(view, subject, observer)
                   standardGeneric("unRegisterView"), where = where)
    }
    setMethod("unRegisterView", "notifier",
              function(view, subject, observer)
                  notifier@subNObse <- .modSubNObse(notifier, view,
                                                      subject, observer),
                  where = where)



    return("Class notifier initialized")
}

# Called by register of .initNotifier to register views, subjects, and
# observers
.writeSubNObse <- function(notifier, view, subject, observer){
    if(!is.null(notifier@subNObse[[view]])){
        if(!is.null(notifier@subNObse[[view]][[subject]])){
            notifier@subNObse[[view]][[subject]] <-
                c(notifier@subNObse[[view]][[subject]], observer)
        }else{
            notifier@subNObse[[view]][[subject]]<- observer
        }
    }else{
        tempList <- list()
        tempList[[subject]] <- observer
        notifier@subNObse[[view]] <- tempList
    }
    return(notifier@subNObse)
}

# Called by unRegister of .initNotifier to unregister views, subjects, and
# observers
.modSubNObse <- function(notifier, view, subject, observer){
    removeView <- function(){
        notifier@subNObse <-
            notifier@subNObse[setdiff(names(notifier@subNObse), view)]
        return(notifier@subNObse)
    }
    # Remove the view if subject is null
    if(is.null(subject)){
        return(removeView())
    }
    if(!is.null(notifier@subNObse[[view]][[subject]])){
        temp <- setdiff(notifier@subNObse[[view]][[subject]], observer)
        # Remove the subject if empty
        if(length(temp) == 0){
            temp <- setdiff(names(notifier@subNObse[[view]]), subject)
            if(length(temp) == 0){
                # Remove the view if empty
                return(removeView())
            }else{
                notifier@subNObse[[view]] <-
                    notifier@subNObse[[view]][setdiff(
                              names(notifier@subNObse[[view]]), subject)]
            }
        }else{
            notifier@subNObse[[view]][[subject]] <- temp
            return(notifier@subNObse)
        }
    }
    return(notifier@subNObse)
}

# This function initilizes tkWidget class with default functions
# title - a character string for the text to be displayed as the title
# of the widget to be created
# name - a "tkwin" object that is the toplevel of the widget to be created
#
.initTKWidget <- function(where){
    setClass("tkWidget", representation(title = "character",
                                        name = "tkwin"),
              where = where)
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
    setMethod("doPack", "pWidget",
              function(what, side = "left")
              tkpack(getName(what), side = side), where = where)
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
           "list" = temp <- makeViewer(parent, text = getText(pWidget),
                           vWidth = getWidth(pWidget),
                           vHeight = getHeight(pWidget),
                           hScroll = getHScroll(pWidget),
                           vScroll = getVScroll(pWidget), what = "list"),
           "label" = temp <- tklabel(parent, text = getText(pWidget),
                             width = getWidth(pWidget)),
           stop("Invalid pWidget type"))
    pWidget <- setName(pWidget, temp)

    return(pWidget)
}
