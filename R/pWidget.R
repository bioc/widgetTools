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
pWidget <- function(type, parent, text, variable = "",
                    width = 0, height = 0, vScroll = FALSE,
                    hScroll = FALSE, preFun = function (x) x,
                    postFun = function(x) x){

    .checkArgs(type, parent, text, variable)

    new("pWidget", type = type, parent = parent,
        text = text, variable = variable, width = width,
        height = height, vScroll = vScroll, hScroll = hScroll,
        preFun = preFun, postFun = postFun)
}

.checkArgs <- function(type, parent, text, variable){
    if(any(c(is.null(parent), is.null(type)) || any(is.na(parent),
                              is.na(type), parent == "", type == ""))){
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

notifier <- function(subNObse = list()){
     new("notifier", subNObse = subNObse)
}

# This function constructs a tkWidget class with default methods
#
# title <- a character string for the title of the tkWidget to be
# created
#

tkWidget <- function(title){
    temp <- new("tkWidget", title = title)
    base <- tktoplevel()
    tktitle(base) <- title
    temp@name <- base
    return(temp)
}
