\name{button}
\alias{button}
\alias{entryBox}
\alias{textBox}
\alias{listBox}
\alias{checkButton}
\alias{radioButton}
\alias{label}
\alias{widget}
\alias{widgetView}
\title{Functions to construct objects of primary widgets and render them}
\description{
  All the primary widgets such as button, text box, and so on are
  objects of basicPW class. The functions are constructors of primary
  widgets that are subjects of basicPW class with behaviors specific to
  primary widgets.  
}
\usage{
button(wName, wEnv, wValue = "", wWidth = 12, wHeight = 0, wFuns = list(),
wNotify = list(), wPreFun = function(x) x, wPostFun = function(x) x,
wView = new("widgetView") )
entryBox(wName, wEnv, wValue = "", wWidth = 50, wHeight = 0, wFuns = list(),
wNotify = list(), wPreFun = function (x) x, wPostFun = function(x) x,
wView = new("widgetView"))
textBox(wName, wEnv, wValue = "", wWidth = 25, wHeight = 12, wFuns = list(),
wNotify = list(), wPreFun = function (x) x, wPostFun = function(x) x,
wView = new("widgetView"))
listBox(wName, wEnv, wValue = "", wWidth = 25, wHeight = 10, wFuns = list(),
wNotify = list(), wPreFun = function (x) x, wPostFun = function(x) x,
wView = new("widgetView"))
checkButton(wName, wEnv, wValue, wWidth = 50, wFuns = list(), wNotify =
list(), wPreFun = function (x) x, wPostFun = function(x) x,
wView = new("widgetView"))
radioButton(wName, wEnv, wValue, wWidth = 50, wFuns = list(), wNotify =
list(), wPreFun = function (x) x, wPostFun = function(x) x,
wView = new("widgetView"))
label(wName, wEnv, wValue = "", wWidth = 0, wHeight = 0, wFuns = list(),
wNotify = list(), wPreFun = function (x) x, wPostFun = function(x) x,
wView = new("widgetView"))
widget(wTitle, pWidgets, funs = list(), preFun = function()
print("Hello"), postFun = function() print("Bye"), env, defaultNames =c(
"Finish", "Cancel"))
widgetView(WVTitle, vName, widgetids = list(), theWidget = new("widget"),
winid)
}
\arguments{
  \item{wName}{\code{wName} a character string for the name to be
    associated with a given primary widget}
  \item{vName}{\code{vName} same as wName but for a widget object}
  \item{wEnv}{\code{wEnv} an R environment object within which the
    original values for each primary widget will be stored and updating
    and retrieval of the values will take place}
  \item{env}{\code{env} same as wEnv but for a widget object}
  \item{wValue}{\code{wValue} the initial values to be associated with a
    given primary widget}
  \item{wWidth}{\code{wWidth} an integer for the width of the primary
    widget (if applicable)} 
  \item{wHeight}{\code{wHeight} an integer for the height of the primary
    widget (if applicable)}
  \item{wFuns}{\code{wFuns} a list of R functions that will be
    associated with a primary widget and invoked when an operation
    (e.g. click, get focus, ...) is applied to the primary widget}
  \item{funs}{\code{funs} same as wFuns but for a widget object}
  \item{wNotify}{\code{wNotify} a list of functions defining the actions to
    be performed when the value of the primary widget changes}
  \item{wPreFun}{\code{wPreFun} an R function that should be applied when
    the widget is activated}
  \item{preFun}{\code{preFun} same as wPreFun but for a view}
  \item{wPostFun}{\code{wPostFun} an R function that will be applied when
    the widget is inactivated}
  \item{postFun}{\code{postFun} same as wPostFun but for a view}
  \item{wTitle}{\code{wTitle} a character string for the title to be
    displayed when the widget is rendered}
  \item{pWidgets}{\code{pWidget} a list of primary widgets (e.g. button,
    list box, ...) to be rendered}
  \item{WVTitle}{\code{WVTitle} same as wTitle}
  \item{widgetids}{\code{widgetids} a list of tkwin ids for the primary
    widgets to be rendered}
  \item{theWidget}{\code{theWidget} a \code{\link{widget}} object to
    render the primary widgets}
  \item{wView}{\code{wView} an object of class widgetView}
  \item{winid}{\code{winid} an object of class winid}
  \item{defaultNames}{\code{defaultName} a vector of character string of
    length two for the text to be shown on the two default buttons. The
    first is to end the process and the second to abort the process}
}
\details{
  \code{\link{button}} constructs a button widget object.
  
  \code{\link{button}} constructs an entry box widget object.
  
  \code{\link{textBox}} constructs a text box widget object.
  
  \code{\link{listBox}} constructs a list box widget object. Value for
  a listbox object should be a named vector with names being the content 
  to be shown in the list box and values being TRUE (default value) or
  FALSE.

  \code{\link{checkButton}} constructs a group of check box widget
  objects. Value for check button objects should be a named vector
  with names being the content to be shown in the list box and values
  being TRUE (checked) or FALSE (not checked). 

  \code{\link{radioButton}} constructs a group of radio button widget
  objects. Value for radio button objects should be a named vector
  with names being the content to be shown in the list box and values
  being TRUE (default) or FALSE.

  \code{\link{label}} constructs a text label widget object with the
  value displayed as the text.

  \code{\link{widget}} constructs a widget object to render the primary
  widgets.

  \code{\link{widgetView}} constructs a widgetView object. This class is
  for internal use by class \code{\link{widget-class}}. Users trying to
  create GUI type widget do not need to use this class.
}
\value{
  Each constructor returns a tkwin object for the primary widget object.
}
\references{R tcltk}
\author{Jianhua Zhang}
\note{The functions are part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{widget-class}}, \code{\link{basicPW-class}}}
\examples{
# Create an R environment to store the values of primary widgets
PWEnv <- new.env(hash = TRUE, parent = parent.frame(1))

# Create a label
label1 <- label(wName = "label1", wValue = "File Name: ", wEnv = PWEnv)

# Create an entry box with "Feed me using brows" as the default value
entry1 <- entryBox(wName = "entry1", wValue = "Feed me using browse",
                   wEnv = PWEnv)

# Create a button that will call the function browse2Entry1 when
# pressed.
browse2Entry1 <- function(){
    tempValue <- tclvalue(tkgetOpenFile())
    temp <- get(wName(entry1), env = PWEnv)
    wValue(temp) <- paste(tempValue, sep = "", collapse = ";")
    assign(wName(entry1), temp, env = PWEnv)
}
button1 <- button(wName = "button1", wValue = "Browse",
                     wFuns = list(command = browse2Entry1), wEnv = PWEnv)

# Create a list box with "Option1", "Option2", and "Option3" as the
# content and "Option1" selected
list1 <- listBox(wName = "list1", wValue = c(Option1 = TRUE, Option2 = FALSE,
                                 Option3 = FALSE), wEnv = PWEnv)

# Create a text box with "Feed me something" displayed
text1 <- textBox(wName = "text1", wValue = "Feed me something",
                 wEnv = PWEnv)

# Create a set of radio buttons with "radio1" as the default
label2 <- label(wName = "label2", wValue = "Select one:  ", wEnv = PWEnv)
radios1 <- radioButton(wName = "radios1", wValue = c(radio1 = TRUE,
                       radio2 = FALSE, radio3 = FALSE), wEnv = PWEnv)

# Create a set of check boxes with "check1" selected and "check2" and
# "check3" not selected
label3 <- label(wName = "label3", wValue = "Select one to many: ",
wEnv = PWEnv)
checks1 <- checkButton(wName = "checks1", wValue = c(check1 = TRUE,
                       check22 = FALSE, check3 = FALSE), wEnv = PWEnv)

# Please not that the name of the primary widget object (e.g. checks1)
# should be the same as the value of the name slot of the object
# (e. g. name = "checks1")

# Render the widgets
pWidgets <- list(topRow = list(label1 = label1, entry1 = entry1,
                 button1 = button1), textRow = list(list1 = list1,
                 text1 = text1), radGroup = list(label2 = label2,
                 radios1 = radios1), chkGroup = list(label3 = label3,
                                     checks1 = checks1))
\dontrun{
## These cannot be run by examples() but should be OK when pasted
## into an interactive R session with the widgetTools package loaded

aWidget <- widget(wTitle = "A test widget", pWidgets, funs = list(),
                 preFun = function() print("Hello"),
                 postFun = function() print("Bye"), env = PWEnv)
} 
}
\keyword{interface}
