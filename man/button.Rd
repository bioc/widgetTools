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
button(name, env, value = "", width = 12, height = 0, funs = list(),
notify = list(), preFun = function(x) x, postFun = function(x) x,
view = new("widgetView") )
entryBox(name, env, value = "", width = 50, height = 0, funs = list(),
notify = list(), preFun = function (x) x, postFun = function(x) x,
view = new("widgetView"))
textBox(name, env, value = "", width = 25, height = 12, funs = list(),
notify = list(), preFun = function (x) x, postFun = function(x) x,
view = new("widgetView"))
listBox(name, env, value = "", width = 25, height = 10, funs = list(),
notify = list(), preFun = function (x) x, postFun = function(x) x,
view = new("widgetView"))
checkButton(name, env, value, width = 50, funs = list(), notify =
list(), preFun = function (x) x, postFun = function(x) x,
view = new("widgetView"))
radioButton(name, env, value, width = 50, funs = list(), notify =
list(), preFun = function (x) x, postFun = function(x) x,
view = new("widgetView"))
label(name, env, value = "", width = 0, height = 0, funs = list(),
notify = list(), preFun = function (x) x, postFun = function(x) x,
view = new("widgetView"))
widget(wTitle, pWidgets, funs = list(), preFun = function()
print("Hello"), postFun = function() print("Bye"), env)
widgetView(WVTitle, name, widgetids = list(), theWidget = new("widget"),
winid)
}
\arguments{
  \item{name}{\code{name} a character string for the name to be
    associated with a given primary widget}
  \item{env}{\code{env} an R enviornment object within which the
    original values for each primary widget will be stored and updating
    and reterival of the values will take place}
  \item{value}{\code{value} the initial values to be associated with a
    given primary widget}
  \item{width}{\code{width} an integer for the width of the primary widget (if
    applicable)} 
  \item{height}{\code{height} an integer for the height of the primary
    widget (if applicable)}
  \item{funs}{\code{funs} a list of R functions that will be associated with a
    pimary widget and invoked when an operation (e.g. click, get focuse,
    ...) is applied to the primary widget}
  \item{notify}{\code{notify} a list of functions defning the actions to
    be performed when the value of the primary widget changes}
  \item{preFun}{\code{preFun} an R function that should be applied when
    the widget is activated}
  \item{postFun}{\code{postFun} an R function that will be applied when
    the widget is inactivited}
  \item{wTitle}{\code{wTitle} a character string for the title to be
    displyed when the widget is rendered}
  \item{pWidgets}{\code{pWidget} a list of primary widgets (e.g. button,
    list box, ...) to be rendered}
  \item{WVTitle}{\code{WVTitle} same as wTitle}
  \item{widgetids}{\code{widgetids} a list of tkwin ids for the primary
    widgets to be rendered}
  \item{theWidget}{\code{theWidget} a \code{\link{widget}} object to
    render the primary widgets}
  \item{view}{\code{view} an object of class widgetView}
  \item{winid}{\code{winid} an object of class winid}
}
\details{
  \code{\link{button}} constructs a button widget object.
  
  \code{\link{button}} constructs an entry box widget object.
  
  \code{\link{textBox}} constructs a text box widget object.
  
  \code{\link{listBox}} constructs a list box widget object. Value for
  alistbox object should be a named vector with names being the content 
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
\note{The functions are part of the Bioconductor project at Dana-Frber
  Cancer Institute to provide Bioinformatics funcitonalities through R}

\seealso{\code{\link{widget-class}}, \code{\link{basicPW-class}}}
\examples{
# Create an R environment to store the values of primary widgets
PWEnv <- new.env(hash = TRUE, parent = parent.frame(1))

# Create a label
label1 <- label(name = "label1", value = "File Name: ", env = PWEnv)

# Create an entry box with "Feed me using brows" as the default value
entry1 <- entryBox(name = "entry1", value = "Feed me using browse",
                   env = PWEnv)

# Create a button that will call the function browse2Entry1 when
# pressed.
browse2Entry1 <- function(){
    tempValue <- fileBrowser()
    temp <- get(name(entry1), env = PWEnv)
    value(temp) <- paste(tempValue, sep = "", collapse = ";")
    assign(name(entry1), temp, env = PWEnv)
}
button1 <- button(name = "button1", value = "Browse",
                     funs = list(command = browse2Entry1), env = PWEnv)

# Create a list box with "Option1", "Option2", and "Option3" as the
# content and "Option1" selected
list1 <- listBox(name = "list1", value = c(Option1 = TRUE, Option2 = FALSE,
                                 Option3 = FALSE), env = PWEnv)

# Create a text box with "Feed me something" displayed
text1 <- textBox(name = "text1", value = "Feed me something",
                 env = PWEnv)

# Create a set of radio buttons with "radio1" as the default
label2 <- label(name = "label2", value = "Select one:  ", env = PWEnv)
radios1 <- radioButton(name = "radios1", value = c(radio1 = TRUE,
                       radio2 = FALSE, radio3 = FALSE), env = PWEnv)

# Create a set of check boxes with "check1" selected and "check2" and
# "check3" not selected
label3 <- label(name = "label3", value = "Select one to many: ", env = PWEnv)
checks1 <- checkButton(name = "checks1", value = c(check1 = TRUE,
                       check22 = FALSE, check3 = FALSE), env = PWEnv)

# Please not that the name of the primary widget object (e.g. checks1)
# should be the same as the value of the name slot of the object
# (e. g. name = "checks1")

# Render the widgets
pWidgets <- list(topRow = list(label1 = label1, entry1 = entry1,
                 button1 = button1), textRow = list(list1 = list1,
                 text1 = text1), radGroup = list(label2 = label2,
                 radios1 = radios1), chkGroup = list(label3 = label3,
                                     checks1 = checks1))
if(interactive()){
aWidget <- widget(wTitle = "A test widget", pWidgets, funs = list(),
                 preFun = function() print("Hello"),
                 postFun = function() print("Bye"), env = PWEnv)
} 
}
\keyword{interface}
