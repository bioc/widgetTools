\name{widget-class}
\docType{class}
\alias{widget-class}
\alias{env<-}
\alias{env}
\alias{funs<-}
\alias{funs}
\alias{postFuns<-}
\alias{postFun}
\alias{preFuns<-}
\alias{preFun}
\alias{pWidgets<-}
\alias{pWidgets}
\alias{updateCheck}
\alias{updateList}
\alias{updateRadio}
\alias{updateText}
\alias{wTitle<-}
\alias{wTitle}
\title{Class "widget" creates a widget with primary widgets contained in
the list pWidgets rendered}
\description{This class takes a list of primary widgets and then creates a
  "widgetView" object that renders the primary widgets}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("widget", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{wTitle}:}{Object of class \code{"character"} - a
      character string for the title of the widget to be created}
    \item{\code{pWidgets}:}{Object of class \code{"list"} - a list of
      "basicPW" objects representing widget elements to be rendered}
    \item{\code{env}:}{Object of class \code{"environment"} - an R
      environment for the object to work within}
    \item{\code{funs}:}{Object of class \code{"list"} - a list of
      functions that will be associated with buttons on the widget to be
      rendered. The name of the function in the list will be the text
      appears on the button and the function will be executed when the
      button is pressed} 
    \item{\code{preFun}:}{Object of class \code{"function"} - a function
      that will be executed before the widget is constructed}
    \item{\code{postFun}:}{Object of class \code{"function"} - a function
      that will be executed before the widget is destroyed}
  }
}

\section{Methods}{
  \describe{
    \item{env<-}{\code{signature(object = "widget")}: set the value for env}
    \item{env}{\code{signature(object = "widget")}: get the value for env}
    \item{funs<-}{\code{signature(object = "widget")}: set the value for
      funs}
    \item{funs}{\code{signature(object = "widget")}: get the value for funs}
    \item{postFuns<-}{\code{signature(object = "widget")}: set the value
      for postFuns}
    \item{postFun}{\code{signature(object = "widget")}: get the value
      for postFuns}
    \item{preFuns<-}{\code{signature(object = "widget")}: set the value
      for preFun}
    \item{preFun}{\code{signature(object = "widget")}: get the value for
      preFun}
    \item{pWidgets<-}{\code{signature(object = "widget")}: set the value
      for pWidgets}
    \item{pWidgets}{\code{signature(object = "widget")}: get the value
      for pWidgets}
    \item{updateCheck}{\code{signature(object = "widget")}: update the
      value of check buttons of the widget to be rendered}
    \item{updateList}{\code{signature(object = "widget")}: update the
      value of list box/entry of the widget to be rendered}
    \item{updateRadio}{\code{signature(object = "widget")}: update the
      value of radio buttons of the widget to be rendered}
    \item{updateText}{\code{signature(object = "widget")}: update the
      value of text box of the widget to be rendered}
    \item{wTitle<-}{\code{signature(object = "widget")}: set the value
      of wTitle}
    \item{wTitle}{\code{signature(object = "widget")}: get the value of
      wTitle} 
  }
}
\references{Programming with data}
\author{Jianhua Zhang}
\note{This class is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{basicPW-class}}, \code{\link{widgetView-class}} }

\examples{
PWEnv <- new.env(hash = TRUE, parent = parent.frame(1))

label1 <- label(name = "label1", value = "File Name: ", env = PWEnv)
entry1 <- entryBox(name = "entry1", value = "Feed me using browse",
                   env = PWEnv)
browse2Entry1 <- function(){
    tempValue <- fileBrowser()
    temp <- get(name(entry1), env = PWEnv)
    value(temp) <- paste(tempValue, sep = "", collapse = ";")
    assign(name(entry1), temp, env = PWEnv)
}
button1 <- button(name = "button1", value = "Browse",
                     funs = list(command = browse2Entry1), env = PWEnv)
list1 <- listBox(name = "list1", value = c(Option1 = TRUE, Option2 = FALSE,
                                 Option3 = FALSE), env = PWEnv)
text1 <- textBox(name = "text1", value = "Feed me something",
                 env = PWEnv)
label2 <- label(name = "label2", value = "Select one:  ", env = PWEnv)
radios1 <- radioButton(name = "radios1", value = c(radio1 = TRUE,
                       radio2 = FALSE, radio3 = FALSE), env = PWEnv)
label3 <- label(name = "label3", value = "Select one to many: ", env = PWEnv)
checks1 <- checkButton(name = "checks1", value = c(check1 = TRUE,
                       check22 = FALSE, check3 = FALSE), env = PWEnv)
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
\keyword{classes}
