\name{widget-class}
\docType{class}
\alias{widget-class}
\alias{wEnv<-}
\alias{wEnv}
\alias{wFuns<-}
\alias{wFuns}
\alias{wPostFuns<-}
\alias{wPostFun}
\alias{wPreFuns<-}
\alias{wPreFun}
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
    \item{\code{wEnv}:}{Object of class \code{"environment"} - an R
      environment for the object to work within}
    \item{\code{wFuns}:}{Object of class \code{"list"} - a list of
      functions that will be associated with buttons on the widget to be
      rendered. The name of the function in the list will be the text
      appears on the button and the function will be executed when the
      button is pressed} 
    \item{\code{wPreFun}:}{Object of class \code{"function"} - a function
      that will be executed before the widget is constructed}
    \item{\code{wPostFun}:}{Object of class \code{"function"} - a function
      that will be executed before the widget is destroyed}
  }
}

\section{Methods}{
  \describe{
    \item{wEnv<-}{\code{signature(object = "widget")}: set the value for wEnv}
    \item{wEnv}{\code{signature(object = "widget")}: get the value for wEnv}
    \item{wFuns<-}{\code{signature(object = "widget")}: set the value for
      wFuns}
    \item{wFuns}{\code{signature(object = "widget")}: get the value for wFuns}
    \item{wPostFuns<-}{\code{signature(object = "widget")}: set the value
      for wPostFuns}
    \item{wPostFun}{\code{signature(object = "widget")}: get the value
      for wPostFuns}
    \item{wPreFuns<-}{\code{signature(object = "widget")}: set the value
      for wPreFun}
    \item{wPreFun}{\code{signature(object = "widget")}: get the value for
      wPreFun}
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

label1 <- label(wName = "label1", wValue = "File Name: ", wEnv = PWEnv)
entry1 <- entryBox(wName = "entry1", wValue = "Feed me using browse",
                   wEnv = PWEnv)
browse2Entry1 <- function(){
    tempValue <- fileBrowser()
    temp <- get(wName(entry1), wEnv = PWEnv)
    wValue(temp) <- paste(tempValue, sep = "", collapse = ";")
    assign(wName(entry1), temp, env = PWEnv)
}
button1 <- button(wName = "button1", wValue = "Browse",
                     wFuns = list(command = browse2Entry1), wEnv = PWEnv)
list1 <- listBox(wName = "list1", wValue = c(Option1 = TRUE, Option2 = FALSE,
                                 Option3 = FALSE), wEnv = PWEnv)
text1 <- textBox(wName = "text1", wValue = "Feed me something",
                 wEnv = PWEnv)
label2 <- label(wName = "label2", wValue = "Select one:  ", wEnv = PWEnv)
radios1 <- radioButton(wName = "radios1", wValue = c(radio1 = TRUE,
                       radio2 = FALSE, radio3 = FALSE), wEnv = PWEnv)
label3 <- label(wName = "label3", wValue = "Select one to many: ",
wEnv = PWEnv)
checks1 <- checkButton(wName = "checks1", wValue = c(check1 = TRUE,
                       check22 = FALSE, check3 = FALSE), wEnv = PWEnv)
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
