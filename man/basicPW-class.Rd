\name{basicPW-class}
\docType{class}
\alias{basicPW-class}
\alias{wHeight<-}
\alias{wHeight}
\alias{wNotify<-}
\alias{wNotify}
\alias{wType<-}
\alias{wType}
\alias{wValue<-}
\alias{wValue}
\alias{wPostFun}
\alias{wPostFun<-}
\alias{wPreFun}
\alias{wPreFun<-}
\alias{wView<-}
\alias{wView}
\alias{wWidth<-}
\alias{wWidth}
\alias{wEnv}
\alias{wEnv<-}
\alias{wFuns}
\alias{wName}
\alias{wPreFun}
\alias{wPostFun<-}
\alias{wFuns<-}
\alias{wName<-}
\alias{wHeight<-,basicPW-method}
\alias{wHeight,basicPW-method}
\alias{wNotify<-,basicPW-method}
\alias{wNotify,basicPW-method}
\alias{wType<-,basicPW-method}
\alias{wType,basicPW-method}
\alias{wValue<-,basicPW-method}
\alias{wValue,basicPW-method}
\alias{wPostFun,basicPW-method}
\alias{wPreFun<-,basicPW-method}
\alias{wView<-,basicPW-method}
\alias{wView,basicPW-method}
\alias{wWidth<-,basicPW-method}
\alias{wWidth,basicPW-method}
\alias{wEnv,basicPW-method}
\alias{wFuns,basicPW-method}
\alias{wName,basicPW-method}
\alias{wPreFun,basicPW-method}
\alias{wPostFun<-,basicPW-method}
\alias{wFuns<-,basicPW-method}
\alias{wName<-,basicPW-method}
\alias{wEnv<-,basicPW-method}
\title{Class "basicPW", a basic class for primary widgets}
\description{This class defines the behavior shared by primary widget
  object used to build a GUI type interface}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("basicPW", ...)}.
    Constructors have been defined to create objects of this class for
    specific widgets such as buttons, list boxes, .. 
}
\section{Slots}{
  \describe{
    \item{\code{wName}:}{Object of class \code{"character"} - a string
      for the name of the object}
    \item{\code{wType}:}{Object of class \code{"character"} - a string
      defining the type of the primary widget. (e.g. button)}
    \item{\code{wValue}:}{Object of class \code{"ANY"} - the initial
      value to be associated with the object}
    \item{\code{wWidth}:}{Object of class \code{"numeric"} - an integer
      for the width of the object to be rendered (if applicable)}
    \item{\code{wHeight}:}{Object of class \code{"numeric"} - an integer
      for the height of the object to be rendered (if applicable)}
    \item{\code{wFuns}:}{Object of class \code{"list"} - a list of R
      functions to be executed befer the widget is activated}
    \item{\code{wPreFun}:}{Object of class \code{"function"} - a list of
      functions to be executed before the value of the widget to be updated}
    \item{\code{wPostFun}:}{Object of class \code{"function"} - a list of
      functions to be executed before the value of the widget to be retrieved}
    \item{\code{wNotify}:}{Object of class \code{"list"} - a list of
      functions to be executed each time when the value of the widget changes}
    \item{\code{wEnv}:}{Object of class \code{"environment"} - an R
      environment object within which the value of the object is stored}
    \item{\code{wView}:}{Object of class \code{"widgetView"} - a object
      of the class widgetView to which the widget is rendered}
  }
}

\section{Methods}{
  \describe{
    \item{wEnv<-}{\code{signature(object = "basicPW")}: Set the value for
      wEnv slot}
    \item{wEnv}{\code{signature(object = "basicPW")}: Get the value for
      wEnv slot }
    \item{wFuns<-}{\code{signature(object = "basicPW")}: Set the value
      for wFuns slot}
    \item{wRuns}{\code{signature(object = "basicPW")}: Get the value for
      wFuns slot}
    \item{wHeight<-}{\code{signature(object = "basicPW")}: Set the value
      for wHeight slot}
    \item{wHeight}{\code{signature(object = "basicPW")}: Get the value
      for wHeight slot}
    \item{wName<-}{\code{signature(object = "basicPW")}: Set the value
      for wName slot}
    \item{wName}{\code{signature(object = "basicPW")}: Get the value for
      wName slot}
    \item{wNotify<-}{\code{signature(object = "basicPW")}: Set the value
      for wNotify slot}
    \item{wNotify}{\code{signature(object = "basicPW")}: Get the value
      for wNotify slot}
    \item{wPostFun<-}{\code{signature(object = "basicPW")}: Set the value
      for wPostFun slot} 
    \item{wPostFun}{\code{signature(object = "basicPW")}: Get the vlaue
      for wPostFun slot}
    \item{wPreFun<-}{\code{signature(object = "basicPW")}: Set the value
      for wPreFun slot}
    \item{wPreFun}{\code{signature(object = "basicPW")}: Get the vlaue
      for wPreFun slot}
    \item{wType<-}{\code{signature(object = "basicPW")}: Set the value
      for wType slot}
    \item{wType}{\code{signature(object = "basicPW")}: Get the value for
      wType slot}
    \item{wValue<-}{\code{signature(object = "basicPW")}: Set the value
      for wValue slot}
    \item{wValue}{\code{signature(object = "basicPW")}: Get the value for
      wValue slot} 
    \item{wView<-}{\code{signature(object = "basicPW")}: Set the value
      for wView slot }
    \item{view}{\code{signature(object = "basicPW")}: Get the value for
      wView slot }
    \item{wWidth<-}{\code{signature(object = "basicPW")}: Set the value
      for wWidth slot }
    \item{wWidth}{\code{signature(object = "basicPW")}: Get the value for
      wWidth slot}
  }
}
\references{Programming with data }
\author{Jianhua Zhang}
\note{This class is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics funcitonalities through R}

\seealso{\code{\link{widgetView-class}},\code{\link{widget-class}} }

\examples{
# Create an R environment to store the values of primary widgets
PWEnv <- new.env(hash = TRUE, parent = parent.frame(1))

# Create a label
label1 <- label(wName = "label1", wValue = "File Name: ", wEnv = PWEnv)

# Create an entry box with "Feed me using brows" as the default value
entry1 <- entryBox(wName = "entry1", wValue = "Feed me using browse",
                   wEnv = PWEnv)
}
\keyword{classes}
