\name{basicPW-class}
\docType{class}
\alias{basicPW-class}
\alias{height<-}
\alias{height}
\alias{notify<-}
\alias{notify}
\alias{type<-}
\alias{type}
\alias{value<-}
\alias{value}
\alias{postFun<-}
\alias{preFun<-}
\alias{view<-}
\alias{view}
\alias{width<-}
\alias{width}
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
    \item{\code{name}:}{Object of class \code{"character"} - a string
      for the name of the object}
    \item{\code{type}:}{Object of class \code{"character"} - a string
      defining the type of the primary widget. (e.g. button)}
    \item{\code{value}:}{Object of class \code{"ANY"} - the initial
      value to be associated with the object}
    \item{\code{width}:}{Object of class \code{"numeric"} - an integer
      for the width of the object to be rendered (if applicable)}
    \item{\code{height}:}{Object of class \code{"numeric"} - an integer
      for the height of the object to be rendered (if applicable)}
    \item{\code{funs}:}{Object of class \code{"list"} - a list of R
      functions to be executed befer the widget is activated}
    \item{\code{preFun}:}{Object of class \code{"function"} - a list of
      functions to be executed before the value of the widget to be updated}
    \item{\code{postFun}:}{Object of class \code{"function"} - a list of
      functions to be executed before the value of the widget to be retrieved}
    \item{\code{notify}:}{Object of class \code{"list"} - a list of
      functions to be executed each time when the value of the widget changes}
    \item{\code{env}:}{Object of class \code{"environment"} - an R
      environment object within which the value of the object is stored}
    \item{\code{view}:}{Object of class \code{"widgetView"} - a object
      of the class widgetView to which the widget is rendered}
  }
}

\section{Methods}{
  \describe{
    \item{env<-}{\code{signature(object = "basicPW")}: Set the value for
      env slot}
    \item{env}{\code{signature(object = "basicPW")}: Get the value for
      env slot }
    \item{funs<-}{\code{signature(object = "basicPW")}: Set the value
      for funs slot}
    \item{funs}{\code{signature(object = "basicPW")}: Get the value for
      funs slot}
    \item{height<-}{\code{signature(object = "basicPW")}: Set the value
      for height slot}
    \item{height}{\code{signature(object = "basicPW")}: Get the value
      for height slot}
    \item{name<-}{\code{signature(object = "basicPW")}: Set the value
      for name slot}
    \item{name}{\code{signature(object = "basicPW")}: Get the value for
      name slot}
    \item{notify<-}{\code{signature(object = "basicPW")}: Set the value
      for notify slot}
    \item{notify}{\code{signature(object = "basicPW")}: Get the value
      for notify slot}
    \item{postFun<-}{\code{signature(object = "basicPW")}: Set the value
      for postFun slot} 
    \item{postFun}{\code{signature(object = "basicPW")}: Get the vlaue
      for postFun slot}
    \item{preFun<-}{\code{signature(object = "basicPW")}: Set the value
      for preFun slot}
    \item{preFun}{\code{signature(object = "basicPW")}: Get the vlaue
      for preFun slot}
    \item{type<-}{\code{signature(object = "basicPW")}: Set the value
      for type slot}
    \item{type}{\code{signature(object = "basicPW")}: Get the value for
      type slot}
    \item{value<-}{\code{signature(object = "basicPW")}: Set the value
      for value slot}
    \item{value}{\code{signature(object = "basicPW")}: Get the value for
      value slot} 
    \item{view<-}{\code{signature(object = "basicPW")}: Set the value
      for view slot }
    \item{view}{\code{signature(object = "basicPW")}: Get the value for
      view slot }
    \item{width<-}{\code{signature(object = "basicPW")}: Set the value
      for width slot }
    \item{width}{\code{signature(object = "basicPW")}: Get the value for
      width slot}
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
label1 <- label(name = "label1", value = "File Name: ", env = PWEnv)

# Create an entry box with "Feed me using brows" as the default value
entry1 <- entryBox(name = "entry1", value = "Feed me using browse",
                   env = PWEnv)
}
\keyword{classes}
