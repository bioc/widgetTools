\name{basicPW-class}
\docType{class}
\alias{basicPW-class}
\title{Class "basicPW" is for primary widget objects}
\description{A primary widget is defined as a single or set of tcltk
  widgets such as list, button, or a set of radio buttons or check buttons}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("basicPW",
    ...)}. A name and type is required for creating an object
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{\code{"character"}a vector of character
      string(s) for the names of the tk widget elements corresponding to
      the pWidgets to be created. The name is required and assumed to be
      unique and will be used as the identifier for the pWidget. The
      length of name should be one except for select boxes and radio
      buttons where more than one names can be given}  
    \item{\code{type}:}{\code{"character"} a character string for the
      type (e. g. button, list, ...) of the pWidget. This slot will be
      populated automatically by the system}
    \item{\code{value}:}{\code{"ANY"} an optional character string for
      the value associated with the pWidget. If the pWidget is an entry
      box, a list box, or a text box, the value will also be displayed
      inside the tk widget element corresponding to the pWidget}
    \item{\code{width}:}{\code{"numeric"} an optional integer for the
      width of the tk widget element corresponding to the pWidget to be
      created} 
    \item{\code{height}:}{\code{"numeric"} an optional integer for the
      height of the tk widget element corresponding to the pWidget to be
      created}
    \item{\code{funs}:}{\code{"list"} an optional list of functions that
      are to be executed when a given action is performed on the tk
      widget element corresponding to the pWidget. The name for the
      element in the list defines the type of action. Currently, only
      sClick(a single click), dClick(a double click), and kPress(a key
      strick) are the valid action types} 
    \item{\code{preFun}:}{\code{"function"} an optional function that is
      to be executed to format the string that will be used to set the
      value of the pWidget gets updated} 
    \item{\code{postFun}:}{\code{"function"}an optional function that is
      to be executed to format the string stored as the value of the
      pWidget when it is reterived by any operation }
    \item{\code{notify}:}{\code{"list"} an optional list of functions
      that will be executed each time the value of the pWidget gets updated}
    \item{\code{env}:}{\code{"environment"} a required R environment
      where objects of this class work within}
    \item{\code{view}:}{\code{"widgetView"} a required widgetView object
      that holds the widget} 
  }
}

\section{Methods}{
  \describe{
    \item{env<-}{\code{signature(object = "basicPW")}: assign values to env }
    \item{env}{\code{signature(object = "basicPW")}: get the value of env}
    \item{funs<-}{\code{signature(object = "basicPW")}: assign values to
      funs}
    \item{funs}{\code{signature(object = "basicPW")}: get the value of funs}
    \item{height<-}{\code{signature(object = "basicPW")}: assign values
      to height}
    \item{height}{\code{signature(object = "basicPW")}: get the value of
      height}
    \item{name<-}{\code{signature(object = "basicPW")}: assign values to
      name}
    \item{name}{\code{signature(object = "basicPW")}: get the value of name}
    \item{notify<-}{\code{signature(object = "basicPW")}: assign values
      to notify}
    \item{notify}{\code{signature(object = "basicPW")}: get the value of
      notify}
    \item{postFun<-}{\code{signature(object = "basicPW")}: assign values
      to postFun}
    \item{postFun}{\code{signature(object = "basicPW")}: get the value
      of postFun}
    \item{preFun<-}{\code{signature(object = "basicPW")}: assign values
      to preFun}
    \item{preFun}{\code{signature(object = "basicPW")}: get the value of
      preFun}
    \item{type<-}{\code{signature(object = "basicPW")}: assign values to
      type}
    \item{type}{\code{signature(object = "basicPW")}: get the value of type}
    \item{value<-}{\code{signature(object = "basicPW")}: assign values
      to value}
    \item{value}{\code{signature(object = "basicPW")}: get the value of value}
    \item{view<-}{\code{signature(object = "basicPW")}: assign values to
      view}
    \item{view}{\code{signature(object = "basicPW")}: get the value of view}
    \item{width<-}{\code{signature(object = "basicPW")}: assign values
      to width}
    \item{width}{\code{signature(object = "basicPW")}: get the value of width}
  }
}
\author{Jianhua Zhang}
\note{This class is par of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinfomatics functionalities through R}

\seealso{\code{\link{widgetView-class}}, \code{\link{widget}}}

\examples{
new("basicPW", name = "aaa", type = "text", env = new.env(TRUE, NULL))
}
\keyword{classes}
