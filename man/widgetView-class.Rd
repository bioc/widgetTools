\name{widgetView-class}
\docType{class}
\alias{widgetView-class}
\title{Class "widgetView", a class for a GUI type widget holding widget
  elements}
\description{"widgetView" renders element widgets}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("widgetView",
    ...)}. This class is for internal use by class
  \code{\link{widget-class}}. Users trying to create GUI type widget do
  not need to use this class.
  
}
\section{Slots}{
  \describe{
    \item{\code{WVTitle}:}{Object of class \code{"character"} - a
      character string that will be desplayed as the title of the widget
      to be created}
    \item{\code{name}:}{Object of class \code{"character"} - a character
      string for the name of the widget}
    \item{\code{winid}:}{Object of class \code{"tkwin"} - a tkwin object
      for the id of the top window for the widget}
    \item{\code{widgetids}:}{Object of class \code{"list"} - a list of
      tkwin ids for element widgets}
    \item{\code{theWidget}:}{Object of class \code{"widget"} - a widget
      object that creates the widgetView}
  }
}

\section{Methods}{
  \describe{
    \item{killWin}{\code{signature(tkWidget = "widgetView")}: destroies
      the window representing the widgetView}
    \item{name<-}{\code{signature(object = "widgetView")}: set the value
      for name}
    \item{name}{\code{signature(object = "widgetView")}: get the value
      for name}
    \item{renderWidgets}{\code{signature(widgetView = "widgetView",
	pWidgets = "list")}: takes a list of "basicPW" objects
      (pWidgets) and  renders them accordingly} 
    \item{renewView}{\code{signature(widgetView = "widgetView", pWidgets
	= "list")}: using values contained by the "basicPW" objects of
      pWidgets to update the values of widget elements displayed}
    \item{theWidget<-}{\code{signature(object = "widgetView")}: set the
      value for theWidget}
    \item{theWidget}{\code{signature(object = "widgetView")}: get the
      value for theWidget}
    \item{updateDisplay}{\code{signature(widgetView = "widgetView")}:
      update the value of list box or text box element widgets}
    \item{widgetids<-}{\code{signature(object = "widgetView")}: set the
      value of widgetids}
    \item{widgetids}{\code{signature(object = "widgetView")}: get the
      value of widgetids}
    \item{winid<-}{\code{signature(object = "widgetView")}: set the
      value of winid}
    \item{winid}{\code{signature(object = "widgetView")}: set the value
      of winid}
    \item{winWait}{\code{signature(tkWidget = "widgetView")}: make
      widgetView modal}
    \item{WVTitle}{\code{signature(object = "widgetView")}: get the
      value for WVTitle}
  }
}
\references{Programming with data}
\author{Jianhua Zhang}
\note{This class is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{basicPW-class}},\code{\link{widget-class}}}

\examples{
if(interactive()){
	widgetView <- widgetView(WVTitle = "demo", name = "widget1")
}
}
\keyword{classes}
