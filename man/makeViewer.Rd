\name{makeViewer}
\alias{makeViewer}
\title{Put a Scrollable List Box into a tkWidget.}
\description{
  This function associates a tk listbox with a scroll bar and then puts
  them into a given tk widget.
}
\usage{
makeViewer(target, vWidth = "", vHeight = "", hScroll = FALSE,
vScroll = TRUE, what = "list", side = "left", text = "")
}

\arguments{
  \item{target}{tk widget that can accommodate a list box.}
  \item{vWidth, vHeight}{integers giving width and height of the listbox.}
  \item{hScroll, vScroll}{logicals indicating whether a horizontal or
    vertical scroll bar should be associated with the list box.}
  \item{what}{A character string indicating the type of the viewer to be
    put on a widget. Valid types include "list" for list box, "canvas",
    and "text" for text box}
  \item{side}{A character string for the geometry management of the
    viewer on the widget. Valid values include "left", "right", "top",
    and "bottom"}
  \item{text}{A character string to be displayed}
}
\details{
  Tk list boxes (or canvas, text box) and scroll bars are separate
  widgets. This function provides a common interface to put them
  together and functionally associated. 
}
\value{
  This function does not return any value.
}
\author{Jianhua (John) Zhang}

\seealso{\code{\link[tcltk:TkWidgets]{tklistbox}} (from the \file{tcltk}
  package). 
}
\examples{
\dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the widgetTools package loaded
    
    # Create a top level window and put a list box in it
    base <- tktoplevel()
    listBox <- makeViewer(base)

    # Destroy toplevel widget
    # tkdestroy(base)
 }
}
\keyword{interface}











