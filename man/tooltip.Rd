\name{tooltip}
\alias{tooltip}
\title{A tcltk widget to mimic a tooltip}
\description{
  Current tcltk library does not support tooltip unless an extension is
  included. The function tooltip is implemented as an alternative. 
}
\usage{
tooltip(text, targetWidget, width = 350)
}
\arguments{
  \item{text}{\code{text} a character string for the content of the tooltip}
  \item{targetWidget}{\code{targetWidget} a tkwin object for the target
    tcltk widget to which a tool tip will be associated}
  \item{width}{\code{width} an integer for the width (in pixels) of the
    tooltip} 
}
\details{
  Given a target tcltk widget, a tooltip will be associated with the
  widget. The content of the tooltip will be shown when mouse moves over
  the widget and disappear when mouse moves out of the widget.
}
\value{
  This function returns invisible()
}
\references{tcltk}
\author{Jianhua Zhang}

\seealso{\code{\link{dropdownList}}}
\examples{
\dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the widgetTools package loaded
    
    base <- tktoplevel()
    but <- tkbutton(base, text = "Move Mouse Over Me")
    tkpack(but)
    tkbind(but, "<Enter>", expression(tooltip("Move mouse off me", but)))

    # Destroy toplevel widget
    # tkdestroy(base)
}
}
\keyword{misc}


