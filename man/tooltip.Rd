\name{tooltip}
\alias{tooltip}
\title{A tcltk widget to mimic a tooltip}
\description{
  Current tcltk library does not support tooltip unless an extension is
  included. The function tooltip is implemented as an alternative. 
}
\usage{
tooltip(text, targetWidget)
}
\arguments{
  \item{text}{\code{text} a character string for the content of the tooltip}
  \item{targetWidget}{\code{targetWidget} a tkwin object for the target
    tclck widget to which a tool tip will be associated}
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
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{dropdownList}}}
\examples{
if(interactive()){
    base <- tktoplevel()
    but <- tkbutton(base, text = "Move Mouse Over Me")
    tkpack(but)
    tkbind(but, "<Enter>", expression(tooltip("Move mouse off me", but)))
    tkdestroy(base)
}
}
\keyword{misc}


