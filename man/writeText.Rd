\name{writeText}
\alias{writeText}
\alias{writeList}
\alias{getListValue}
\alias{getTextValue}
\alias{getEntryValue}

\title{Functions that read from and write to tcltk widgets}
\description{
  These functions provide some of the common read and write operations
  for tcltk widgets
}
\usage{
writeText(widget, value, clear = TRUE)
writeList(widget, value, clear = TRUE)
getListValue(which)
getTextValue(which)
getEntryValue(which)
}

\arguments{
  \item{widget}{\code{widget} a tkwin object for the tcltk widget to be
    read or written to}
  \item{value}{\code{value} the text of numerical value to be written to
    a tcltk widget}
  \item{clear}{\code{clear} a boolean to indicate whether a value will
    append to the existing one (FALSE)}
  \item{which}{\code{which} a tkwin object for the tcltk widget whose
  value will be retrieved}
}
\details{
  \code{\link{writeText}} writes to a given tcltk text box widget.

  \code{\link{writeList}} writes to a given tcltk list or entry box
  widget.

  \code{\link{getListValue}} retrieves the selected value in a tcltk
  list widget.

  \code{\link{getTextValue}} retrieves the value of a text box.

  \code{\link{getEntryValue}} retrieves the value of an entry box.
}
\value{
  \code{\link{getListValue}} returns the selected value in a tcltk
  list widget.

  \code{\link{getTextValue}} returns the value of a text box.

  \code{\link{getEntryValue}} returns the value of an entry box.
}
\references{R tcltk}
\author{Jianhua Zhang}
\note{The functions are part of the Bioconductor project at
  Dana-Farber Cancer Institute to provide Bioinformatics
  functionalities through R}

\seealso{\code{\link{basicPW-class}}, \code{\link{widget-class}}}
\examples{
\dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the widgetTools package loaded

    # Create the widgets
    base <- tktoplevel()
    list <- tklistbox(base, width = 20, height = 5)

    entry <- tkentry(base)
    text <- tktext(base, width = 20, height = 5)
    tkpack(list, entry, text)
    # Write and read from the widgets
    writeList(list, c("Option1", "Option2", "Option3"))
    writeList(entry, "An Entry box")
    writeText(text, "A text box")
    # Will be NULL if not selected
    getListValue(list)
    getTextValue(text)
    getEntryValue(entry)
# Destroy toplevel widget
#    tkdestroy(base)
}
}
\keyword{interface}
