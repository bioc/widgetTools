\name{dropdownList}
\alias{dropdownList}
\alias{getListOption}
\title{A widget to mimic a dropdown list}
\description{
  The current tcltk library does not support dropdown lists unless an
  extension is included. The function dropdownList provide an alternative.
}
\usage{
dropdownList(base, options, textvariable, width = 10, default, editable
= FALSE)
getListOption(targetWidget, options)
}
\arguments{
  \item{base}{\code{base} a tkwin object that is the parent frame of the
    dropdown list to be created}
  \item{options}{\code{options} a vector of character strings for the
    content of the dropdown list}
  \item{textvariable}{\code{textvariable} a tclVar object to be
    associated with the selected item of the dropdown list}
  \item{width}{\code{width} an integer for the width in number of
    characters of the selection containing part of the dropdown list}
  \item{default}{\code{default} a character string for the default
    selection that is going to be shown in the selection containing
    window of the dropdown list}
  \item{targetWidget}{\code{targetWidget} a tkwin object for an entry
    box to which a button will be associated to make the look of a
    dropdown list}
  \item{editable}{\code{editable} a boolean indicating whether the
    dropdown list will be editable or not}
}
\details{
  \code{base} can be a top window or a frame.

  The widget returns a frame that contains a dropdown list. The frame
  need to be placed using any of the layout methods of tcltk. The value
  of the selection will be accessed through the tclVar object passed to
  the function.

  getListOptions is called by dropdown list to get the selected item
}
\value{
  dropdownList returns a tkwin object for the frame that contains a
  dropdown list

  getListOptions returns a character string for the selected item
}
\references{tcltk}
\author{Jianhua Zhang}
\note{The functions are part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{tooltip}}}
\examples{
\dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the widgetTools package loaded
    
    base <- tktoplevel()
    selection <- tclVar()
    dropdownList(base, c("Option 1", "Option 2", "Option 3"),
          selection, 15, "Option 2")
    tclvalue(selection)
    # Destroy toplevel widget
    # tkdestroy(base)
}
}
\keyword{misc}

