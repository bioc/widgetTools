\name{oneVScrList}
\alias{oneVScrList}
\title{A function that creates a groups of list boxes sharing a single
  vertical scroll bar}
\description{
  This function creates a group of list boxes what share a common
  vertical scroll bar. Values in all the list boxes scroll up or down
  when the scroll bar is dragged
}
\usage{
oneVScrList(base, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base}{\code{base} a tkwin object that will be the container of
    the list boxes to be created}
  \item{data}{\code{data} a matrix with data to be put in the list boxes} 
}
\details{
  The matrix should have names for its columns. The names of the list
  boxes to be created will be the same as the corresponding columns of
  the matrix.

  Data in the list boxes can be sorted based on values in any of the
  list boxes.
}
\value{
  This function returns a list containing the tkwin objects of the list
  boxes created.
}
\references{tcltk}
\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{dropdownList}}, \code{\link{tooltip}}}
\examples{
    \dontrun{
        ## These cannot be run by examples() but should be OK when pasted
        ## into an interactive R session with the widgetTools package loaded
        
        testData <- matrix(c(1:50, 100:51), ncol = 2)
        colnames(testData) <- c("Column 1", "Column 2")
        base <- tktoplevel()
        tt <- oneVScrList(base, testData)

        # Destroy toplevel widget
        # tkdestroy(base)
    }
}
\keyword{interface}

