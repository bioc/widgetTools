\name{safeFileOpen}
\alias{safeFileOpen}
\title{A function that checks to see if a connection can be made to a
  given file}
\description{
  This function checks to see if a given file name exists. If so, the
  function returns a connection to the file. Otherwise, it returns
  "fileName doest exist".
}
\usage{
safeFileOpen(fileName)
}

\arguments{
  \item{fileName}{\code{fileName} a character string for the name of a
    file to which a connection is to be oppened}
}
\details{
  When this function is used, users have to make sure to check to see if
  the returnd object inherits object "connection". Otherwise, the file
  doest not exist or a connection has not be made. 
}
\value{
  The function returns a connection object that inherits class
  "connection" if the file exists and is opend. Otherwise, the string
  "fileName doest not exist"
}
\author{Jeff Gentry}
\note{This function is no placed here to be used by various
  widgets. May be mored to a more suitable place later}

\seealso{\code{\link{file}}}
\examples{
write("A test file", "testFile4safeFileOpen")
tt <- safeFileOpen("testFile4safeFileOpen")
inherits(tt, "connection")
unlink("testFile4safeFileOpen")
tt <- safeFileOpen("testFile4safeFileOpen")
inherits(tt, "connection")
}
\keyword{file}
