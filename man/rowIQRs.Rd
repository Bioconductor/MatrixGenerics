% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rowIQRs.R
\name{rowIQRs}
\alias{rowIQRs}
\alias{rowIQRs,matrix_OR_array_OR_table_OR_numeric-method}
\alias{rowIQRs,ANY-method}
\alias{colIQRs}
\alias{colIQRs,matrix_OR_array_OR_table_OR_numeric-method}
\alias{colIQRs,ANY-method}
\title{Calculates the interquartile range for each row (column) of a matrix-like
object}
\usage{
rowIQRs(x, rows = NULL, cols = NULL, na.rm = FALSE, ...)

\S4method{rowIQRs}{matrix_OR_array_OR_table_OR_numeric}(x, rows = NULL, cols = NULL, na.rm = FALSE, ...)

\S4method{rowIQRs}{ANY}(x, rows = NULL, cols = NULL, na.rm = FALSE, ...)

colIQRs(x, rows = NULL, cols = NULL, na.rm = FALSE, ...)

\S4method{colIQRs}{matrix_OR_array_OR_table_OR_numeric}(x, rows = NULL, cols = NULL, na.rm = FALSE, ...)

\S4method{colIQRs}{ANY}(x, rows = NULL, cols = NULL, na.rm = FALSE, ...)
}
\arguments{
\item{x}{An NxK matrix-like object.}

\item{rows, cols}{A \code{\link{vector}} indicating the subset of rows
(and/or columns) to operate over. If \code{\link{NULL}}, no subsetting is
done.}

\item{na.rm}{If \code{\link[base:logical]{TRUE}}, \code{\link{NA}}s
are excluded first, otherwise not.}

\item{...}{Additional arguments passed to specific methods.}
}
\value{
Returns a \code{\link{numeric}} \code{\link{vector}} of length N (K).
}
\description{
Calculates the interquartile range for each row (column) of a matrix-like
object.
}
\details{
The S4 methods for \code{x} of type \code{\link{matrix}},
\code{\link{array}}, or \code{\link{numeric}} call
\code{matrixStats::rowIQRs}
/ \code{matrixStats::colIQRs}.
}
\examples{
  mat <- matrix(rnorm(15), nrow = 5, ncol = 3)
  mat[2, 1] <- NA
  mat[3, 3] <- Inf
  mat[4, 1] <- 0
  
  print(mat)
  
  rowIQRs(mat)
  colIQRs(mat)
  
}
\seealso{
\itemize{
\item \code{matrixStats::\link[matrixStats]{rowIQRs}()} and
\code{matrixStats::\link[matrixStats:rowIQRs]{colIQRs}()} which are used
when the input is a \code{matrix} or \code{numeric} vector.
\item For a non-robust analog, see \code{\link{rowSds}()}. For a more
robust version see \code{\link[=rowMads]{rowMads()}}
\item \code{stats::\link[stats]{IQR}()}.
}
}
\keyword{array}
\keyword{iteration}
\keyword{robust}
\keyword{univar}
