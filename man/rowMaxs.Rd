% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rowMaxs.R
\docType{methods}
\name{rowMaxs}
\alias{rowMaxs}
\alias{rowMaxs,matrix-method}
\alias{rowMaxs,numeric-method}
\alias{colMaxs}
\alias{colMaxs,matrix-method}
\alias{colMaxs,numeric-method}
\title{Calculates the maximum for each row (column) of a matrix-like object}
\usage{
rowMaxs(x, rows = NULL, cols = NULL, na.rm = FALSE, ...)

\S4method{rowMaxs}{matrix}(x, rows = NULL, cols = NULL,
  na.rm = FALSE, dim. = dim(x))

\S4method{rowMaxs}{numeric}(x, rows = NULL, cols = NULL,
  na.rm = FALSE, dim. = dim(x))

colMaxs(x, rows = NULL, cols = NULL, na.rm = FALSE, ...)

\S4method{colMaxs}{matrix}(x, rows = NULL, cols = NULL,
  na.rm = FALSE, dim. = dim(x))

\S4method{colMaxs}{numeric}(x, rows = NULL, cols = NULL,
  na.rm = FALSE, dim. = dim(x))
}
\arguments{
\item{x}{An NxK matrix-like object.}

\item{rows, cols}{A \code{\link[base]{vector}} indicating the subset (and/or
columns) to operate over. If \code{\link[base]{NULL}}, no subsetting is done.}

\item{na.rm}{If \code{\link[base:logical]{TRUE}}, \code{\link[base]{NA}}s
are excluded first, otherwise not.}

\item{...}{Additional arguments passed to specific methods.}

\item{dim.}{An \code{\link[base]{integer}} \code{\link[base]{vector}} of
length two specifying the dimension of \code{x}, essential when x is a
\code{\link[base]{numeric}} vector.}
}
\value{
Returns a \code{\link[base]{numeric}} \code{\link[base]{vector}} of
length N (K).
}
\description{
Calculates the maximum for each row (column) of a matrix-like object.
}
\details{
The S4 methods for \code{x} of type \code{\link[base]{matrix}} or
\code{\link[base]{numeric}} call \code{matrixStats::\link[matrixStats]{rowMaxs}}
/ \code{matrixStats::\link[matrixStats]{colMaxs}}.
}
\examples{
  mat <- matrix(rnorm(15), nrow = 5, ncol = 3)
  mat[2, 1] <- NA
  mat[3, 3] <- Inf
  mat[4, 1] <- 0
  
  print(mat)
  
  rowMaxs(mat)
  colMaxs(mat)
  
}
\seealso{
\itemize{
\item \code{matrixStats::\link[matrixStats]{rowMaxs}()} and
  \code{matrixStats::\link[matrixStats]{colMaxs}()} which are used when
  the input is a \code{matrix} or \code{numeric} vector.
\item For min estimates, see \code{\link{rowMins}()}.
}
}
\keyword{array}
\keyword{iteration}
\keyword{robust}
\keyword{univar}