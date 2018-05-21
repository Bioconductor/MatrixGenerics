#' Calculates the weighted medians for each row (column) in a matrix-like object
#'
#' Calculates the weighted medians for each row (column) in a matrix-like object.
#'
#' @param x An NxK matrix-like object.
#'
#' @param w A \code{\link[base]{numeric}} \code{\link[base]{vector}} of length
#' K (N).
#'
#' @param rows,cols A \code{\link[base]{vector}} indicating subset of rows
#' (and/or columns) to operate over. If \code{\link[base]{NULL}}, no subsetting
#' is done.
#'
#' @param na.rm If \code{\link[base:logical]{TRUE}}, missing values are
#' excluded from the calculation, otherwise not.
#'
#' @param ... Additional arguments passed to specific methods.
#'
#' @return Returns a \code{\link[base]{numeric}} \code{\link[base]{vector}} of
#' length N (K).
#'
#' @seealso
#' \itemize{
#' \item See \code{\link[matrixStats]{rowWeightedMedians}()} and
#'   \code{\link[matrixStats]{colWeightedMedians}()} for the default methods.
#' \item See \code{\link{rowMedians}()} and \code{colMedians()} for
#'   non-weighted medians.
#' }
#' @keywords array iteration robust univar
#'
#' @name rowWeightedMedians
setGeneric(
  "rowWeightedMedians",
  function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
           ...) standardGeneric("rowWeightedMedians"),
  signature = "x",
  useAsDefault = function(x, w, rows = NULL, cols = NULL, na.rm = FALSE,
                          dim. = dim(x), ...) {
    if (!requireNamespace("matrixStats", quietly = TRUE)) {
      stop("'matrixStats' package required for matrix operations",
           call. = FALSE)
    }
    matrixStats::rowMedians(x, w = w, rows = rows, cols = cols, na.rm = na.rm,
                            dim. = dim., ...)
  })
setGenericImplicit("rowWeightedMedians")

#' @rdname rowWeightedMedians
#' @name colWeightedMedians
setGeneric(
  "colWeightedMedians",
  function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x),
           ...) standardGeneric("colWeightedMedians"),
  signature = "x",
  useAsDefault = function(x, w = NULL, rows = NULL, cols = NULL, na.rm = FALSE,
                          dim. = dim(x), ...) {
    if (!requireNamespace("matrixStats", quietly = TRUE)) {
      stop("'matrixStats' package required for matrix operations",
           call. = FALSE)
    }
    matrixStats::colWeightedMedians(x, w = w, rows = rows, cols = cols,
                                    na.rm = na.rm, dim. = dim., ...)
  })
setGenericImplicit("colWeightedMedians")