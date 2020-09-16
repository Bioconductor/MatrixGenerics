### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### load_next_suggested_package_to_search()
###
### The purpose of load_next_suggested_package_to_search() is to support
### useful fallbacks methods i.e. "ANY" methods that implement a fallback
### mechanism in case dispatch failed to find a suitable method.
### Only used for the colVars() generic for now, as a proof of concept. See
### colVars#ANY method below.
### This file is a temporary place for load_next_suggested_package_to_search().
### If we decide to add this type of fallback methods for all the generics in
### MatrixGenerics, load_next_suggested_package_to_search() would need to go
### to its own .R file.

### All packages listed below must also be listed in the Suggests field.
### They are expected to implement methods for the generics defined in
### MatrixGenerics. No need to list matrixStats here, MatrixGenerics already
### imports it by default.
.SUGGESTED_PACKAGES_TO_SEARCH <- c(
    "sparseMatrixStats",
    ## We list DelayedMatrixStats even though the methods defined in that
    ## package won't be found by the generics in MatrixGenerics. This is
    ## because DelayedMatrixStats defines its own matrixStats generics.
    "DelayedMatrixStats"
    #, ... add more packages in the future
)

.long_and_fancy_errmsg <- function(crude_errmsg, unloaded_pkgs)
{
    plural <- length(unloaded_pkgs) > 1L
    errmsg <- paste0(crude_errmsg,  "\n  However, the following package",
        if (plural) "s are" else " is",
        " likely to contain the missing method\n  but ",
        if (plural) "are" else "is", " not installed: ",
        paste0(unloaded_pkgs, collapse=", "), "\n  ",
        "Please install ", if (plural) "them" else "it",
        " (with 'BiocManager::install()') and try again.")
    if (plural)
        errmsg <- paste0(errmsg, "\n  Alternatively, if you know where ",
                         "the missing method is defined, install\n  only ",
                         "that package.")
    errmsg
}

### Try to load installed packages first.
load_next_suggested_package_to_search <- function(GENERIC, x)
{
    crude_errmsg <- paste0("Failed to find a ", GENERIC," method for ",
                           class(x), " objects.")
    is_loaded <- vapply(.SUGGESTED_PACKAGES_TO_SEARCH, isNamespaceLoaded,
                        logical(1))
    if (all(is_loaded))
        stop(crude_errmsg)
    unloaded_pkgs <- .SUGGESTED_PACKAGES_TO_SEARCH[!is_loaded]
    for (pkg in unloaded_pkgs) {
        if (requireNamespace(pkg, quietly=TRUE))
            return()
    }
    stop(.long_and_fancy_errmsg(crude_errmsg, unloaded_pkgs))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#' Calculates the variance for each row (column) of a matrix-like object
#'
#' Calculates the variance for each row (column) of a matrix-like object.
#' 
#' @templateVar rowName rowVars
#' @templateVar colName colVars
#' 
#' @template matrixStatsLink
#' 
#' @template standardParameters
#' @template na_rmParameter
#' @param center (optional) the center, defaults to the row means.
#' @template dimParameter
#'
#' @template returnVector
#'
#' @seealso
#' \itemize{
#' \item \code{matrixStats::\link[matrixStats]{rowVars}()} and
#'   \code{matrixStats::\link[matrixStats:rowVars]{colVars}()} which are used 
#'   when the input is a \code{matrix} or \code{numeric} vector.
#' \item For mean estimates, see \code{\link{rowMeans2}()} and
#'   \code{\link[base:colSums]{rowMeans}()}.
#' \item For standard deviation estimates, see \code{\link{rowSds}()}.
#' \item \code{stats::\link[stats:cor]{var}()}.
#' }
#' 
#' @template standardExamples
#'
#' @keywords array iteration robust univar
#'
#' @name rowVars
#' @export
setGeneric("rowVars", function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, ...) standardGeneric("rowVars"),
           signature = "x"
)

.default_rowVars <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...){
  matrixStats::rowVars(x, rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
}

#' @rdname rowVars
setMethod("rowVars", signature = "matrix", .default_rowVars)

#' @rdname rowVars
setMethod("rowVars", signature = "numeric", .default_rowVars)

#' @rdname rowVars
setMethod("rowVars", signature = "array", .default_rowVars)




#' @rdname rowVars
#' @name colVars
#' @export
setGeneric("colVars", function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, ...) standardGeneric("colVars"),
           signature = "x"
)

.default_colVars <- function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...){
  matrixStats::colVars(x, rows = rows, cols = cols, na.rm = na.rm, center = center, dim. = dim., ...)
}

#' @rdname rowVars
setMethod("colVars", signature = "matrix", .default_colVars)

#' @rdname rowVars
setMethod("colVars", signature = "numeric", .default_colVars)

#' @rdname rowVars
setMethod("colVars", signature = "array", .default_colVars)

#' @rdname rowVars
setMethod("colVars", signature = "ANY",
  function(x, rows = NULL, cols = NULL, na.rm = FALSE, center = NULL, dim. = dim(x), ...){
    load_next_suggested_package_to_search("colVars()", x)
    callGeneric()
  }
)


