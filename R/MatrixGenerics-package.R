#' The MatrixGenerics package
#'
#' The \pkg{MatrixGenerics} package defines S4 generic summary statistic
#' functions that operate on matrix-Like objects.
#'
# NOTE: Import a single function to quieten R CMD check NOTE:
#         Package in Depends field not imported from: ‘matrixStats’
#           These packages need to be imported from (in the NAMESPACE file)
#           for when this namespace is loaded but not attached.
#' @importFrom matrixStats allocArray
#' @import methods
#
#' @name MatrixGenerics-package
#' @exportClass matrix_OR_array_OR_table_OR_numeric
#' @aliases class:matrix_OR_array_OR_table_OR_numeric
#' @aliases matrix_OR_array_OR_table_OR_numeric-class
#' @aliases matrix_OR_array_OR_table_OR_numeric
# NOTE: Starting with R 4.0.0 a matrix is an array so no need to explicitly
# list "matrix" as a member of the union.
setClassUnion("matrix_OR_array_OR_table_OR_numeric",
    c("array", "table", "numeric")
)


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## make_default_method_def()
##

## All packages listed below must also be listed in the Suggests field.
## They are expected to implement methods for the generics defined in
## MatrixGenerics. No need to list matrixStats here as it is special and
## already imported by default.
.SUGGESTED_PACKAGES_TO_SEARCH <- c(
    "sparseMatrixStats",
    ## We list DelayedMatrixStats even though the methods defined in it
    ## won't be found by the generics in MatrixGenerics. This is because
    ## DelayedMatrixStats defines its own matrixStats generics.
    "DelayedMatrixStats"
    # ... add more packages in the future
)

.long_and_fancy_errmsg <- function(short_errmsg, unloaded_pkgs)
{
    plural <- length(unloaded_pkgs) > 1L
    pkgs_to_install <- if (plural) "..." else paste0("\"", unloaded_pkgs, "\"")
    errmsg <- paste0(short_errmsg,  "\n  However, the following package",
        if (plural) "s are" else " is",
        " likely to contain the missing method\n  but ",
        if (plural) "are" else "is", " not installed: ",
        paste0(unloaded_pkgs, collapse=", "), ".\n  ",
        "Please install ", if (plural) "them" else "it",
        " (with 'BiocManager::install(", pkgs_to_install, ")')",
        if (plural) " " else "\n  ", "and try again.")
    if (plural)
        errmsg <- paste0(errmsg, "\n  Alternatively, if you know where ",
                         "the missing method is defined, install\n  only ",
                         "that package.")
    errmsg
}

## The purpose of .load_next_suggested_package_to_search() is to support
## useful fallbacks methods i.e. "ANY" methods that implement a fallback
## mechanism in case dispatch failed to find a suitable method.
## Try to load installed packages first.
.load_next_suggested_package_to_search <- function(x, genericName=NULL)
{
    if (is.null(genericName)) {
        ## The ugly and hacky code below tries to find the name of the
        ## generic. Taken from the implementation of callGeneric().
        call <- sys.call(sys.parent(1L))
        .local <- identical(call[[1L]], quote(.local))
        methodCtxInd <- 1L + if (.local) 1L else 0L
        callerCtxInd <- methodCtxInd + 1L
        methodCall <- sys.call(sys.parent(methodCtxInd))
        if (methods:::fromNextMethod(methodCall))
            methodCtxInd <- methodCtxInd + 1L
        methodFrame <- parent.frame(methodCtxInd)
        genericName <- methods:::getGenericFromCall(methodCall, methodFrame)
        if (is.null(genericName))
          stop("when 'genericName' is not supplied, ",
               ".load_next_suggested_package_to_search()\n  ",
               "must be called from within a method body")
    }
    short_errmsg <- paste0("Failed to find a ", genericName,"() method ",
                           "for ", class(x), " objects.")
    is_loaded <- vapply(.SUGGESTED_PACKAGES_TO_SEARCH, isNamespaceLoaded,
                        logical(1))
    if (all(is_loaded))
        stop(short_errmsg)
    unloaded_pkgs <- .SUGGESTED_PACKAGES_TO_SEARCH[!is_loaded]
    for (pkg in unloaded_pkgs) {
        if (requireNamespace(pkg, quietly=TRUE)) {
            ## This is just a hack to refresh the method dispatch cache.
            ## Calling trace() on the method has the side effect of making
            ## showMethods(genericName) aware of the method.
            ## See https://github.com/Bioconductor/MatrixGenerics/pull/16#issuecomment-707516999
            ## for more information.
            GENERIC <- match.fun(genericName)
            suppressMessages(trace(GENERIC, signature=class(x)))
            suppressMessages(untrace(GENERIC, signature=class(x)))
            return()
        }
    }
    stop(.long_and_fancy_errmsg(short_errmsg, unloaded_pkgs))
}

make_default_method_def <- function(genericName)
{
    def <- function() { }
    formals(def) <- formals(match.fun(genericName))
    e <- expression(MatrixGenerics:::.load_next_suggested_package_to_search(x),
                    callGeneric())
    body(def) <- as.call(c(as.name("{"), e))
    def
}

