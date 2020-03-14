if(FALSE){
# Make sure that nothing bad happens if the file is accidentally sourced

all_statements <- readLines(system.file("NAMESPACE", package = "matrixStats"))
exported_functions <- grep("^export\\(", all_statements, value = TRUE)
function_names <- gsub("^export\\((.+)\\)", "\\1", exported_functions)
all_col_functions <- grep("^col.+", function_names, value = TRUE)
all_row_functions <- grep("^row.+", function_names, value = TRUE)

fnc_list <- lapply(c(all_col_functions, all_row_functions), function(fnc_name) eval(parse(text = paste0("matrixStats::", fnc_name))))
# fnc_args <- lapply(fnc_list, function(fnc) names(formals(fnc)))
# unique(unlist(lapply(fnc_list, function(fnc) as.list(formals(fnc)))))
# unlist(fnc_args)

default_args <-  unlist(lapply(lapply(fnc_list, function(fnc) as.list(formals(fnc))), function(args){
  lapply(args, function(arg){
    if(is.vector(arg)) as.character(arg)
    else if(is.name(arg)) rlang::as_string(arg)
    else if(is.null(arg)) "NULL"
    else if(is.call(arg)) paste0(deparse(arg), collapse = ", ")
    else "NO IDEA"
  })
}))

parsed_matrix_stats_api <- data.frame(function_name = unlist(lapply(seq_along(fnc_list), function(idx) 
  rep(c(all_col_functions, all_row_functions)[[idx]], length((formals(fnc_list[[idx]])))))),
           function_arg = unlist(lapply(fnc_list, function(fnc) names(formals(fnc)))),
           default_value = default_args)

library(tidyverse)
parsed_matrix_stats_api %>%
  distinct(function_arg, default_value, .keep_all = TRUE) 

function_args <- list(x = list("mat"),
                      rows = list(NULL, 1:3),
                      cols = list(NULL, 2),
                      value = list(TRUE, FALSE, 0),
                      na.rm = list(TRUE, FALSE),
                      dim. = list("dim(mat)", "c(12L, 8L)"),
                      idxs = list(1, 2:3),
                      lag = list(1, 3),
                      differences = list(1, 2),
                      diff = list(1, 2),
                      trim = list(0, 1/3, 0.5),
                      lx = list("mat"),
                      center = list(NULL, 3.1),
                      constant = list(1.4826, 5),
                      which = list(2, 1),
                      method = list("'direct'", "'expSumLog'"),
                      probs = list("seq(from = 0, to = 1, by = 0.25)", "0.1"),
                      type = list(7L, 3L),  # Type of quantile estimator see  `?quantile`
                      drop = list(TRUE, FALSE),
                      ties.method = list("'max'", "'first'", "'dense'"),
                      preserveShape = list(FALSE, TRUE),
                      values = list(0, c(0, 1)),
                      w = list(1:16, NULL))

extra_statements<- list(
  colTabulates = "mat <- array(suppressWarnings(as.integer(mat)), dim(mat))",
  rowTabulates = "mat <- array(suppressWarnings(as.integer(mat)), dim(mat))",
  colOrderStats = "mat[is.na(mat)] <- 4.1",
  rowOrderStats = "mat[is.na(mat)] <- 4.1",
  rowWeightedMeans = "mat <- array(mat, dim(t(mat)))"
)

testable_functions <- c(all_col_functions, all_row_functions)
testable_functions <- setdiff(testable_functions, c("colAnyMissings", "rowAnyMissings"))

res <- paste0(sapply(testable_functions, function(fnc_name){
  can_load <- tryCatch({eval(parse(text = fnc_name))}, error = function(error) error)
  skip <- if(is(can_load, "error")){
    paste0('\tskip("', fnc_name, ' not yet implemented")')
  }else{
    ""
  }
  fnc_ms <- eval(parse(text = paste0("matrixStats::", fnc_name)))
  default_args <- as.list(formals(fnc_ms))
  arg_missing <- setdiff(names(default_args)[sapply(default_args, function(arg) is.name(arg))], "...")
  filled_in_args <- function_args[arg_missing]
  filled_in_args_str <- lapply(seq_along(arg_missing), function(idx) paste0(arg_missing[idx], " = ", filled_in_args[[idx]]))
  # First call without any additional argument
  default_tests <- paste0(sapply(seq_len(max(lengths(filled_in_args_str))), function(idx){
    argument_string <-   paste0( sapply(filled_in_args_str, function(args) {
      args[(idx - 1) %% length(args) + 1]
    }), collapse = ", ")
    paste0("\tmg_res_def_", idx, " <- ", fnc_name, "(", argument_string, ")\n",
           "\tms_res_def_", idx, " <- matrixStats::", fnc_name, "(", argument_string, ")\n",
           "\texpect_equal(mg_res_def_", idx, ", ms_res_def_", idx, ")")
  }), collapse = "\n\n")
  
  # Now all combinations of parameters
  arg_names <- setdiff(names(default_args), "...")
  filled_in_args <- function_args[arg_names]
  filled_in_args_str <- lapply(seq_along(arg_names), function(idx) paste0(arg_names[idx], " = ", filled_in_args[[idx]]))
  param_tests <- paste0(sapply(seq_len(max(lengths(filled_in_args_str))), function(idx){
    argument_string <-   paste0( sapply(filled_in_args_str, function(args) {
      args[(idx - 1) %% length(args) + 1]
    }), collapse = ", ")
    paste0("\tmg_res_", idx, " <- ", fnc_name, "(", argument_string, ")\n",
           "\tms_res_", idx, " <- matrixStats::", fnc_name, "(", argument_string, ")\n",
           "\texpect_equal(mg_res_", idx, ", ms_res_", idx, ")")
  }), collapse = "\n\n")
  
  extra_stat <- ""
  if(fnc_name %in% names(extra_statements)){
    # Do special stuff for those functions
    extra_stat <- paste0(paste0("\t", extra_statements[[fnc_name]], collapse = "\n"), "\n")
  }
  
  full_test <- paste0('test_that("', fnc_name,  ' works ", {\n',
                      skip, "\n", extra_stat, default_tests, "\n\n", param_tests, "\n})")

  full_test
}), collapse = "\n\n\n")

preamble <- "
# Generated by tests/testthat/generate_tests_helper_script.R
# do not edit by hand


# Make a matrix with different features
mat <- matrix(rnorm(16 * 6), nrow = 16, ncol = 6)
mat[1,1] <- 0
mat[2,3] <- NA
mat[3,3] <- -Inf
mat[5,4] <- NaN
mat[5,1] <- Inf
mat[6,2] <- 0
mat[6,5] <- 0
\n\n\n"


writeLines(paste0(preamble, res), "tests/testthat/test-api_compatibility.R")


}
