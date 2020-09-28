\name{NEWS}
\title{News for Package \pkg{MatrixGenerics}}
\encoding{UTF-8}

\section{Version 1.2.0}{\itemize{
  \item Add default methods with user-friendly fallback mechanism (\url{https://github.com/Bioconductor/MatrixGenerics/pull/16}). Suggested packages are now loaded the first time a MatrixGenerics' generic is called (e.g. the first time \code{MatrixGenerics::colVars()} is called). With this new approach, if the user passes a \emph{dgCMatrix} object and if \pkg{sparseMatrixStats} is already loaded, will 'just work' and the fallback mechanism won't try to load anything.
  \item Dispatch on methods for matrix objects when table objects are supplied (\url{https://github.com/Bioconductor/MatrixGenerics/pull/15})
}}

\section{Version 1.0.0}{\itemize{
  \item New package \pkg{MatrixGenerics}, providing S4 generic functions modeled after the 'matrixStats' API for alternative matrix implementations.
}}