# MatrixGenerics

The `MatrixGenerics` package provides the S4 generics for popular row and column aggregation functions for matrices (e.g. `colVars()`, `rowMedians()`). It follows the API developed by the [`matrixStats`](https://cran.r-project.org/package=matrixStats) package. The target audience for `MatrixGenerics` are R package developers that want to write code that can automatically handle different kind of matrix implementations: for example base R `matrix`, the S4 `Matrix` (including sparse representations), and `DelayedMatrix` objects.

A prerequisite to handle these matrix objects is that a package with the corresponding implementation is available. So far, there are three packages:

* [`matrixStats`](https://cran.r-project.org/package=matrixStats) for base R `matrix` objects
* [`DelayedMatrixStats`](https://bioconductor.org/packages/DelayedMatrixStats/) for `DelayedMatrix` objects from the [`DelayedArray`](https://bioconductor.org/packages/DelayedArray/) package
* [`sparseMatrixStats`](https://github.com/const-ae/sparseMatrixStats) for `dgCMatrix` (sparse matrix) objects from the `Matrix` package

This package imports `matrixStats` and automatically forwards all function calls with `matrix`, `numeric`, and `array` objects to `matrixStats`. To handle other matrix objects, the user must manually install the corresponding `___MatrixStats` package.


## Caveat

Note that the package is not released yet and currently both `DelayedMatrixStats` and `sparseMatrixStats` roll their own S4 generic functions for the `matrixStats` API. However, we hope to submit this package to BioConductor soon and refactor both packages to use the generics defined here.
