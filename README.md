[<img src="https://www.bioconductor.org/images/logo/jpg/bioconductor_logo_rgb.jpg" width="200" align="right"/>](https://bioconductor.org/)

**MatrixGenerics** is an R/Bioconductor package that provides the S4 generics for popular row and column aggregation functions for matrices (e.g. `colVars()`, `rowMedians()`). It follows the API developed by the [**matrixStats**](https://cran.r-project.org/package=matrixStats) package. The target audience for **MatrixGenerics** are R package developers that want to write code that can automatically handle different kind of matrix implementations: for example base R `matrix`, the S4 `Matrix` (including sparse representations), and `DelayedMatrix` objects.

A prerequisite to handle these matrix objects is that a package with the corresponding implementation is available. So far, there are three packages:

* [**matrixStats**](https://cran.r-project.org/package=matrixStats) for base R `matrix` objects
* [**DelayedMatrixStats**](https://bioconductor.org/packages/DelayedMatrixStats/) for `DelayedMatrix` objects from the [**DelayedArray**](https://bioconductor.org/packages/DelayedArray/) package
* [**sparseMatrixStats**](https://github.com/const-ae/sparseMatrixStats) for `dgCMatrix` (sparse matrix) objects from the **Matrix** package

This package imports **matrixStats** and automatically forwards all function calls with `matrix`, `numeric`, and `array` objects to **matrixStats**. To handle other matrix objects, the user must manually install the corresponding **xxxMatrixStats** package.

See https://bioconductor.org/packages/MatrixGenerics for more information including how to install the release version of the package (please refrain from installing directly from GitHub).

