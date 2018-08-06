#' Generators for HZHF test function
#'
#' @param id [\code{integer(1)}] \cr
#'   ID of hzhf function to generate, element of {1, 2, ..., 15}.
#' @param in.dim [\code{integer(1)}] \cr
#'   Size of parameter space.
#' @param k [\code{integer(1)}] \cr
#'   Number non-hierarchical parameters. Must be small than in.dim - 1.
#' @param c [\code{double(1)}] \cr
#'   Metaparameter of all test functions, size of hierarchical area, see details. Default is 0.5.
#' @param s [\code{double(1)}] \cr
#'   Metaparameter of all test functions, shift of hierarchical area, see details.. Default is 0.
#' @param check [\code{logical(1)}] \cr
#'   Shall parameter checks be performed befor each function evaluation? Default is TRUE
#' @return A \code{hzhf_function}, subclass of \code{smoof_function}.
#'
#'
#' @aliases hzhf HZHF
#'
#' @name hzhf

NULL
