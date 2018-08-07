#' @title Plot function for \code{hzhf_function} objects
#'
#' Produces a heat map / contour line plot for a given \code{hzhf_function}.
#'
#' @param x [\\code{hzhf_function}] \cr
#'   Function to be plotted
#' @param resolution [\code{integer(1)}] \cr
#'   Size of the grid of function evaluations. In total, resolution * resolution
#'   points are evaluated. Default is 128.
#' @param ids [\code{integer(2)}] \cr
#'   For functions with dimension \eqn{> 2}, only 2 parameters are varied and
#'   plotted. ids defines which, the default the eqn{1}. and the \eqn{(k + 1)}. parameter
#' @param def.vec [\code{double(in.dim)}] \cr
#'   Defines values for the parameters not varied. (For simplicity, value for all parameters
#'   must be specified, however, the values \code{def.vec[ids]} will be ignored).
#'   Defaults to the optimum of the function, i.e. \code{0.7 * 1:in.dim}
#' @param ... [any] \cr
#'   Not used.
#' @return A \code{smoof_function}.
#'
#'
#'
#'
#' @export

plot.hzhf_function = function(x, resolution = 128, ids = NULL,
  def.vec = 0.35 * getZMax(f), ...) {

  f = x

  in.dim = getNumberOfParameters(f)
  k = getK(f)
  l = in.dim - k

  if (is.null(ids)) {
    ids = c(1, getK(f) + 1)
  }


  x = seq(0, 1, length.out = resolution)
  xs = expand.grid(getZMax(f)[ids[1]] * x, getZMax(f)[ids[2]] * x)

  # Build design, set xs values and evaluate.
  # Include NAs for hierarchical vars, if x settings are not valid
  des = as.data.frame(t(replicate(resolution^2, def.vec)))
  names(des) = getParamIds(getParamSet(f), repeated = TRUE, with.nr = TRUE)
  des[, ids] = xs
  y = apply(des, 1, function(x) {
    res = try(f(x), silent = TRUE)
    if (inherits(res, "try-error")) f(c(x[1:k], rep(NA, l))) else res
  })

  xs = as.data.frame(cbind(xs, y))
  names(xs) = c("x1", "x2", "value")

  pl = ggplot(xs, aes_string(x = "x1", y = "x2", z = "value", fill = "value")) +
    geom_raster() + geom_contour(colour = "black", bins = 15) +
    scale_fill_gradientn(colours=c("#000070","#EEFFFF")) +
    xlab(paste("x", ids[1], sep = "")) + ylab(paste("x", ids[2], sep = ""))

  return(pl)
}
