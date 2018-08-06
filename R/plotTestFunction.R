#' @title Plot function for hzhf function
#'
#' @param x [\code{function}] \cr
#'   Size of parameter space.
#' @param resolution [\code{integer(1)}] \cr
#'   Number non-hierarchical parameters. Must be small than in.dim - 1.
#' @param ids [\code{double(1)}] \cr
#'   Metaparameter of all test functions, size of hierarchical area, see details. Default is 0.5.
#' @param def.vec [\code{double(1)}] \cr
#'   Metaparameter of all test functions, shift of hierarchical area, see details.. Default is 0.
#' @param ... [any] \cr
#'   Not used.
#' @return A \code{smoof_function}.
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

  ggplot(xs, aes_string(x = "x1", y = "x2", z = "value", fill = "value")) +
    geom_raster() + geom_contour(colour = "black", bins = 15) +
    scale_fill_gradientn(colours=c("#000070","#EEFFFF")) +
    xlab(paste("x", ids[1], sep = "")) + ylab(paste("x", ids[2], sep = ""))


}
