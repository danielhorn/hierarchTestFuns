

plotTestFunction = function(f, solution = 128) {
  x = seq(0, 1, length.out = solution)
  xs = expand.grid(2 * x, 4 * x)
  xs = cbind(xs,  apply(xs, 1, f))
  # FIXME: Warum sind hier NaN
  xs = as.data.frame(xs[is.finite(xs[, 3]), ])
  names(xs) = c("x1", "x2", "value")

  ggplot(xs, aes(x = x1, y = x2, z = value, fill = value)) +
    geom_raster() + geom_contour(colour = "black", bins = 15) +
    scale_fill_gradientn(colours=c("#000070","#EEFFFF")) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6))


}
