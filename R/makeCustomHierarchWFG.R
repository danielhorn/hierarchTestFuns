makeCustomHierarchWFG = function(z.max, A, trafos, k, c, s) {


  customHierarchWFG = function(z) {
    x = z / z.max
    for (i in seq_along(trafos)) {
      # Due to numerical reason, x could leave its limit [0, 1] here.
      x = pmin(1, pmax(0, trafos[[i]](x)))
    }
    x[1] + if (z[1] > c) x[2] else s
  }

  return(customHierarchWFG)
}
