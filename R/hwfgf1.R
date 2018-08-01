
#'@export

makeHWFGF1 = function(in.dim, k, c, s) {
  n = in.dim
  z.max = 2 * 1:n

  trafo1 = makeWFGTrafo(list(#list(name = "identity", ids = (k + 1):n),
    list(name = "s_linear", ids = 1:n, params = list(A = 0.35))))

  trafo2 = makeWFGTrafo(list(
    list(name = "b_flat", ids = 1:k, params = list(A = 0.8, B = 0.75, C = 0.85)),
    list(name = "identity", ids = (k + 1):n)
  ))

  trafo3 = makeWFGTrafo(list(list(name = "b_poly", ids = 1:n,
    params = list(alpha = 0.5))))


  trafo4 = makeWFGTrafo(list(
    list(name = "r_sum", ids = 1:k, params = list(w = 2 * 1:k)),
    list(name = "r_sum", ids = (k + 1):n, params = list(w = 2 * (k + 1):n))))

  trafos = list(trafo1, trafo2, trafo3, trafo4)
  makeCustomHierarchWFG(z.max, trafos, k, c, s)
}
