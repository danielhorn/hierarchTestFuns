
#'@export


makeHWfGf9 = function(in.dim, k, s, c) {
  n = in.dim
  z.max = 2 * 1:n


  trafo1 = lapply((k + 1):n, function(i)
    list(
      name = "b_param",
      ids = i, y.prime.ids = 1:(i - 1),
      params = list(u = mean, A = 0.98 / 49.98, B = 0.02, C = 50)
    ))

  trafo1 = makeWFGTrafo(c(list(list(name = "identity",ids = 1:k)), trafo1))

  trafo2 = makeWFGTrafo(list(
    list(name = "s_decept", ids = 1:k, params = list(A = 0.35, B = 0.001, C = 0.05)),
    list(name = "s_multi", ids = (k + 1):n, params = list(A = 30, B = 95, C = 0.35))))

  trafo3 = makeWFGTrafo(list(
    list(name = "r_nonsep", ids = 1:k, params = list(A = k)),
    list(name = "r_nonsep", ids = (k + 1):n, params = list(A = n - k))))

  trafos = list(trafo1, trafo2, trafo3)
  makeCustomHierarchWFG(z.max, trafos, k, c, s)
}
