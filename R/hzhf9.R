
#'@export


makeHZHF9 = function(in.dim, k, s, c) {

  in.dim = asCount(in.dim)
  k = asInteger(k, lower = 1L, upper = in.dim - 1L)
  assertNumeric(c, lower = 0, upper = 2)
  assertNumeric(s)

  n = in.dim

  z.max = 2 * 1:in.dim

  trafo1 = makeWFGTrafo(c(
    list(list(name = "identity",ids = 1:k)),
    lapply((k + 1):n, function(i)
      list(name = "b_param", ids = i, y.prime.ids = 1:(i - 1),
        params = list(u = mean, A = 0.98 / 49.98, B = 0.02, C = 50)
      ))
  ))

  trafo2 = makeWFGTrafo(list(
    list(name = "s_decept", ids = 1:k, params = list(A = 0.35, B = 0.001, C = 0.05)),
    list(name = "s_multi", ids = (k + 1):in.dim, params = list(A = 30, B = 95, C = 0.35))
  ))

  trafo3 = makeWFGTrafo(list(
    list(name = "r_nonsep", ids = 1:k, params = list(A = k)),
    list(name = "r_nonsep", ids = (k + 1):in.dim, params = list(A = in.dim - k))
  ))

  trafos = list(trafo1, trafo2, trafo3)
  makeCustomHierarchWFG(name = "HZHF9", in.dim = in.dim, k = k, z.max = z.max,
    trafos = trafos, c = c, s = s)
}
