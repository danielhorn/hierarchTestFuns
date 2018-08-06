
#'@export


makeHZHF09 = function(in.dim, k, s, c) {

  in.dim = asCount(in.dim)
  k = asInteger(k, lower = 1L, upper = in.dim - 1L)
  assertNumeric(c, lower = 0, upper = 2)
  assertNumeric(s)

  z.max = 2 * 1:in.dim

  trafo1 = makeWFGTrafo(c(
    list(list(name = "identity",ids = 1)),
    lapply(2:in.dim, function(i)
      list(name = "b_param", ids = i, y.prime.ids = 1:(i - 1),
        params = list(u = mean, A = 0.98 / 49.98, B = 0.02, C = 50)
      ))
  ))

  trafo2 = makeWFGTrafo(list(
    list(name = "s_linear", ids = 1:in.dim, params = list(A = 0.35))
  ))

  trafo3 = makeWFGTrafo(list(
    list(name = "r_nonsep", ids = 1:k, params = list(A = k)),
    list(name = "r_nonsep", ids = (k + 1):in.dim, params = list(A = in.dim - k))
  ))

  trafos = list(trafo1, trafo2, trafo3)
  makeCustomHierarchWFG(name = "HZHF09", in.dim = in.dim, k = k, z.max = z.max,
    trafos = trafos, c = c, s = s)
}
