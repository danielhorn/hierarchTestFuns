
#'@export


makeHZHF05 = function(in.dim, k, s, c) {

  in.dim = asCount(in.dim)
  k = asInteger(k, lower = 1L, upper = in.dim - 1L)
  assertNumeric(c, lower = 0, upper = 2)
  assertNumeric(s)

  z.max = 2 * 1:in.dim

  trafo1 = makeWFGTrafo(list(
    list(name = "s_decept", ids = 1:in.dim, params = list(A = 0.35, B = 0.001, C = 0.05))
  ))


  trafo2 = makeWFGTrafo(list(
    list(name = "r_sum", ids = 1:k, params = list(w = 2 *
        1:k)),
    list(name = "r_sum", ids = (k + 1):in.dim, params = list(w = 2 *
        (k + 1):in.dim))
  ))

  trafos = list(trafo1, trafo2)

  makeCustomHierarchWFG(name = "HZHF05", in.dim = in.dim, k = k, z.max = z.max,
    trafos = trafos, c = c, s = s)
}
