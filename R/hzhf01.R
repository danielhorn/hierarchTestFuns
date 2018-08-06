
#'@export

makeHZHF01 = function(in.dim, k, c, s) {

  in.dim = checkmate::asCount(in.dim)
  k = checkmate::asInteger(k, lower = 1L, upper = in.dim)
  assertNumeric(c, lower = 0, upper = 2)
  assertNumeric(s)

  z.max = 2 * 1:in.dim

  trafo1 = makeWFGTrafo(list(
    list(name = "s_linear", ids = 1:in.dim, params = list(A = 0.35))
  ))

  trafo2 = makeWFGTrafo(list(
    list(name = "b_poly", ids = 1:in.dim, params = list(alpha = 0.5))
  ))

  trafo3 = makeWFGTrafo(list(
    list(name = "r_sum", ids = 1:k, params = list(w = 2 * 1:k)),
    list(name = "r_sum", ids = (k + 1):in.dim, params = list(w = 2 * (k + 1):in.dim))
  ))

  trafos = list(trafo1, trafo2, trafo3)

  makeCustomHierarchWFG(name = "HZHF01", in.dim = in.dim, k = k, z.max = z.max,
    trafos = trafos, c = c, s = s)
}
