#'@rdname hzhf
#'@export

makeHZHF03 = function(in.dim, k, c = 0.5, s = 0, check = TRUE) {

  in.dim = checkmate::asCount(in.dim)
  k = checkmate::asInteger(k, lower = 1L, upper = in.dim)

  z.max = 2 * 1:in.dim

  trafo1 = makeWFGTrafo(list(
    list(name = "s_linear", ids = 1:in.dim, params = list(A = 0.35))
  ))

  trafo2 = makeWFGTrafo(list(
    list(name = "b_flat", ids = 1:k, params = list(A = 0.8, B = 0.75, C = 0.85)),
    list(name = "identity", ids = (k + 1):in.dim)
  ))

  trafo3 = makeWFGTrafo(list(
    list(name = "b_poly", ids = 1:in.dim, params = list(alpha = 0.02))
  ))


  trafo4 = makeWFGTrafo(list(
    list(name = "r_sum", ids = 1:k, params = list(w = 2 * 1:k)),
    list(name = "r_sum", ids = (k + 1):in.dim, params = list(w = 2 * (k + 1):in.dim))
  ))

  trafos = list(trafo1, trafo2, trafo3, trafo4)

  makeCustomHierarchWFG(name = "HZHF03", in.dim = in.dim, k = k, z.max = z.max,
    trafos = trafos, c = c, s = s, check = check)
}
