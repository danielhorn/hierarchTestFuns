#'@rdname hzhf
#'@export


makeHZHF14 = function(in.dim, k, c = 0.5, s = 0, check = TRUE) {

  in.dim = asCount(in.dim)
  k = asInteger(k, lower = 1L, upper = in.dim - 1L)

  z.max = 2 * 1:in.dim

  trafo1 = makeWFGTrafo(c(
    list(list(name = "identity",ids = 1)),
    lapply(2:in.dim, function(i)
      list(name = "b_param", ids = i, y.prime.ids = 1:(i - 1),
        params = list(u = mean, A = 0.98 / 9.98, B = 0.02, C = 10)
      ))
  ))

  trafo2 = makeWFGTrafo(list(
    list(name = "s_multi", ids = 1:in.dim, params = list(A = 10, B = 30, C = 0.35))
  ))

  trafo3 = makeWFGTrafo(list(
    list(name = "b_flat", ids = 1:k, params = list(A = 0.8, B = 0.75, C = 0.85)),
    list(name = "identity", ids = (k + 1):in.dim)
  ))

  trafo4 = makeWFGTrafo(list(
    list(name = "b_poly", ids = 1:in.dim, params = list(alpha = 0.5))
  ))

  trafo5 = makeWFGTrafo(list(
    list(name = "r_nonsep", ids = 1:k, params = list(A = k)),
    list(name = "r_nonsep", ids = (k + 1):in.dim, params = list(A = in.dim - k))
  ))

  trafos = list(trafo1, trafo2, trafo3, trafo4, trafo5)
  makeCustomHierarchWFG(name = "HZHF14", in.dim = in.dim, k = k, z.max = z.max,
    trafos = trafos, c = c, s = s, check = check)
}
