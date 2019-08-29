#'@rdname hzhf
#'@export


makeHZHF11 = function(in.dim, k, c = 0.5, s = 0, check = TRUE) {

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
    list(name = "s_multi", ids = 1:in.dim, params = list(A = 5, B = 10, C = 0.35))
  ))

  trafo3 = makeWFGTrafo(list(
    list(name = "r_sum", ids = 1:k, params = list(w = 2 *
        1:k)),
    list(name = "r_sum", ids = (k + 1):in.dim, params = list(w = 2 *
        (k + 1):in.dim))
  ))

  trafos = list(trafo1, trafo2, trafo3)
  makeCustomHierarchWFG(name = "HZHF11", in.dim = in.dim, k = k, z.max = z.max,
    trafos = trafos, c = c, s = s, check = check)
}
