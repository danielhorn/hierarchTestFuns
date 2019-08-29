#'@rdname hzhf
#'@export


makeHZHF13 = function(in.dim, k, c = 0.5, s = 0, check = TRUE) {

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
    list(name = "s_decept", ids = 1:k, params = list(A = 0.35, B = 0.1, C = 0.05)),
    list(name = "s_multi", ids = (k + 1):in.dim, params = list(A = 10, B = 30, C = 0.35))
  ))

  trafo3 = makeWFGTrafo(list(
    list(name = "r_nonsep", ids = 1:k, params = list(A = k)),
    list(name = "r_nonsep", ids = (k + 1):in.dim, params = list(A = in.dim - k))
  ))

  trafos = list(trafo1, trafo2, trafo3)
  makeCustomHierarchWFG(name = "HZHF13", in.dim = in.dim, k = k, z.max = z.max,
    trafos = trafos, c = c, s = s, check = check)
}
