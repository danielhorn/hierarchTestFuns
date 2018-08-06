
#'@export


makeHZHF06 = function(in.dim, k, s, c) {

  in.dim = asInt(in.dim, lower = 3L)
  k = asInteger(k, lower = 2L, upper = in.dim - 1L)
  if (k %% 2 != 0)
    stop("k must be divisble by 2.")
  assertNumeric(c, lower = 0, upper = 2)
  assertNumeric(s)

  z.max = 2 * 1:in.dim

  trafo1 = makeWFGTrafo(c(
    list(list(name = "identity",ids = 1)),
    lapply(1 + seq_len(k - 1), function(i)
      list(name = "b_param", ids = i, y.prime.ids = 1:(i - 1),
        params = list(u = mean, A = 0.98 / 49.98, B = 0.02, C = 50)
      )),
    list(list(name = "identity",ids = (k + 1):in.dim)
    )))

  trafo2 = makeWFGTrafo(list(
    list(name = "s_linear", ids = 1:in.dim, params = list(A = 0.35))
  ))

  trafo3 = makeWFGTrafo(list(
    list(name = "b_flat", ids = 1:k, params = list(A = 0.8, B = 0.75, C = 0.85)),
    list(name = "identity", ids = (k + 1):in.dim)
  ))

  trafo4 = makeWFGTrafo(list(
    list(name = "b_poly", ids = 1:in.dim, params = list(alpha = 0.5))
  ))

  rIds1 = split(1:k, rep(1:(k / 2), each = 2L))

  trafo5 = makeWFGTrafo(c(
    lapply(seq_along(rIds1), function(i) {
      list(name = "r_nonsep", ids = rIds1[[i]], params = list(A = 2))
    }),
    list(list(name = "identity", ids = (k + 1):in.dim))
  ))

  k2 = k / 2
  in.dim2 = in.dim - k2

  trafo6 = makeWFGTrafo(list(
    list(name = "r_sum", ids = 1:k2, params = list(w = 2 * 1:k2)),
    list(name = "r_sum", ids = (k2 + 1):in.dim2, params = list(w = 2 * (k2 + 1):in.dim2))))


  trafos = list(trafo1, trafo2, trafo3, trafo4, trafo5, trafo6)
  makeCustomHierarchWFG(name = "HZHF06", in.dim = in.dim, k = k, z.max = z.max,
    trafos = trafos, c = c, s = s)
}
