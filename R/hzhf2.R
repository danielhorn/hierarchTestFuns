
#'@export

makeHZHF2 = function(in.dim, k, c, s) {

  in.dim = asCount(in.dim)
  k = asInteger(k, lower = 1L, upper = in.dim - 1L)
  stopifnot(k %% 2 == 0)
  assertNumeric(c, lower = 0, upper = 2)
  assertNumeric(s)

  z.max = 2 * 1:in.dim

  trafo1 = makeWFGTrafo(list(
    list(name = "s_linear", ids = 1:in.dim, params = list(A = 0.35))
  ))

  rIds1 = split(1:k, rep(1:(k / 2), each = 2L))

  trafo2 = makeWFGTrafo(c(
    lapply(seq_along(rIds1), function(i) {
      list(name = "r_nonsep", ids = rIds1[[i]], params = list(A = 2))
    }),
    list(list(name = "identity", ids = (k + 1):in.dim))
    ))

  k2 = k / 2
  in.dim2 = in.dim - k2

  trafo3 = makeWFGTrafo(list(
    list(name = "r_sum", ids = 1:k2, params = list(w = 2 * 1:k2)),
    list(name = "r_sum", ids = (k2 + 1):in.dim2, params = list(w = 2 * k2:in.dim2))))

  trafos = list(trafo1, trafo2, trafo3)
  makeCustomHierarchWFG(name = "HZHF2", in.dim = in.dim, k = k, z.max = z.max,
    trafos = trafos, c = c, s = s)
}
