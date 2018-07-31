
makeHWfGf4 = function(in.dim, k, s, c) {
  n = in.dim
  z.max = 2 * 1:n


  trafo1 = makeWFGTrafo(list(list(name = "s_multi", ids = 1:n,
    params = list(A = 30, B = 10, C = 0.35))))


  trafo2 = makeWFGTrafo(list(
    list(name = "r_sum", ids = 1:k, params = list(w = 2 *
        1:k)),
    list(name = "r_sum", ids = (k + 1):n, params = list(w = 2 *
        (k + 1):n))))

  trafos = list(trafo1, trafo2)
  makeCustomHierarchWFG(z.max, A, trafos, k, c, s)
}
