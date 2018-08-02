#' WFG Transformations
#'
#' \code{wfgTrafoIdentity} is a transformation that changes nothing. \cr
#' \code{wfgTrafoBPoly} is the polynomial bias transformation. \cr
#' \code{wfgTrafoBFlat} creates a region in search space in which all points have the same objective values. \cr
#' \code{wfgTrafoBParam} is the parameter-dependent transformation. \cr
#' \code{wfgTrafoSLinear} creates a linear shift of the true optimum. \cr
#' \code{wfgTrafoSDecept} creates regions in the search space that have a sub-optimal value but larger area. \cr
#' \code{wfgTrafoSMulti} creates many local optima. \cr
#' \code{wfgTrafoRSum} creates a dependence between different search-space entries. \cr
#' \code{wfgTrafoRNonsep} creates a dependence between objectives. \cr
#'
#' @param alpha [\code{numeric(1)}] \cr
#'   wfgTrafoBPoly: alpha > 1 biases toward 0, alpha < 1 biases toward 1.
#' @param A [\code{numeric(1)}] \cr
#'  For parameter requirements see WFG Paper. Need at: \cr
#'  wfgTrafoBFlat, wfgTrafoBParam, wfgtrafoSLinear, wfgTrafoSDecept, wfgTrafoSMulti, wfgTrafoRNonsep
#' @param B [\code{numeric(1)}] \cr
#'  For parameter requirements see WFG Paper. Need at: \cr
#'  wfgTrafoBFlat, wfgTrafoBParam, wfgTrafoSDecept, wfgTrafoSMulti
#' @param C [\code{numeric(1)}] \cr
#'  For parameter requirements see WFG Paper. Need at: \cr
#'  wfgTrafoBFlat, wfgTrafoBParam, wfgTrafoSDecept, wfgTrafoSMulti
#' @param u [\code{function}] \cr
#'  At wfgTrafoBParamt
#' @param w [\code{numeric}] \cr
#'  At wfgTrafoRSum
#'
#' @return A \code{wfgTrafoFunction}.
#'
#' @name WFGTrafos
#'
#' @aliases wfgTrafo WFGTrafo

#' @rdname WFGTrafos
wfgTrafoIdentity = function(A) {
  # A as placeholder.
  trafo.function = function(y) y
  trafo.function = addClasses(trafo.function, "wfgTrafoFunction")
  return(trafo.function)
}

#' @rdname WFGTrafos
wfgTrafoBPoly = function(alpha) {
  assertNumber(alpha, lower = 0)
  if (alpha == 1L)
    stop("alpha must be odd 1.")

  trafo.function = function(y) y^alpha

  trafo.function = addClasses(trafo.function, "wfgTrafoFunction")
  return(trafo.function)
}

#' @rdname WFGTrafos
wfgTrafoBFlat = function(A, B, C) {
  assertNumber(A, lower = 0, upper = 1)
  assertNumber(B, lower = 0, upper = 1)
  assertNumber(C, lower = 0, upper = 1)
  if (B > C)
    stop("C must be greater than B")
  if (B == 0L && (A != 0 || C == 1))
    stop("Parameters not satiesfied.")
  if (C == 1L && (A != 1 || B == 0))
    stop("Parameters not satiesfied.")

  trafo.function = function(y) {
    tmp1 = pmin(0, floor(y - B)) * A * (B - y) / B
    tmp2 = pmin(0, floor(C - y)) * (1 - A) * (y - C) / (1 - C)
    A + tmp1 - tmp2
  }

  trafo.function = addClasses(trafo.function, "wfgTrafoFunction")
  return(trafo.function)
}

#' @rdname WFGTrafos
wfgTrafoBParam = function(u, A, B, C) {
  assertNumber(A, lower = 0, upper = 1)
  assertNumber(B, lower = A, upper = C)
  assertNumber(C, lower = B)

  trafo.function = function(y, y.prime) {
    y.prime = u(y.prime)
    tmp = A - (0.7 - 2 * y.prime) * abs(floor(0.35 - y.prime) + A)
    y^(B + (C - B) * tmp)
  }

  trafo.function = addClasses(trafo.function, "wfgTrafoFunction")
  return(trafo.function)
}

#' @rdname WFGTrafos
wfgTrafoSLinear = function(A) {
  assertNumber(A, lower = 0, upper = 1)

  trafo.function = function(y) {
    abs(y - A) / abs(floor(A - y) + A)
  }

  trafo.function = addClasses(trafo.function, "wfgTrafoFunction")
  return(trafo.function)
}

#' @rdname WFGTrafos
wfgTrafoSDecept = function(A, B, C) {
  assertNumber(A, lower = 0, upper = 1)
  assertNumber(B, lower = 0, upper = 1)
  assertNumber(C, lower = 0, upper = 1)
  if (A < B)
    stop("A must be greater than B.")
  if ((A + B) >= 1L)
    stop("A + B must be smaller than 1.")

  trafo.function = function(y) {
    tmp1 = floor(y - A + B) * (1 - C + (A - B) / B) / (A - B)
    tmp2 = floor(A + B - y) * (1 - C + (1 - A - B) / B) / (1 - A - B)
    1 + (abs(y - A) - B) * (tmp1 + tmp2 + 1 / B)
  }

  trafo.function = addClasses(trafo.function, "wfgTrafoFunction")
  return(trafo.function)
}

#' @rdname WFGTrafos
wfgTrafoSMulti = function(A, B, C) {
  A = asCount(A)
  # Fix me
  assertNumber(B, lower = 0)
  assertNumber(C, lower = 0, upper = 1)
  if (((4*A + 2) * pi) < (4 * B))
    stop("Parameters not satiesfied.")

  trafo.function = function(y) {
    tmp1 = abs(y - C) / (2 * (floor(C - y) + C))
    tmp2 = (4 * A + 2) * pi * (0.5 - tmp1)
    (1 + cos(tmp2) + 4 * B * (tmp1)^2) / (B + 2)
  }

  trafo.function = addClasses(trafo.function, "wfgTrafoFunction")
  return(trafo.function)
}

#' @rdname WFGTrafos
wfgTrafoRSum = function(w) {
  assertNumeric(w, lower = 0)

  trafo.function = function(y) {
    sum(w * y) / sum(w)
  }

  trafo.function = addClasses(trafo.function, "wfgTrafoFunction")
  return(trafo.function)
}

#' @rdname WFGTrafos
wfgTrafoRNonsep = function(A) {
  A = asCount(A)

  trafo.function = function(y) {
    n = length(y)
    if (A == 1L)
      return(wfgTrafoRSum(rep(1, n))(y))
    mat = array(dim = c(n, A - 1))
    for (i in 1:(A - 1))
      mat[, i] = y[1 + (i:(i + n - 1) %% n)]
    (sum(y) + sum(abs(y - mat))) / (n / A * ceiling(A / 2) * (1 + 2 * A - 2 * ceiling(A / 2)))
  }

  trafo.function = addClasses(trafo.function, "wfgTrafoFunction")
  return(trafo.function)
}


