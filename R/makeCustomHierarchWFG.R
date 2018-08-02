

makeCustomHierarchWFG = function(name, in.dim, k, z.max, trafos, c, s) {

  customHierarchWFG = function(z) {
    stopifnot(length(z) == in.dim)

    a = z[1:k]
    b = z[(k + 1):in.dim]

    if (!isFeasible(getParamSet(customHierarchWFG), list(a = a, b = b))) {
      stop("Input Parameter is not feasible. Correct dimension? Hierarchical structur violated?")
    }

    x = z / z.max
    for (i in seq_along(trafos)) {
      # Floating Point Arithmetic is a bitch. Round a bit here for better results
      x = round(x, 14)
      x = trafos[[i]](x)
    }

    x[1] + if (z[1] > c) x[2] else s
  }

  customHierarchWFG = makeSingleObjectiveFunction(
    name = name, id = name,
    description = "Testfunction for hierarchical parameter spaces",
    fn = customHierarchWFG,
    par.set = makeParamSet(
      makeNumericVectorParam("a", len = k, lower = 0, upper = z.max[1:k]),
      makeNumericVectorParam("b", len = in.dim - k, lower = 0,
        upper = z.max[(k + 1):in.dim],
        requires = substitute(a[1] > c, list(c = c)))
    )
  )

  addClasses(customHierarchWFG, "hierarchTestFun")

  return(customHierarchWFG)
}

getK = function(fun) {
  environment(environment(f)$fn)$k
}

getZMax = function(fun) {
  environment(environment(f)$fn)$z.max
}


