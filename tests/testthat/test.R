context("cf")

test_that("cf1", {

  myTest = function(id, in.dim = 5, k = 4, c = 0.5, s = 0) {
    f = makeHZHF(id, in.dim = in.dim, k = k, c = c, s = s)
    pl = plot(f, resolution = 4)
    pl = plot(f, resolution = 4, ids = c(k, in.dim))

    if (c <= 0.7) {
      val = f(0.7 * 1:in.dim)
    } else {
      val = f(c(0.7 * 1:k, rep(NA, in.dim - k)))
    }
    expect_equal(val, 0)
  }


  for (id in 1:15) {
    for (c in c(0.5, 1.5)) {
      myTest(id, in.dim = 3, k = 2)
      myTest(id, in.dim = 6, k = 4)
    }
  }
})
