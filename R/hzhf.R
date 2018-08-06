#'@rdname hzhf
#'@export


makeHZHF = function(id, in.dim, k, c = 0.5, s = 0, check = TRUE) {

  assertChoice(id, choices = 1:15)

  maker = switch(id, makeHZHF01, makeHZHF02, makeHZHF03, makeHZHF04, makeHZHF05,
    makeHZHF06, makeHZHF07, makeHZHF08, makeHZHF09, makeHZHF10, makeHZHF11,
    makeHZHF12, makeHZHF13, makeHZHF14, makeHZHF15)

  maker(in.dim = in.dim, k = k, c = c, s = s, check = check)
}
