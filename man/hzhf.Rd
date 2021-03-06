% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hzhf.R, R/hzhf01.R, R/hzhf02.R, R/hzhf03.R,
%   R/hzhf04.R, R/hzhf05.R, R/hzhf06.R, R/hzhf07.R, R/hzhf08.R, R/hzhf09.R,
%   R/hzhf10.R, R/hzhf11.R, R/hzhf12.R, R/hzhf13.R, R/hzhf14.R, R/hzhf15.R,
%   R/hzhf_doc.R
\name{makeHZHF}
\alias{makeHZHF}
\alias{makeHZHF01}
\alias{makeHZHF02}
\alias{makeHZHF03}
\alias{makeHZHF04}
\alias{makeHZHF05}
\alias{makeHZHF06}
\alias{makeHZHF07}
\alias{makeHZHF08}
\alias{makeHZHF09}
\alias{makeHZHF10}
\alias{makeHZHF11}
\alias{makeHZHF12}
\alias{makeHZHF13}
\alias{makeHZHF14}
\alias{makeHZHF15}
\alias{hzhf}
\alias{HZHF}
\title{Generators for HZHF test function}
\usage{
makeHZHF(id, in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF01(in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF02(in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF03(in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF04(in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF05(in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF06(in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF07(in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF08(in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF09(in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF10(in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF11(in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF12(in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF13(in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF14(in.dim, k, c = 0.5, s = 0, check = TRUE)

makeHZHF15(in.dim, k, c = 0.5, s = 0, check = TRUE)
}
\arguments{
\item{id}{[\code{integer(1)}] \cr
ID of hzhf function to generate, element of {1, 2, ..., 15}.}

\item{in.dim}{[\code{integer(1)}] \cr
Size of parameter space.}

\item{k}{[\code{integer(1)}] \cr
Number of non-hierarchical parameters. Must be smaller than in.dim - 1,
must be divisible by 2 for HFHF6, HZHF7, HZHF8.}

\item{c}{[\code{double(1)}] \cr
Metaparameter of all test functions, size of hierarchical area, see details. Default is 0.5.}

\item{s}{[\code{double(1)}] \cr
Metaparameter of all test functions, shift of hierarchical area, see details.. Default is 0.}

\item{check}{[\code{logical(1)}] \cr
Shall parameter checks be performed befor each function evaluation? Default is TRUE}
}
\value{
A \code{hzhf_function}, subclass of \code{smoof_function}.
}
\description{
Hierarchical test functions by Horn and Zaefferer
}
\details{
Hierarchical test functions have several parameters that only influence the
outcome if a certain condition is fullfilled. The HZHF test function family
consists of 15 hierarchical test functions with numeric parameters
\eqn{z[1], ... z[in.dim]}. The first \eqn{k} of these parameters always affect
the function value while the last \eqn{in.dim - k} ones only do if \eqn{z[1]}
fullfills a certain condition.

All HZHF functions are to be minimized and follow the scheme

\eqn{f(z) = a(z[1], ..., z[k]) +} if (\eqn{z[1] < c}) \eqn{b(z[1], ..., z[in.dim])} else s

Here, \eqn{c} controls the size of the active region of the hierarchical
parameters. \eqn{s} is a shift added to the function if the hierarchical
parameters are inactive and controls a) whether an imputation approach
is promising and b) the jump height at the discontinuity.

The function \eqn{a} and \eqn{b} are defined using different transformation
of the WFG toolkit (A toolkit for multi-objective test functions). The 15
HZHF function differ in their properties, as for example modality and
seperability.

For \eqn{s = 0} and \eqn{c < 0.7} the global optimum is allways at \eqn{z[i] = 0.7 * i, i = 1, ..., in.dim}
(the hierarchical parameters are active).
If \eqn{s = 0} and \eqn{c > 0.7}  holds, the optimum is at \eqn{z[i] = 0.7 * i, i = 1, ..., k},
\eqn{z[i] = NA, i = k + 1, ..., in.dim}, i.e. the hierarchical parameters are inactive.
The best possible function value is 0 in both cases.

If \eqn{s < 0} and the optimum is in in the inactive region, its function value
is shifted to \eqn{s}, but its location remains unchanged. If \eqn{s < 0} and
the optimum is in the active region, the former global optimum is only
guarenteed to be a local optimum with function value \eqn{0}. Their may be
other local optima in the inactive regions with function values \eqn{< 0},
especially at the discontinuity \eqn{z[1] = c}. Unfortunately, we can not
control those values.

If \eqn{s > 0} and the optimum is in the active region, it is guarenteed
to be the global optimum with function value 0. If, however, \eqn{s > 0}
and the optimum is in the inactive region, it is only guarenteed to be
a local optimum with function value \eqn{s}. There may be other local optima
in the active regions with function values between \eqn{0} and \eqn{b},
especially at the discontinuity \eqn{z[1] = c}. Unfortunately, we can not
control those values.
}
\references{
Huband, Simon ; Hingston, Phil ; Barone, Luigi ; While, Lyndon:
A Review of Multiobjective Test Problems and a Scalable Test Problem
Toolkit. In: IEEE Trans. on Evolutionary Computation 10 (2006),
No. 5, pp. 477-506
}
\seealso{
\code{\link{plot.hzhf_function}}
}
