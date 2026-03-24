\name{FourTaStiC_dist}
\alias{FourTaStiC_dist}
\title{Compute the 4TaStiC dissimilarity matrix}
\description{
Computes the 4TaStiC dissimilarity for time series data and returns
the selected mixing parameter together with the pairwise dissimilarity object.

The 4TaStiC approach is based on time traveling and trend traveling. For each
pair of time series, the method searches over allowed time shifts and trend
tilts, then takes the minimum dissimilarity over those candidate alignments.
The base dissimilarity combines a correlation-based component and a weighted
Euclidean component.
}
\usage{
FourTaStiC_dist(
  data,
  L,
  E,
  Time = NULL,
  E.default = TRUE,
  pp = 0.08,
  C = 0,
  alpha = NULL
)
}
\arguments{
  \item{data}{
    A numeric matrix, data frame, or time series object. Rows are interpreted
    as observations and columns as time points after conversion.
  }

  \item{L}{
    A non-negative integer giving the maximum number of time steps allowed for
    shifting.
  }

  \item{E}{
    A numeric value or numeric vector giving the allowed trend-tilting values.
    In the 4TaStiC framework, this should contain \code{0}.
  }

  \item{Time}{
    Optional positive integer giving the number of time points when
    \code{data} is supplied as a \code{data.frame} or \code{time series} object and
    needs reshaping into matrix form.
  }

  \item{E.default}{
    Logical. If \code{TRUE}, the function internally expands the supplied
    tilting values symmetrically by using both negative and positive values, including 0.
    If \code{FALSE}, the values in \code{E} are used directly. For example, if \code{E = c(0.1, 0.2)} and \code{E.default = TRUE},
  then the function uses \code{c(-0.2, -0.1, 0, 0.1, 0.2)} internally;
  if \code{E.default = FALSE}, it uses only \code{c(0.1, 0.2)}.
  }

  \item{pp}{
    Numeric percentile in \code{(0, 1)} used in the default rule for selecting
    \code{alpha} when \code{alpha = NULL}.
  }

  \item{C}{
    A non-negative numeric penalty coefficient for the trend-traveling
    correlation term. Larger values reduce the impact of large trend tilts.
  }

\item{alpha}{
  Optional numeric value in \code{[0, 1]} controlling the balance between
  the correlation-based and Euclidean components. If \code{alpha} is
  \code{NULL}, the function uses \code{select.alpha(data, pp)}. The distance
  is defined as \eqn{\alpha d_{\mathrm{corr}} + (1-\alpha) D_{\mathrm{weu}}}.
}
}
\details{
Let \eqn{X} and \eqn{Y} be two time series. The 4TaStiC dissimilarity is
defined by minimizing a base dissimilarity over allowable time shifts
\eqn{l \in \{0,1,\dots,L\}} and allowable trend tilts \eqn{\epsilon \in E}.


The base dissimilarity used in this implementation interpolates between

\describe{
  \item{correlation-based dissimilarity}{
    A term of the form \eqn{1 - \exp(-C |\epsilon|)\mathrm{Corr}(\cdot,\cdot)}
    when trend traveling is applied.
  }
  \item{weighted Euclidean dissimilarity}{
    A Euclidean-type term computed on the corresponding aligned trend traveling
  }
}

The parameter \code{alpha} controls the trade-off between these two
components. A larger \code{alpha} gives more weight to the
correlation-based term. Parameter \code{alpha} is recommended to lie in
\eqn{[0,1]}, and a percentile-based default rule is proposed so that the
Euclidean term does not dominate when the scale of the data is large.

The penalty parameter \code{C >= 0} is intended to reduce the
chance of selecting excessively large tilts.
}
\value{
A list with two components:
\describe{
  \item{Alpha}{
    The value of the mixing parameter \code{alpha} used in the computation.
  }
  \item{ddist}{
    An object of class \code{"dist"} containing the pairwise 4TaStiC
    dissimilarities.
  }
}
}

\references{
Preedasawakul, O. and Wiroonsri, N. (2026).
\emph{4TaStiC: Time and trend traveling time series clustering for
classifying long-term type 2 diabetes patients}.
\emph{ACM Transactions on Computing for Healthcare}.
}

\examples{
library(FourTaStiC)

x <- TSdata9[,-21]


\dontrun{
out <- FourTaStiC_dist(
  data = x,
  L = 2,
  E = c(-0.05, 0, 0.05),
  pp = 0.08,
  C = 0
)

out$Alpha
as.matrix(out$ddist)
}
}
\seealso{
\code{\link{elbowx4TaStiC},
\link{Hierarchicalx4TaStiC}}
}
