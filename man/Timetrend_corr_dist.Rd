\name{Timetrend_corr_dist}
\alias{Timetrend_corr_dist}
\title{Compute the time-trend traveling correlation dissimilarity matrix}
\description{
Computes a pairwise dissimilarity matrix for time series data based on
time traveling and trend traveling using a correlation-based criterion.

For each pair of time series, the function searches over allowed time shifts
and trend tilts, then takes the minimum dissimilarity over those candidate
alignments. A penalty term can be applied to reduce the effect of large
trend tilts.
}
\usage{
Timetrend_corr_dist(
  data,
  L,
  E,
  Time,
  E.default = TRUE,
  C = 0
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
    In the 4TaStiC framework, \code{E} should contain \code{0}.
  }

  \item{Time}{
    Optional positive integer giving the number of time points when
    \code{data} is supplied as a \code{data.frame} or time series object and
    needs reshaping into matrix form.
  }

  \item{E.default}{
    Logical. If \code{TRUE}, the function internally expands the supplied
    tilting values symmetrically by using both negative and positive values,
    including \code{0}. If \code{FALSE}, the values in \code{E} are used
    directly. For example, if \code{E = c(0.1, 0.2)} and
    \code{E.default = TRUE}, then the function uses
    \code{c(-0.2, -0.1, 0, 0.1, 0.2)} internally; if
    \code{E.default = FALSE}, it uses only \code{c(0.1, 0.2)}.
  }

  \item{C}{
    A non-negative numeric penalty coefficient for the trend-traveling
    correlation term. Larger values reduce the impact of large trend tilts.
  }
}
\details{
Let \eqn{X} and \eqn{Y} be two time series. The time-trend traveling
correlation dissimilarity is defined by minimizing a correlation-based
dissimilarity over allowable time shifts \eqn{l \in \{0,1,\dots,L\}} and
allowable trend tilts \eqn{\epsilon \in E}.

The base dissimilarity is given by a correlation-based term of the form
\eqn{1 - \exp(-C |\epsilon|)\mathrm{Corr}(\cdot,\cdot)}
when trend traveling is applied. When no trend tilt is used, the function
reduces to a time-traveling correlation dissimilarity based on
\eqn{1 - \mathrm{Corr}(\cdot,\cdot)}.

The penalty parameter \code{C >= 0} is intended to reduce the chance of
selecting excessively large tilts.
}
\value{
An object of class \code{"dist"} containing the pairwise time-trend
traveling correlation dissimilarities.
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
res <- Timetrend_corr_dist(
  data = x,
  L = 2,
  E = c(-0.05, 0, 0.05),
  Time = NULL,
  E.default = TRUE,
  C = 0
)

res
}
}
\seealso{
\code{\link{Timetrend_corr}},
\code{\link{FourTaStiC_dist}}
}
