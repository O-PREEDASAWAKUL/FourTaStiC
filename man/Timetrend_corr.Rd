\name{Timetrend_corr}
\alias{Timetrend_corr}
\title{Compute maximum time-trend traveling correlation}
\description{
Computes the maximum correlation between two time series, or for all pairs of
time series in a dataset, over a set of allowed time shifts and trend tilts.

This function is designed for use in the 4TaStiC framework. It searches over
candidate time-traveling and trend-traveling transformations and returns the
largest correlation obtained under the allowed alignments.
}
\usage{
Timetrend_corr(
  x,
  y = NULL,
  L,
  E,
  Time = NULL,
  E.default = TRUE
)
}
\arguments{
  \item{x}{
    A numeric vector, matrix, data frame, or time series object. If \code{y}
    is provided, \code{x} must be a numeric vector. Otherwise, rows are
    interpreted as observations and columns as time points after conversion.
  }

  \item{y}{
    Optional numeric vector. If supplied, the function computes the maximum
    time-trend traveling correlation between \code{x} and \code{y}. If
    \code{NULL}, the function computes the pairwise maximum correlations for
    all rows in \code{x}.
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
    \code{x} is supplied as a \code{data.frame} or time series object and
    needs reshaping into matrix form.
  }

  \item{E.default}{
    Logical. If \code{TRUE}, the function internally expands the supplied
    tilting values symmetrically by using both negative and positive values,
    including \code{0}. If \code{FALSE}, the values in \code{E} are used
    directly. For example, if \code{E = c(0.1, 0.2)} and
    \code{E.default = TRUE}, then the function uses
    \code{c(-0.1, 0.1, 0, -0.2, 0.2)} internally; if
    \code{E.default = FALSE}, it uses only \code{c(0.1, 0.2)}.
  }
}
\details{
Let \eqn{X} and \eqn{Y} be two time series. The function searches over
allowable time shifts \eqn{l \in \{0,1,\dots,L\}} and allowable trend tilts
\eqn{\epsilon \in E}, and returns the maximum correlation over those
candidate alignments.

When \code{y} is provided, the function returns a single numeric value giving
the maximum time-trend traveling correlation between \code{x} and \code{y}.
When \code{y = NULL}, the function treats the rows of \code{x} as a collection
of time series and returns an upper-triangular matrix containing the pairwise
maximum correlations.

This function is a building block for the trend-traveling correlation-based
component used in the 4TaStiC framework.
}
\value{
If \code{y} is provided, a numeric value giving the maximum time-trend
traveling correlation between \code{x} and \code{y}.

If \code{y = NULL}, a numeric matrix whose \eqn{(i,j)} entry contains the
maximum time-trend traveling correlation between row \eqn{i} and row
\eqn{j} of \code{x}, for \eqn{i < j}.
}
\references{
Preedasawakul, O. and Wiroonsri, N. (2026).
\emph{4TaStiC: Time and trend traveling time series clustering for
classifying long-term type 2 diabetes patients}.
\emph{ACM Transactions on Computing for Healthcare}.
}
\examples{
set.seed(1)

x <- rnorm(10)
y <- rnorm(10)

Timetrend_corr(
  x = x,
  y = y,
  L = 2,
  E = c(0, 0.05),
  E.default = TRUE
)

z <- matrix(rnorm(50), nrow = 5, ncol = 10)

Timetrend_corr(
  x = z,
  L = 2,
  E = c(0, 0.05),
  E.default = TRUE
)
}
\seealso{
\code{\link{Timetrend_corr_dist}},
\code{\link{FourTaStiC_dist}}
}
