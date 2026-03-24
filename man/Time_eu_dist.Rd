\name{Time_eu_dist}
\alias{Time_eu_dist}
\title{Compute the time-traveling weighted Euclidean distance matrix}
\description{
Computes a pairwise distance matrix for time series data based on
time traveling using a weighted Euclidean criterion.

For each pair of time series, the function searches over allowed time shifts
and takes the minimum weighted Euclidean distance over those candidate
alignments.
}
\usage{
Time_eu_dist(
  data,
  L,
  Time = NULL
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

  \item{Time}{
    Optional positive integer giving the number of time points when
    \code{data} is supplied as a \code{data.frame} or time series object and
    needs reshaping into matrix form.
  }
}
\details{
Let \eqn{X} and \eqn{Y} be two time series. The time-traveling weighted
Euclidean distance is defined by minimizing a weighted Euclidean distance
over allowable time shifts \eqn{l \in \{0,1,\dots,L\}}.

For each pair of time series, the function compares the original alignment
together with left-shifted and right-shifted alignments, then returns the
smallest resulting distance.

This function provides the weighted Euclidean component used in the
4TaStiC framework.
}
\value{
An object of class \code{"dist"} containing the pairwise time-traveling
weighted Euclidean distances.
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
res <- Time_eu_dist(
  data = x,
  L = 2
)

res
}
}
\seealso{
\code{\link{Timetrend_corr_dist}},
\code{\link{FourTaStiC_dist}}
}
