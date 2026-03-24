\name{cross.corr}
\alias{cross.corr}
\title{Compute within-cluster and between-cluster time-trend traveling correlations}
\description{
Computes average time-trend traveling correlations within and between
clusters of time series.

For each pair of clusters, the function evaluates the time-trend traveling
correlation between all corresponding pairs of time series and returns the
mean correlation. For pairs of time series within the same cluster, the
function also stores the individual pairwise correlation matrix.
}
\usage{
cross.corr(
  data,
  clust.result,
  L,
  E,
  E.default = TRUE
)
}
\arguments{
  \item{data}{
    A numeric matrix, data frame, or time series object. Rows are interpreted
    as observations and columns as time points after conversion.
  }

  \item{clust.result}{
    A vector indicating the cluster membership of each observation. Its length
    must be equal to the number of rows in \code{data}.
  }

  \item{L}{
    A non-negative integer giving the maximum number of time steps allowed for
    shifting.
  }

  \item{E}{
    A numeric value or numeric vector giving the allowed trend-tilting values.
    In the 4TaStiC framework, \code{E} should contain \code{0}.
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
}
\details{
Let the rows of \code{data} represent time series observations, and let
\code{clust.result} indicate their cluster memberships. For each pair of
clusters, the function computes the time-trend traveling correlation between
all relevant pairs of time series using \code{\link{Timetrend_corr}}, then
averages these correlations.

For a pair of distinct clusters, the returned value is the mean of all
between-cluster correlations. For a cluster paired with itself, the function
computes all within-cluster pairwise correlations and stores them in an
upper-triangular matrix.

This function is useful for summarizing similarity structures between
clusters of time series under the time-trend traveling correlation measure.
}
\value{
A list with two components:
\describe{
  \item{cross_corr.mat}{
    A symmetric matrix whose \eqn{(c_1,c_2)} entry is the mean time-trend
    traveling correlation between observations in cluster \eqn{c_1} and
    cluster \eqn{c_2}.
  }
  \item{cor.list}{
    A list of within-cluster upper-triangular correlation matrices. Each
    component corresponds to one cluster and contains the pairwise
    time-trend traveling correlations among observations within that cluster.
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
res <- Hierarchicalx4TaStiC(
  data = x,
  L = 2,
  dist.method = "FourTaStiC_dist",
  ccut = TSdata9[,21] |> n_distinct(),
  h.method = "complete",
  E = c(-0.05, 0, 0.05),
  pp = 0.08,
  C = 0
)


out <- cross.corr(
  data = x,
  clust.result = res$clust.data[,21],
  L = 2,
  E = c(-0.05, 0, 0.05),
  E.default = TRUE
)

out$cross_corr.mat
out$cor.list
}
}
\seealso{
\code{\link{Timetrend_corr}}
}
