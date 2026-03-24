\name{plotalign_4TaStiC}
\alias{plotalign_4TaStiC}
\title{Plot 4TaStiC-aligned time series within each cluster}
\description{
Plots aligned time series within each cluster using the best 4TaStiC
pairwise alignment.

For each cluster, the function selects observations according to the chosen
plotting rule, computes the best pairwise alignment, prepares aligned series
relative to a fixed reference, and returns both aligned data and ggplot
objects for visualization.
}
\usage{
plotalign_4TaStiC(
  group.data,
  label.name,
  L,
  E,
  pp = 0.08,
  choose.plot = "all",
  n_sam = NULL,
  cor_list = NULL,
  cor_sam = NULL,
  Time = NULL,
  E.default = TRUE,
  alpha = NULL,
  C = 0
)
}
\arguments{
  \item{group.data}{
    A data frame or matrix containing the time series data together with a
    cluster label column. Each row is treated as one observation.
  }

  \item{label.name}{
    A character string giving the name of the column in \code{group.data}
    that contains the cluster labels.
  }

  \item{L}{
    A non-negative integer giving the maximum number of time steps allowed
    for shifting.
  }

  \item{E}{
    A numeric value or numeric vector giving the allowable trend-tilting
    values. In the 4TaStiC framework, \code{E} should include \code{0}.
  }

  \item{pp}{
    Numeric percentile in \code{(0, 1)} used in the default rule for
    selecting \code{alpha} when \code{alpha = NULL}.
  }

  \item{choose.plot}{
    A character string specifying which observations to visualize within
    each cluster. Must be one of \code{"all"}, \code{"sample"}, or
    \code{"cor.max"}.
  }

  \item{n_sam}{
    Optional positive integer giving the number of observations to sample
    within each cluster when \code{choose.plot = "sample"}.
  }

  \item{cor_list}{
    Optional list of within-cluster correlation matrices, typically from
    \code{cross.corr(...)}\code{$cor.list}. This is required when
    \code{choose.plot = "cor.max"}.
  }

  \item{cor_sam}{
    Optional positive integer giving the number of observations to retain
    within each cluster when \code{choose.plot = "cor.max"}.
  }

  \item{Time}{
    Optional positive integer giving the number of time points when the input
    needs reshaping into matrix form.
  }

  \item{E.default}{
    Logical. If \code{TRUE}, the function internally expands the supplied
    tilting values symmetrically to include both negative and positive
    values together with \code{0}. If \code{FALSE}, the values in
    \code{E} are used as provided.
  }

  \item{alpha}{
    Optional numeric value in \code{[0, 1]} controlling the balance between
    the correlation-based and weighted Euclidean components. If
    \code{alpha} is \code{NULL}, the default rule from the underlying
    dissimilarity function is used.
  }

  \item{C}{
    A non-negative numeric penalty coefficient for the trend-traveling
    correlation term. Larger values reduce the impact of large trend tilts.
  }
}
\details{
For each cluster, the function first selects observations according to
\code{choose.plot}. It then computes the best pairwise 4TaStiC alignment
within that cluster using \code{\link{pairalign_4TaStiC}} and constructs
aligned series using \code{\link{refalign_4TaStiC}}.

The first selected observation in each cluster is treated as the fixed
reference series. The remaining observations are aligned to this reference
using their best direction, shift value, and tilt value.

Two plots are produced for each cluster:
\describe{
  \item{plot_time}{
    A plot of the best time-shifted series.
  }
  \item{plot_timetrend}{
    A plot of the best time-shifted and tilt-adjusted series.
  }
}

If a cluster contains only one observation, the original time series is used
directly for plotting without alignment.

The function also returns an overall mean plot based on the time-shifted
series from each cluster.
}
\value{
A list whose components correspond to clusters, together with an overall
mean plot.

For each cluster, the corresponding list contains:
\describe{
  \item{best_result}{
    The output from \code{\link{pairalign_4TaStiC}}. If the cluster
    contains only one observation, this component is \code{NULL}.
  }
  \item{best_shift.data}{
    A matrix containing the best time-shifted series used in the time plot.
  }
  \item{best_shift_trend_data}{
    A matrix containing the best time-shifted and tilt-adjusted series used
    in the time-trend plot.
  }
  \item{plot_time}{
    A \code{ggplot2} object showing the time-shifted series.
  }
  \item{plot_timetrend}{
    A \code{ggplot2} object showing the time-shifted and tilt-adjusted
    series.
  }
}

In addition, the component \code{Overall_mean} is a \code{ggplot2} object
showing the mean time-shifted profile for each cluster.
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

out <- plotalign_4TaStiC(
  group.data = res$clust.data,
  label.name = "label",
  L = 2,
  E = c(-0.05, 0, 0.05),
  choose.plot = "all",
  pp = 0.08,
  C = 0
)

out$group_1$plot_time
out$group_1$plot_timetrend
out$Overall_mean
}
}
\seealso{
\code{\link{pairalign_4TaStiC}},
\code{\link{refalign_4TaStiC}},
\code{\link{cross.corr}},
\code{\link{Hierarchicalx4TaStiC}}
}
