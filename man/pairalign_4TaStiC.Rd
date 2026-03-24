\name{pairalign_4TaStiC}
\alias{pairalign_4TaStiC}
\title{Compute 4TaStiC dissimilarities and store the best pairwise alignment}
\description{
Computes the pairwise 4TaStiC dissimilarity for time series data and
returns the best alignment information for each pair of observations.

For every pair of time series, the function searches over all allowable
time shifts and trend tilts, then selects the alignment that gives the
smallest 4TaStiC dissimilarity. In addition to the dissimilarity matrix,
the function records the best alignment , the optimal shift value,
and the optimal tilt value for each pair.
}
\usage{
pairalign_4TaStiC(
  data,
  L,
  E,
  Time = NULL,
  E.default = TRUE,
  pp = 0.08,
  alpha = NULL,
  C = 0
)
}

\arguments{
  \item{data}{
    A numeric matrix, data frame, or time series object. Rows are interpreted
    as observations and columns as time points after conversion.
  }

  \item{L}{
    A non-negative integer giving the maximum number of time steps allowed
    for shifting.
  }

  \item{E}{
    A numeric value or numeric vector giving the allowable trend-tilting
    values. In the 4TaStiC framework, \code{E} should include \code{0}.
  }

  \item{Time}{
    Optional positive integer giving the number of time points when
    \code{data} is supplied as a \code{data.frame} or time series object and
    needs reshaping into matrix form.
  }

  \item{E.default}{
    Logical. If \code{TRUE}, the function expands the supplied tilt values
    symmetrically to include both negative and positive values, together
    with \code{0}. If \code{FALSE}, the values in \code{E} are used as
    provided. For example, if \code{E = c(0.1, 0.2)} and
    \code{E.default = TRUE}, then the function uses
    \code{c(-0.2, -0.1, 0, 0.1, 0.2)} internally; if
    \code{E.default = FALSE}, it uses only \code{c(0.1, 0.2)}.
  }

  \item{pp}{
    Numeric percentile in \code{(0, 1)} used in the default rule for
    selecting \code{alpha} when \code{alpha = NULL}.
  }

  \item{alpha}{
    Optional numeric value in \code{[0, 1]} controlling the balance between
    the correlation-based and weighted Euclidean components. If
    \code{alpha} is \code{NULL}, the function uses
    \code{select.alpha(data, pp)}.
  }

  \item{C}{
    A non-negative numeric penalty coefficient for the trend-traveling
    correlation term. Larger values reduce the impact of large trend tilts.
  }
}
\details{
Let \eqn{X_i} and \eqn{X_j} be two time series. For each pair, the
4TaStiC dissimilarity is obtained by evaluating candidate alignments over
all allowable time shifts \eqn{l \in \{0,1,\dots,L\}} and tilt values
\eqn{\epsilon \in E}, then selecting the combination that minimizes the
dissimilarity.

The dissimilarity combines two components:

\describe{
  \item{correlation-based dissimilarity}{
    A component based on the correlation between aligned series, with an
    optional penalty on large tilt values.
  }
  \item{weighted Euclidean dissimilarity}{
    A Euclidean-type component computed from the aligned and tilt-adjusted
    series.
  }
}

The parameter \code{alpha} controls the trade-off between these two
components. A larger \code{alpha} gives more weight to the
correlation-based component, while a smaller \code{alpha} gives more
weight to the weighted Euclidean component.

In addition to the pairwise dissimilarity matrix, the function stores,
for each pair of observations, the alignment direction, shift amount,
and tilt value that achieve the minimum dissimilarity. These outputs are
useful for interpreting the matching structure between time series and
for downstream visualization.
}
\value{
A list with five components:
\describe{
  \item{Alpha}{
    The value of the mixing parameter \code{alpha} used in the computation.
  }
  \item{ddist}{
    An object of class \code{"dist"} containing the pairwise 4TaStiC
    dissimilarities.
  }
  \item{best.align}{
    A character matrix recording the best alignment direction for each pair
    of time series. Typical values are \code{"none"}, \code{"left"}, and
    \code{"right"}.
  }
  \item{best.shift}{
    A numeric matrix recording the optimal shift value for each pair of
    time series.
  }
  \item{best.tilt}{
    A numeric matrix recording the optimal tilt value for each pair of
    time series.
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

out <- pairalign_4TaStiC(
  data = x,
  L = 2,
  E = c(-0.05, 0, 0.05),
  pp = 0.08,
  C = 0
)

out$Alpha
out$ddist
out$best.align
out$best.shift
out$best.tilt
}
}

\seealso{
\code{\link{FourTaStiC_dist}}
}
