\name{DBSCANx4TaStiC}
\alias{DBSCANx4TaStiC}
\title{Perform DBSCAN clustering using 4TaStiC-based distance or dissimilarity}
\description{
Computes a distance or dissimilarity matrix using one of the supported
time-series methods, applies DBSCAN clustering, and returns the cluster
assignments together with the distance or dissimilarity object.

This function is designed for use with the 4TaStiC framework, which combines
time traveling and trend traveling for time series clustering.
}
\usage{
DBSCANx4TaStiC(
  data,
  L,
  E,
  eps,
  minPts,
  dist.method,
  E.default = TRUE,
  Time = NULL,
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
    A non-negative integer giving the maximum number of time steps allowed for
    shifting.
  }

  \item{E}{
    A numeric value or numeric vector giving the allowed trend-tilting values.
    In the 4TaStiC framework, \code{E} should contain \code{0}.
  }

  \item{eps}{
    A positive numeric value specifying the neighborhood radius used in
    DBSCAN.
  }

  \item{minPts}{
    A positive integer specifying the minimum number of observations required
    to form a dense region in DBSCAN.
  }

  \item{dist.method}{
    A character string specifying the distance or dissimilarity method. Must
    be one of \code{"Time_eu_dist"}, \code{"Timetrend_corr_dist"}, or
    \code{"FourTaStiC_dist"}.
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

  \item{Time}{
    Optional positive integer giving the number of time points when
    \code{data} is supplied as a \code{data.frame} or \code{time series} object and
    needs reshaping into matrix form.
  }

  \item{pp}{
    Numeric percentile in \code{(0, 1)} used in the default rule for selecting
    \code{alpha} when \code{alpha = NULL}.
  }

  \item{alpha}{
    Optional numeric value in \code{[0, 1]} controlling the balance between
    the correlation-based and weighted Euclidean components. If \code{alpha}
    is \code{NULL}, the function uses the default rule from the underlying
    dissimilarity function. The distance is defined as
    \eqn{\alpha d_{\mathrm{corr}} + (1-\alpha) D_{\mathrm{weu}}}.
  }

  \item{C}{
    A non-negative numeric penalty coefficient for the trend-traveling
    correlation term. Larger values reduce the impact of large trend tilts.
  }
}
\details{
A distance or dissimilarity matrix is first computed from \code{data} using
the method specified by \code{dist.method}. DBSCAN clustering is then applied
to the resulting object using \code{\link[dbscan]{dbscan}}.

In the 4TaStiC framework, \code{L} is the maximum allowed time shift,
\code{E} is the set of allowed tilt values, \code{C >= 0} is a penalty
coefficient for excessive tilting in the correlation term, and
\code{alpha} controls the balance between the weighted Euclidean and
correlation-based components.

If DBSCAN assigns some observations to noise, the corresponding cluster label
\code{0} is shifted to \code{1} in the returned output so that all labels are
positive integers.
}
\value{
A list with two components:
\describe{
  \item{clust.data}{
    A matrix containing the input data with an additional final column
    named \code{label} giving the DBSCAN cluster assignment.
  }
  \item{ddist}{
    An object of class \code{"dist"} containing the pairwise distances or
    dissimilarities used in the clustering step.
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
library(dbscan)

x <- TSdata9[,-21]

\dontrun{
res <- DBSCANx4TaStiC(
  data = x,
  L = 2,
  E = c(-0.05, 0, 0.05),
  eps = 0.5,
  minPts = 2,
  dist.method = "FourTaStiC_dist",
  pp = 0.08,
  C = 0
)

res$clust.data
res$ddist
}
}
\seealso{
\code{\link[dbscan]{dbscan}},
\code{\link{FourTaStiC_dist}},
\code{\link{Timetrend_corr_dist}},
\code{\link{Time_eu_dist}}
}
