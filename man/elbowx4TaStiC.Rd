\name{elbowx4TaStiC}
\alias{elbowx4TaStiC}
\title{Elbow plot for selecting the number of clusters with 4TaStiC dissimilarity}
\description{
Computes the total within-cluster dissimilarity over a range of candidate
numbers of clusters and returns both the elbow data and a plot.

This function is designed for use with the 4TaStiC algorithm, which combines
time traveling and trend traveling for time series clustering. A dissimilarity
matrix is first computed using one of the supported distance methods, then
hierarchical clustering is applied, and the total within-cluster dissimilarity
is evaluated for each number of clusters.
}
\usage{
elbowx4TaStiC(
  data,
  dist.method,
  L,
  E,
  kmax,
  kmin = 1,
  E.default = TRUE,
  Time = NULL,
  h.method = "complete",
  pp = 0.08,
  alpha = NULL,
  C = 0,
  plot.control = list()
)
}
\arguments{
  \item{data}{
    A numeric matrix, data frame, or time series object. Rows are interpreted
    as observations and columns as time points after conversion.
  }

  \item{dist.method}{
    A character string specifying the dissimilarity method. Must be one of
    \code{"Time_eu_dist"}, \code{"Timetrend_corr_dist"}, or
    \code{"FourTaStiC_dist"}.
  }

  \item{L}{
    A non-negative integer giving the maximum number of time steps allowed for
    shifting.
  }

  \item{E}{
    A numeric value or numeric vector giving the allowed trend-tilting values.
    In the 4TaStiC framework, this should contain \code{0}.
  }

  \item{kmax}{
    A positive integer giving the maximum number of clusters to be considered.
  }

  \item{kmin}{
    A positive integer giving the minimum number of clusters to be considered.
    Default is \code{1}.
  }

  \item{E.default}{
    Logical. If \code{TRUE}, the function internally expands the supplied
    tilting values symmetrically by using both negative and positive values,
    including \code{0}. If \code{FALSE}, the values in \code{E} are used
    directly. For example, if \code{E = c(0.1, 0.2)} and
    \code{E.default = TRUE}, then the function uses
    \code{c(-0.2, -0.1, 0, 0.1, 0.2)} internally; if
    \code{E.default = FALSE}, it uses only \code{c(0.1, 0, 0.2)}.
  }

  \item{Time}{
    Optional positive integer giving the number of time points when
    \code{data} is supplied as a \code{data.frame} or \code{time series} object and
    needs reshaping into matrix form.
  }

  \item{h.method}{
  Linkage method passed to \code{\link[stats]{hclust}}. Must be one of
  \code{"complete"}, \code{"average"}, \code{"single"},
  \code{"ward.D"}, or \code{"ward.D2"}.
}

  \item{pp}{
    Numeric percentile in \code{(0, 1)} used in the default rule for selecting
    \code{alpha} when \code{alpha = NULL}.
  }

  \item{alpha}{
    Optional numeric value in \code{[0, 1]} controlling the balance between
    the correlation-based and Euclidean components. If \code{alpha} is
    \code{NULL}, the function uses the default rule from the underlying
    dissimilarity function. The distance is defined as
    \eqn{\alpha d_{\mathrm{corr}} + (1-\alpha) D_{\mathrm{Eu}}}.
  }

  \item{C}{
    A non-negative numeric penalty coefficient for the trend-traveling
    correlation term. Larger values reduce the impact of large trend tilts.
  }

  \item{plot.control}{
    A named list controlling the appearance of the elbow plot. Supported
    top-level entries are:
    \describe{
      \item{\code{labels}}{
        A list with optional elements \code{x}, \code{y}, and \code{title}.
      }
      \item{\code{point}}{
        A list of arguments passed to \code{\link[ggplot2]{geom_point}}.
      }
      \item{\code{line}}{
        A list of arguments passed to \code{\link[ggplot2]{geom_line}}.
      }
      \item{\code{theme}}{
        A ggplot2 theme object, such as \code{\link[ggplot2]{theme_minimal}}.
      }
    }
  }
}
\details{
For each candidate number of clusters, the function computes hierarchical
clusters from the selected dissimilarity object, cuts the dendrogram into
that many groups, recomputes within-cluster dissimilarities for each group,
and sums them to obtain the total within-cluster dissimilarity.

The elbow method can then be used to inspect where the reduction in
within-cluster dissimilarity begins to level off.

In the 4TaStiC framework, \code{L} is the maximum allowed time shift,
\code{E} is the set of allowed tilt values, \code{C >= 0} is a penalty
coefficient for excessive tilting in the correlation term, and
\code{alpha} controls the balance between the Euclidean and
correlation-based components.
}
\value{
A list with two components:
\describe{
  \item{elbow_data}{
    A data frame with columns \code{X} and \code{Y}, where \code{X} is the
    candidate number of clusters and \code{Y} is the total within-cluster
    dissimilarity.
  }
  \item{elbow_plot}{
    A \code{ggplot2} object showing the elbow curve.
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
res <- elbowx4TaStiC(
  data = x,
  dist.method = "FourTaStiC_dist",
  L = 2,
  E = c(-0.05, 0, 0.05),
  kmax = 5,
  h.method = "complete",
  pp = 0.08,
  C = 0
)

res$elbow_data
res$elbow_plot


## Customized elbow plot using plot.control
res2 <- elbowx4TaStiC(
  data = x,
  dist.method = "FourTaStiC_dist",
  L = 2,
  E = c(-0.05, 0, 0.05),
  kmax = 5,
  h.method = "complete",
  pp = 0.08,
  C = 0,
  plot.control = list(
    labels = list(
      x = "Number of clusters",
      y = "Within-cluster dissimilarity",
      title = "Elbow plot"
    ),
    point = list(size = 2),
    line = list(linewidth = 0.8),
    theme = theme_minimal()
  )
)

res2$elbow_plot
}

}

\seealso{
\code{\link[stats]{hclust}},
\code{\link[stats]{cutree}},
\code{\link[ggplot2]{ggplot}}
}
