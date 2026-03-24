\name{select_alpha}
\alias{select_alpha}
\title{Select the mixing parameter for 4TaStiC}
\description{
Selects the mixing parameter \code{alpha} used in the 4TaStiC dissimilarity.

The selected value is based on the scale of the weighted Euclidean distance
and a lower quantile of the correlation-based dissimilarity, so that the
weighted Euclidean component does not dominate when the data scale is large.
}
\usage{
select_alpha(
  data,
  Time = NULL,
  pp = 0.08
)
}
\arguments{
  \item{data}{
    A numeric matrix, data frame, or time series object. Rows are interpreted
    as observations and columns as time points after conversion.
  }

  \item{Time}{
    Optional positive integer giving the number of time points when
    \code{data} is supplied as a \code{data.frame} or \code{time series} object and
    needs reshaping into matrix form.
  }

  \item{pp}{
    Numeric percentile in \code{(0, 1)} used to compute a lower quantile of
    the correlation-based dissimilarity in the default rule for selecting
    \code{alpha}.
  }
}
\details{
Let \eqn{D_{\mathrm{Eu}}} denote the Euclidean distance and let
\eqn{d_{\mathrm{corr}}} denote the correlation-based dissimilarity. This
function selects \code{alpha} using the rule

\deqn{
\alpha = \frac{\max(D_{\mathrm{Eu}})}
{\operatorname{Q}_{pp}(d_{\mathrm{corr}}) + \max(D_{\mathrm{Eu}})}
}

where \eqn{\operatorname{Q}_{pp}(d_{\mathrm{corr}})} is the
\code{pp}-th quantile of the correlation-based dissimilarity computed with
\code{L = 0}, \code{E = 0}, and \code{C = 0}.

This rule is intended to balance the correlation-based and weighted
Euclidean components in the 4TaStiC dissimilarity.
}
\value{
A numeric value in \code{(0, 1)} giving the selected mixing parameter
\code{alpha}.
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

select_alpha(
  data = x,
  pp = 0.08
)
}
\seealso{
\code{\link{FourTaStiC_dist}},
\code{\link{Timetrend_corr_dist}}
}
