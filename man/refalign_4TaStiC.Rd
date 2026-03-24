\name{refalign_4TaStiC}
\alias{refalign_4TaStiC}
\title{Prepare reference-aligned series for visualization under 4TaStiC}
\description{
Prepares aligned time series for visualization using the best alignment
information obtained from \code{\link{pairalign_4TaStiC}}.

The first observation is treated as the fixed reference series. Each
observation is then transformed according to its selected alignment
direction, optimal time shift, and optimal tilt value, and stored in
matrix form for direct visual comparison.
}
\usage{
refalign_4TaStiC(
  data,
  best.align
)
}
\arguments{
  \item{data}{
    A numeric matrix containing time series data. Rows are interpreted as
    observations and columns as time points.
  }

  \item{best.align}{
    A list containing the best alignment information, typically the output
    from \code{\link{pairalign_4TaStiC}}. It must contain the
    components \code{best.align}, \code{best.shift}, and \code{best.tilt}.
  }
}
\details{
The function fixes the first row of \code{data} as the reference
observation. For each observation, it extracts the alignment direction,
shift amount, and tilt amount from \code{best.align}, then applies the
corresponding transformation.

Depending on the selected direction, the series is kept unchanged,
left-shifted, or right-shifted. A tilt adjustment is then added to obtain
the trend-adjusted version of the aligned series.

This function is mainly intended for visualization, allowing the user to
inspect how each observation aligns with the fixed reference under the
best 4TaStiC matching.

Two matrices are returned:
\describe{
  \item{shift.mat}{
    A matrix containing the best time-shifted series relative to the fixed
    reference observation.
  }
  \item{shift_tilt.mat}{
    A matrix containing the best time-shifted and tilt-adjusted series
    relative to the fixed reference observation.
  }
}

When a shift is applied, padded positions are filled with \code{0} so that
all series retain the same length as the original data.
}
\value{
A list with two components:
\describe{
  \item{shift.mat}{
    A numeric matrix containing the aligned time-shifted series.
  }
  \item{shift_tilt.mat}{
    A numeric matrix containing the aligned time-shifted and tilt-adjusted
    series.
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
best <- pairalign_4TaStiC(
  data = x,
  L = 2,
  E = c(-0.05, 0, 0.05),
  pp = 0.08,
  C = 0
)

out <- refalign_4TaStiC(
  data = x,
  best.align = best
)

out$shift.mat
out$shift_tilt.mat
}
}
\seealso{
\code{\link{pairalign_4TaStiC}}
}
