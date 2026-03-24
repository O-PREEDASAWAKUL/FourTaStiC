\name{Accuracy}
\alias{Accuracy}
\title{Compute clustering accuracy}
\description{
Computes clustering accuracy by matching the predicted cluster assignments
to the true class labels and returning the proportion of correctly classified
observations.

This function is designed for evaluating clustering results when the true
labels are available. Since cluster labels are arbitrary, the function first
reorders the predicted cluster labels to best align with the true labels,
then computes the final accuracy.
}
\usage{
Accuracy(
  data,
  clust.result,
  label.names = "label"
)
}
\arguments{
  \item{data}{
    A numeric matrix or data frame containing the true labels in one column.
    Rows are interpreted as observations.
  }

  \item{clust.result}{
    A vector of predicted cluster assignments. Its length must be equal to
    the number of rows in \code{data}.
  }

  \item{label.names}{
    A character string indicating the name of the column in \code{data}
    containing the true labels. Default is \code{"label"}.
  }
}
\details{
Let the true labels be given in the column specified by \code{label.names},
and let \code{clust.result} be the predicted cluster memberships. Because
cluster labels are arbitrary, the function first relabels the predicted
clusters to match the true labels as closely as possible, based on the
dominant class within each cluster.

After relabeling, the clustering accuracy is computed as the proportion of
observations whose predicted label agrees with the true label.

This function assumes that the number of predicted clusters is compatible
with the number of true classes.
}
\value{
A numeric value giving the clustering accuracy, that is, the proportion of
correctly classified observations after matching cluster labels to the true
labels.
}

\references{
Preedasawakul, O. and Wiroonsri, N. (2026).
\emph{4TaStiC: Time and trend traveling time series clustering for
classifying long-term type 2 diabetes patients}.
\emph{ACM Transactions on Computing for Healthcare}.
}

\examples{
set.seed(1)

x <- data.frame(
  x1 = rnorm(6),
  x2 = rnorm(6),
  label = c("A", "A", "B", "B", "C", "C")
)

clust.result <- c(1, 1, 2, 2, 3, 3)

Accuracy(data = x, clust.result = clust.result, label.names = "label")
}

\seealso{
\code{\link{elbowx4TaStiC},
\link{Hierarchicalx4TaStiC},
\link{DBSCANx4TaStiC}}
}

