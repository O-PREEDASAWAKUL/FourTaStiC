#' Time series dataset 16
#'
#' A synthetic time series dataset containing 20 series, each with 65 time points.
#' The series are categorized into three classes and generated using sine wave-based
#' structures to simulate periodic (seasonal) behavior. Gaussian noise and random
#' variation were added to introduce variability.
#'
#' @format A matrix with 65 rows (time points) and 21 columns:
#' \describe{
#'   \item{V1--V20}{Numeric columns representing time series values.}
#'   \item{label}{A categorical variable indicating the class (1, 2, 3, 4, 5, or 6).}
#' }
#'
#' @references Preedasawakul, O. and Wiroonsri, N. (2025).\emph{4TaStiC: Time and trend traveling time series clustering for classifying long-term type 2 diabetes patients}.\emph{ACM Transactions on Computing for Healthcare}.
#'
#' @author
#' Onthada Preedasawakul and Nathakhun Wiroonsri
#'
#' @seealso
#' {\link{TSdata15}, \link{TSdata1}}
"TSdata16"
