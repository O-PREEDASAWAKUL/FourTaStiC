#' Time series dataset 1
#'
#' A synthetic time series dataset containing 20 series, each with 30 time points.
#' The series are categorized into three classes and generated using sine wave-based
#' structures to simulate periodic (seasonal) behavior. Gaussian noise and random
#' variation were added to introduce variability.
#'
#' @format A matrix with 30 rows (time points) and 21 columns:
#' \describe{
#'   \item{V1--V20}{Numeric columns representing time series values.}
#'   \item{label}{A categorical variable indicating the class (1, 2, or 3).}
#' }
#'
#' @references Onthada Preedasawakul and Nathakhun Wiroonsri, unpublished.
#'
#' @author
#' Onthada Preedasawakul and Nathakhun Wiroonsri
#'
#' @seealso
#' {\link{TSdata2}, \link{TSdata3}}
"TSdata1"
