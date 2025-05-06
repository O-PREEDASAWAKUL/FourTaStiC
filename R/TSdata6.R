#' Time series dataset 6
#'
#' A synthetic time series dataset containing 20 series, each with 90 time points.
#' The series are categorized into three classes and generated using sine wave-based
#' structures to simulate periodic (seasonal) behavior. Gaussian noise and random
#' variation were added to introduce variability.
#'
#' @format A matrix with 90 rows (time points) and 21 columns:
#' \describe{
#'   \item{V1--V20}{Numeric columns representing time series values.}
#'   \item{label}{A categorical variable indicating the class (1, 2,..., 8, or 9).}
#' }
#'
#' @references Onthada Preedasawakul and Nathakhun Wiroonsri, unpublished.
#'
#' @author
#' Onthada Preedasawakul and Nathakhun Wiroonsri
#'
#' @seealso
#' {\link{TSdata5}, \link{TSdata7}}
"TSdata6"
