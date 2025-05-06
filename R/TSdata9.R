#' Time series dataset 9
#'
#' A synthetic time series dataset containing 20 series, each with 40 time points.
#' The series are categorized into three classes and generated using sine wave-based
#' structures to simulate periodic (seasonal) behavior. Gaussian noise and random
#' variation were added to introduce variability.
#'
#' @format A matrix with 40 rows (time points) and 21 columns:
#' \describe{
#'   \item{V1--V20}{Numeric columns representing time series values.}
#'   \item{label}{A categorical variable indicating the class (1, 2, 3, or 4).}
#' }
#'
#' @references Onthada Preedasawakul and Nathakhun Wiroonsri, unpublished.
#'
#' @author
#' Onthada Preedasawakul and Nathakhun Wiroonsri
#'
#' @seealso
#' {\link{TSdata1}, \link{TSdata8}}
"TSdata9"
