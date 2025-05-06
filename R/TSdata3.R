#' Time series dataset 3
#'
#' A synthetic time series dataset containing 20 series, each with 75 time points.
#' The series are categorized into three classes and generated using sine wave-based
#' structures to simulate periodic (seasonal) behavior. Gaussian noise and random
#' variation were added to introduce variability.
#'
#' @format A matrix with 75 rows (time points) and 21 columns:
#' \describe{
#'   \item{V1--V20}{Numeric columns representing time series values.}
#'   \item{label}{A categorical variable indicating the class (1, 2, 3, ..., 6, or 7).}
#' }
#'
#' @references Onthada Preedasawakul and Nathakhun Wiroonsri, unpublished.
#'
#' @author
#' Onthada Preedasawakul and Nathakhun Wiroonsri
#'
#' @seealso
#' {\link{TSdata2}, \link{TSdata4}}
"TSdata3"
