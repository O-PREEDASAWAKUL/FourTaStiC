#' @noRd
#' @keywords internal
.is.positive_integer <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && as.integer(x) == x && x >= 0
}
