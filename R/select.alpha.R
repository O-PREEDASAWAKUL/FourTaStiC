select.alpha <- function(data, Time = NULL, pp=0.08){
  if (missing(data))
    stop("Missing input argument. Please provide a numeric matrix, data frame, or time series object (`ts`).")
  if (!is.matrix(data) && !is.data.frame(data) && !inherits(data, "ts")) {
    stop("`data` must be a numeric matrix, data frame, or a time series object (`ts`).")
  }
  if (inherits(data, c("ts", "data.frame"))) {
    if (is.null(Time)) {
      stop(paste0("'", class(data), "' input detected. Please provide the `Time` argument."))
    }
    if (!is.positive_integer(Time)) {
      stop("`Time` must be a natural number.")
    }
    # Convert ts or data frame to matrix form with Time columns
    data <- matrix(data, ncol = Time, byrow = TRUE)
  }
  if (!is.numeric(pp) || pp <= 0 || pp >= 1) {
    stop("`pp` must be a numeric percentile between 0 and 1.")
  }
  al_eu.dist = dist(data,upper = TRUE) / ncol(data)
  corrdist = Timetrend_corr.dist(data = data, L = 0, E = 0, Time = Time, E.default = TRUE, C = 0) # correlation distance
  param  = (max(al_eu.dist))/(quantile(corrdist,probs = pp) + max(al_eu.dist))
  return(param)
}
