Time_eu.dist <- function(data, L, Time = NULL){
  if (missing(data)) {
    stop("Missing input argument. Please provide a numeric matrix, data frame, or time series object (`ts`).")
  }
  if (missing(L)) {
    stop("Missing input: `L` must be a natural number indicating time steps to shift.")
  }
  if (!is.matrix(data) && !is.data.frame(data) && !inherits(data, "ts")) {
    stop("`data` must be a numeric matrix, data frame, or a time series object (`ts`).")
  }
  if (!is.positive_integer(L)) {
    stop("Argument 'L' must be a natural number indicating time step to be shifted.")
  }
  if (inherits(data, c("ts", "data.frame"))) {
    if (is.null(Time)) {
      stop(paste0("'", class(data), "' input detected. Please provide the `Time` argument."))
    }
    if (!is.positive_integer(Time)) {
      stop("`Time` must be a natural number.")
    }
    # Convert ts or data frame to matrix form with Time columns
    data <- as.matrix(data, ncol = Time, byrow = TRUE)
  }
  # For setting tilt (E) parameter
  n.row = nrow(data)
  n.col = ncol(data)
  t.range = if (L == 0) 0 else seq_len(L)
  dist.mat = matrix(0,nrow =  n.row, ncol =  n.row)
  for (i in 1:(n.row-1)) {
    for (j in (i+1):n.row) {
      int.min = Inf
      for (t in t.range) {
        if (L == 0) {
          mmin = sqrt(sum((data[i,] - data[j,])^2) / n.col)
        } else {
          m.no = sqrt(sum((data[i,] - data[j,])^2) / n.col)
          m.left = sqrt(sum((data[i,1:(n.col-t)] - data[j,(t+1):n.col])^2)/(n.col-t))
          m.right  = sqrt(sum((data[i,((t+1):n.col)] - data[j,1:(n.col-t)])^2)/(n.col-t))
          mmin = min(m.right,m.left,m.no)
        }
        if(mmin<int.min){
          int.min = mmin
          dist.mat[i,j] = mmin
        }
      }
    }
  }
  return(as.dist(t(dist.mat)))
}
