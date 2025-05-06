DBSCANx4TaStiC <- function(data, L , E, eps, minPts, dist.method, E.default =TRUE, Time = NULL,
                       pp = 0.08, alpha =NULL, C=0){
  if (missing(data))
    stop("Missing input argument. Please provide a numeric matrix, data frame, or time series object (`ts`).")
  if (missing(L)) {
    stop("Missing input: L must be a natural number indicating time steps to shift.")
  }
  if (missing(E)) {
    stop("Missing input: E must be a real number or vector of small tilt angles.")}
  if (missing(eps)){
    stop("Missing input: eps must be a positive real number.")
  }
  if (missing(minPts)){
    stop("Missing input: minPts must be a positive integer.")
  }
  if (!is.positive_integer(L))
    stop("`L` must be a positive integer indicating the time shift.")
  if (!is.numeric(E))
    stop("`E` must be a numeric value or numeric vector.")
  if (!is.numeric(eps) || eps <= 0)
    stop("`eps` must be a positive numeric value.")
  if (!is.positive_integer(minPts))
    stop("`minPts` must be a positive integer.")
  if (!is.character(dist.method) || length(dist.method) != 1) {
    stop("`dist.method` must be a character string.")
  }
  if (!dist.method %in% c("Time_eu.dist", "Timetrend_corr.dist", "FourTaStiC.dist")) {
    stop("Invalid `dist.method`. Choose from: 'Time_eu.dist', 'Timetrend_corr.dist', 'FourTaStiC.dist'.")
  }
  if (!is.matrix(data) && !is.data.frame(data) && !inherits(data, "ts")) {
    stop("`data` must be a numeric matrix, data frame, or a time series object (`ts`).")
  }
  if (!is.logical(E.default)) {
    stop("`E.default` must be a logical value.")
  }
  if (!is.numeric(pp) || pp <= 0 || pp >= 1) {
    stop("`pp` must be a numeric percentile between 0 and 1.")
  }
  if (C < 0) {
    stop("`C` must be greater than or equal to zero.")
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
  dist.dat = data
  if (dist.method %in% c("Time_eu.dist")) {
    dist.obj = do.call(dist.method, list(data = dist.dat, L = L, Time = Time))
  } else if (dist.method %in% c("Timetrend_corr.dist")) {
    dist.obj = do.call(dist.method, list(data = dist.dat, L = L, E = E, Time = Time, E.default = E.default, C = C))
  } else if (dist.method %in% c("FourTaStiC.dist")) {
    obj = do.call(dist.method, list(data = dist.dat, L = L, E = E ,Time = Time, E.default = E.default, pp = pp, C = C, alpha = alpha))
    dist.obj = obj$ddist
  }
  d.result = dbscan(x = dist.obj, eps = eps , minPts = minPts)
  if (0 %in% d.result$cluster) {
    cclust = d.result$cluster + 1
  } else{
    cclust = d.result$cluster
  }
  clust.dat = cbind(data, label = cclust)
  result = list(clust.data = clust.dat, ddist = dist.obj)
  return(result)
}
