Hierarchicalx4TaStiC  <- function(data, L, dist.method, ccut, h.method = "complete", E = 0, E.default =TRUE, Time = NULL,
                       pp = 0.08, alpha =NULL, C=0) {
  if (missing(data)) {
    stop("Missing input argument. Please provide a numeric matrix, data frame, or time series object.")
  }
  if (missing(L)) {
    stop("Missing input argument. Please provide `L`, the maximum number of time steps allowed for shifting.")
  }
  if (missing(dist.method)) {
    stop("Missing input argument. Please provide `dist.method`.")
  }
  if (missing(ccut)) {
    stop("Missing input argument. Please provide `ccut`, the number of clusters.")
  }
  if (!is.character(dist.method) || length(dist.method) != 1) {
    stop("`dist.method` must be a character string.")
  }
  if (!dist.method %in% c("Time_eu_dist", "Timetrend_corr_dist", "FourTaStiC_dist")) {
    stop("Invalid `dist.method`. Choose from: 'Time_eu_dist', 'Timetrend_corr_dist', or 'FourTaStiC_dist'.")
  }
  if (!h.method %in% c("complete", "average", "single", "ward.D", "ward.D2")) {
    stop("Invalid `h.method`. Choose from: 'complete', 'average', 'single', 'ward.D', or 'ward.D2'.")
  }
  if (!is.matrix(data) && !is.data.frame(data) && !inherits(data, "ts")) {
    stop("`data` must be a numeric matrix, data frame, or time series object.")
  }
  if (!.is.positive_integer(L)) {
    stop("Argument 'L' must be a natural number indicating the maximum time shift.")
  }
  if (!is.vector(E)) {
    stop("Argument 'E' must be a real number or a numeric vector.")
  }
  if (!is.logical(E.default)) {
    stop("`E.default` must be a logical value.")
  }
  if (!is.numeric(pp) || length(pp) != 1 || pp <= 0 || pp >= 1) {
    stop("`pp` must be a numeric percentile between 0 and 1.")
  }
  if (!is.null(alpha) && (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0 || alpha > 1)) {
    stop("`alpha` must be NULL or a numeric value in [0, 1].")
  }
  if (!is.numeric(C) || length(C) != 1 || C < 0) {
    stop("`C` must be a non-negative numeric value.")
  }
  if (!is.numeric(ccut) || length(ccut) != 1 || ccut <= 0) {
    stop("`ccut` must be a positive integer indicating the number of clusters.")
  }

  if (inherits(data, c("ts", "data.frame"))) {
    if (is.null(Time)) {
      stop(paste0("'", class(data), "' input detected. Please provide the `Time` argument."))
    }
    if (!.is.positive_integer(Time)) {
      stop("`Time` must be a natural number.")
    }
    # Convert ts or data frame to matrix form with Time columns
    data <- as.matrix(data, ncol = Time, byrow = TRUE) # 02/02/2026
  }
  dist.dat = data
  if (dist.method %in% c("Time_eu_dist")) {
    dist.obj = do.call(dist.method, list(data = dist.dat, L = L, Time = Time))
  } else if (dist.method %in% c("Timetrend_corr_dist")) {
    dist.obj = do.call(dist.method, list(data = dist.dat, L = L, E = E, Time = Time, E.default = E.default, C=C))
  } else if (dist.method %in% c("FourTaStiC_dist")) {
    obj = do.call(dist.method, list(data = dist.dat, L = L, E = E ,Time = Time, E.default = E.default, pp = pp, C = C, alpha = alpha))
    dist.obj = obj$ddist
  }
  # Clustering part
  hc = hclust(dist.obj,method = h.method)
  cut.group = cutree(hc, ccut)
  clust.dat = cbind(data, label = cut.group)
  if (dist.method %in% c("Timetrend_corr_dist","FourTaStiC_dist")) {
    out = list(clust.data = clust.dat, ddist = dist.obj)
  } else {
    out = list(clust.data = clust.dat , ddist = dist.obj)
  }
  return(out)
}
