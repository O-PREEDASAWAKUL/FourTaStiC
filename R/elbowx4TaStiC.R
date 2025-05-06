elbowx4TaStiC <- function(data, dist.method, L, E, kmax,
                          kmin = 1, E.default = TRUE, Time = NULL, h.method = "complete", pp = 0.08,
                          alpha = NULL, C= 0, plot.control = list()) {

  if (missing(data))
    stop("Missing input argument. Please provide a numeric matrix, data frame, or time series object (`ts`).")
  if (missing(L)) {
    stop("Missing input: `L` must be a natural number indicating time steps to shift.")}
  if (missing(kmax)) {
    stop("Missing input: `kmax` must be a natural number indicating the maximum number of clusters for consideration.")}
  if (!is.character(dist.method) || length(dist.method) != 1) {
    stop("`dist.method` must be a character string.")
  }
  if (!dist.method %in% c("Time_eu.dist", "Timetrend_corr.dist", "FourTaStiC.dist")) {
    stop("Invalid `dist.method`. Choose from: 'Time_eu.dist', 'Timetrend_corr.dist', 'FourTaStiC.dist'.")
  }
  if (!h.method %in% c("complete", "average", "single")) {
    stop("Invalid `h.method`. Choose from: 'complete', 'average', 'complete'.")
  }
  if (missing(E)) {
    stop("Missing input: `E` must be a real number or vector of small tilt angles.")}
  if (!is.matrix(data) && !is.data.frame(data) && !inherits(data, "ts")) {
    stop("`data` must be a numeric matrix, data frame, or a time series object (`ts`).")
  }
  if (!is.positive_integer(L)) {
    stop("Argument 'L' must be a natural number indicating time step to be shifted.")
  }
  if (!is.positive_integer(kmax)) {
    stop("Argument 'kmax' must be a natural number.")
  }
  if (!is.vector(E)) {
    stop("Argument 'E' must be real number or a numeric vector.")
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

  # default parameter for plot
  default_control <- list(
    x_label = "Number of Clusters (K)",
    y_label = "Total Within-cluster Distance",
    title = "Elbow Method for Optimal K",
    point_size = 2,
    line_size = 0.7,
    color = "black"
  )
  # check control input
  invalid_keys = setdiff(names(plot.control), names(default_control))
  if (length(invalid_keys) > 0) {
    warning("The following elements in `plot.control` are not recognized and will be ignored: ",
            paste(invalid_keys, collapse = ", "))
  }
  y = numeric(kmax - kmin + 1)
  dist.dat = data

  if (dist.method %in% c("Time_eu.dist")) {
    dist.obj = do.call(dist.method, list(data = dist.dat, L = L, Time = Time))
  } else if (dist.method %in% c("Timetrend_corr.dist")) {
    dist.obj = do.call(dist.method, list(data = dist.dat, L = L, E = E, Time = Time, E.default = E.default, C = C))
  } else if (dist.method %in% c("FourTaStiC.dist")) {
    obj = do.call(dist.method, list(data = dist.dat, L = L, E = E ,Time = Time, E.default = E.default, pp = pp, C = C, alpha = alpha))
    dist.obj = obj$ddist
  }

  hc = hclust(dist.obj, method = h.method)

  # Iterate through possible cluster counts
  for (k in kmin:kmax) {
    clusters = cutree(hc, k = k)

    total_within_cluster_dist = 0

    # Compute within-cluster distances for each cluster
    for (i in unique(clusters)) {
      indat = dist.dat[clusters == i, , drop = FALSE]

      if (nrow(indat) > 1) {
        if (dist.method %in% c("Time_eu.dist")) {
          dist.in = do.call(dist.method, list(data = indat, L = L, Time = Time))
        } else if (dist.method %in% c("Timetrend_corr.dist")) {
          dist.in = do.call(dist.method, list(data = indat, L = L, E = E, Time = Time, E.default = E.default, C=C))
        } else if (dist.method %in% c("FourTaStiC.dist")) {
          obj = do.call(dist.method, list(data = indat, L = L, E = E ,Time = Time, E.default = E.default, pp = pp, C = C, alpha = alpha))
          dist.in = obj$ddist
        }
        total_within_cluster_dist = total_within_cluster_dist + sum(dist.in)
      }
    }

    y[k - kmin + 1] = total_within_cluster_dist
  }

  # Plot the elbow curve

  # Combine defaults with user-provided controls
  control = modifyList(default_control, plot.control)
  frame = data.frame("X" = kmin:kmax, "Y" = y)
  el.plot = ggplot(frame, aes(x = X, y = Y)) +
    geom_point(size = control$point_size, color = control$color) +
    geom_line(linewidth = control$line_size, color = control$color) +
    labs(x = control$x_label, y = control$y_label, title = control$title) +
    theme_minimal()
  return(list(elbow.frame = frame, elbow.plot = el.plot))
}



