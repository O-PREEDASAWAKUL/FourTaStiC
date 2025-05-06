Timetrend_corr.dist <- function(data, L, E, Time, E.default = TRUE, C=0){
  if (missing(data)) {
    stop("Missing input argument. Please provide a numeric matrix, data frame, or time series object (`ts`).")
  }
  if (missing(L)) {
    stop("Missing input: `L` must be a natural number indicating time steps to shift.")
  }
  if (missing(E)) {
    stop("Missing input: `E` must be a real number or vector of small tilt angles.")
  }
  if (!is.matrix(data) && !is.data.frame(data) && !inherits(data, "ts")) {
    stop("`data` must be a numeric matrix, data frame, or a time series object (`ts`).")
  }
  if (!is.positive_integer(L)) {
    stop("Argument 'L' must be a natural number indicating time step to be shifted.")
  }
  if (!is.vector(E)) {
    stop("Argument 'E' must be real number or a numeric vector.")
  }
  if (!is.logical(E.default)) {
    stop("`E.default` must be a logical value.")
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
  # For setting tilt (E) parameter
  ttilt = if (E.default) {
    as.vector(rbind(-E, E))
  } else {
    E
  }
  n.row = nrow(data)
  n.col = ncol(data)
  t.range = if (L == 0) 0 else seq_len(L)
  dist.mat = matrix(0,nrow = n.row, ncol = n.row)
  for (i in 1:(n.row-1)) {
    for (j in (i+1):n.row) {
      int.min = Inf
      for (t in t.range) {
        if (L == 0) {
          m.no = 1-cor(data[i,],data[j,])
          m.left = m.no
          m.right = m.no
        } else {
          m.no = 1-cor(data[i,],data[j,])
          m.left = 1 - cor(data[i,1:(n.col-t)],data[j,(t+1):n.col])
          m.right = 1 - cor(data[i,((t+1):n.col)],data[j,1:(n.col-t)])
        }
        if (all(E == 0)) {
          mmin = min(m.right,m.left,m.no)
          if(mmin<int.min){
            int.min = mmin
            dist.mat[i,j] = mmin
          }
        } else {
          for (m in 1:length(ttilt)) { #Add tilt
            tilt = ttilt[m]
            # Adjust only corr
            m.no.tilt = (1-cor(data[i,], (data[j,]+ tilt*(seq(n.col)-1))))
            # Add penalty term
            m.left.tilt = (1- (exp(-abs(tilt)*C)*cor(data[i,1:(n.col-t)],(data[j,(t+1):n.col]+ tilt*(seq(n.col-t)-1)))))
            m.right.tilt = (1- (exp(-abs(tilt)*C)*cor(data[i,((t+1):n.col)],(data[j,1:(n.col-t)]+ tilt*(seq(n.col-t)-1)))))
            mmin = min(m.right,m.left,m.no, m.no.tilt,m.right.tilt,m.left.tilt)
            if (mmin<int.min) {
              int.min = mmin
              dist.mat[i,j] = mmin
            }
          }
        }
      }
    }
  }
  return(as.dist(t(dist.mat)))
}
