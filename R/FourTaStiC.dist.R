FourTaStiC.dist <- function(data, L, E ,Time = NULL, E.default = TRUE, pp = 0.08, C = 0, alpha=NULL){
  if (missing(data))
    stop("Missing input argument. Please provide a numeric matrix, data frame, or time series object (`ts`).")
  if (missing(L)) {
    stop("Missing input: `L` must be a natural number indicating time steps to shift.")}
  if (missing(E)) {
    stop("Missing input: `E` must be a real number or vector of small tilt angles.")}
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
  # For setting tilt (E) parameter
  ttilt = if (E.default) {
    as.vector(rbind(-E, E))
  } else {
    E
  }
  n.row = nrow(data)
  n.col = ncol(data)
  t.range = if (L == 0) 0 else seq_len(L)
  if (is.null(n.row)) {
    dist.mat = 0
  } else{
    dist.mat = matrix(0,nrow = n.row, ncol = n.row)
    # select alpha
    if (is.numeric(alpha)) {
      if (alpha < 0 | alpha > 1) {
        stop("`alpha` must be a numeric between 0 and 1.")
      }
      alpha = alpha
    } else {
      alpha = select.alpha(data = data, pp = pp)
    }
    print(paste("Selected alpha:", alpha))
    rresult = data.frame()
    for (i in 1:(n.row - 1)){
      for (j in (i + 1):n.row) {
        # Tilt
        int.min = Inf
        for (t in t.range) {
          # No tilt
          if (L == 0) { # not shift
            m.no = (alpha*(1-cor(data[i,],data[j,]))) + ((1-alpha)*(sqrt(sum((data[i,] - data[j,])^2) / n.col)))
            m.left = m.no
            m.right = m.no
          } else{
            m.no = (alpha*(1-cor(data[i,],data[j,]))) + ((1-alpha)*(sqrt(sum((data[i,] - data[j,])^2) / n.col)))
            m.left = (alpha*(1- cor(data[i,1:(n.col-t)],data[j,(t+1):n.col]))) +
              ((1-alpha)*sqrt(sum((data[i,1:(n.col-t)] - data[j,(t+1):n.col])^2)/(n.col-t)))
            m.right = (alpha*(1- cor(data[i,((t+1):n.col)],data[j,1:(n.col-t)]))) +
              ((1-alpha)*sqrt(sum((data[i,((t+1):n.col)] - data[j,1:(n.col-t)])^2)/(n.col-t)))
          }
          if (all(E == 0)) {
            mmin = min(m.right,m.left,m.no)
            if (mmin < int.min) {
              int.min = mmin
              dist.mat[i,j] = mmin
            }
          } else{
            # For tilt
            for (tilt in ttilt) { #Add tilt
              m.no.tilt = (alpha*(1-cor(data[i,], (data[j,]+ tilt*(seq(n.col)-1))))) + ((1-alpha)*(sqrt(sum((data[i,] - data[j,])^2) / n.col)))
              m.left.tilt = (alpha*(1- (exp(-abs(tilt)*C)*cor(data[i,1:(n.col-t)],(data[j,(t+1):n.col]+ tilt*(seq(n.col-t)-1)))))) +
                ((1-alpha)*sqrt(sum((data[i,1:(n.col-t)] - data[j,(t+1):n.col])^2)/(n.col-t)))
              m.right.tilt = (alpha*(1- (exp(-abs(tilt)*C)*cor(data[i,((t+1):n.col)],(data[j,1:(n.col-t)]+ tilt*(seq(n.col-t)-1)))))) +
                ((1-alpha)*sqrt(sum((data[i,((t+1):n.col)] - data[j,1:(n.col-t)])^2)/(n.col-t)))
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
  }
  param = list(Alpha = alpha, ddist = as.dist(t(dist.mat)))
  return(param)
}
