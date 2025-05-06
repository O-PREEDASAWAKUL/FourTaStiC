Timetrend_corr <- function(x, y = NULL, L, E,Time = NULL, E.default = TRUE) {
  if (!is.positive_integer(L)) {
    stop("Argument 'L' must be a natural number indicating time step to be shifted.")
  }
  if (!is.vector(E)) {
    stop("Argument 'E' must be real number or a numeric vector.")
  }
  if (!is.logical(E.default)) {
    stop("`E.default` must be a logical value.")
  }

  ttilt = if (E.default) {
    as.vector(rbind(-E, E))
  } else {
    E
  }

  # Check if vector form
  if (!is.null(y)) {
    # Vector mode
    if (!is.numeric(x) || !is.numeric(y)) {
      stop("Both `x` and `y` must be numeric vectors.")
    }
    if (length(x) != length(y)) {
      stop("`x` and `y` must be the same length.")
    }

    n = length(x)
    int_max = 0

    t.range = if (L == 0) 0 else seq_len(L)

    for (t in  t.range) {
      if (L == 0) {
        m.no = cor(x, y)
        m.left = m.no
        m.right = m.no
      } else {
        m.no = cor(x, y)
        m.left = cor(x[1:(n - t)], y[(t + 1):n])
        m.right = cor(x[(t + 1):n], y[1:(n - t)])
      }
      if (all(E == 0)){
        mmax = max(m.no, m.right, m.left, na.rm = TRUE)
        if (mmax > int_max) {
          int_max = mmax
        }
      } else {
        for (tilt in ttilt) {
          m.no.tilt = cor(x, y + (tilt * seq_len(n)))
          m.left.tilt = cor(x[1:(n - t)], y[(t + 1):n] + (tilt * seq_len(n - t)))
          m.right.tilt = cor(x[(t + 1):n], y[1:(n - t)] + (tilt * seq_len(n - t)))
          mmax = max(m.no, m.right, m.left, m.no.tilt, m.right.tilt, m.left.tilt, na.rm = TRUE)
          if (mmax > int_max) {
            int_max = mmax
          }
        }
      }
    }

    return(int_max)

  } else {
    # Matrix mode
    if (missing(x))
      stop("Missing input argument. Please provide a numeric matrix, data frame, or time series object (`ts`).")
    if (!is.matrix(x) && !is.data.frame(x) && !inherits(x, "ts")) {
      stop("`x` must be a numeric matrix, data frame, or a time series object (`ts`).")
    }
    if (inherits(x, c("ts", "data.frame"))) {
      if (is.null(Time)) {
        stop(paste0("'", class(x), "' input detected. Please provide the `Time` argument."))
      }
      if (!is.positive_integer(Time)) {
        stop("`Time` must be a natural number.")
      }
      # Convert ts or data frame to matrix form with Time columns
      data = matrix(x, ncol = Time, byrow = TRUE)
    } else if (inherits(x, c("matrix", "array"))) {
      data = x
    }
    n.row = nrow(data)
    n.col = ncol(data)
    t.range = if (L == 0) 0 else seq_len(L)

    corr_mat = matrix(0, nrow = n.row, ncol = n.row)

    for (i in 1:(n.row - 1)) {
      for (j in (i + 1):n.row) {
        int_max = 0
        for (t in  t.range) {
          if (L == 0) {
            m.no = cor(data[i, ], data[j, ])
            m.left = m.no
            m.right = m.no
          } else {
            m.no = cor(data[i, ], data[j, ])
            m.left = cor(data[i, 1:(n.col - t)], data[j, (t + 1):n.col])
            m.right = cor(data[i, (t + 1):n.col], data[j, 1:(n.col - t)])
          }
          if (all(E == 0)) {
            mmax = max(m.no, m.right, m.left, na.rm = TRUE)
            if (mmax > int_max) {
              int_max = mmax
              corr_mat[i, j] = mmax
            }
          } else {
            for (tilt in ttilt) {
              m.no.tilt = cor(data[i, ], data[j, ] + (tilt * seq_len(n.col)))
              m.left.tilt = cor(data[i, 1:(n.col - t)], data[j, (t + 1):n.col] + (tilt * seq_len(n.col - t)))
              m.right.tilt = cor(data[i, (t + 1):n.col], data[j, 1:(n.col - t)] + (tilt * seq_len(n.col - t)))
              mmax = max(m.no, m.right, m.left, m.no.tilt, m.right.tilt, m.left.tilt, na.rm = TRUE)
              if (mmax > int_max) {
                int_max = mmax
                corr_mat[i, j] = mmax
              }
            }
          }
        }
      }
    }
    return(corr_mat)
  }
}

