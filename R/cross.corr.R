# Cross cor matrix
cross.corr <- function(data, group, L, E, E.default = TRUE) {
  if (missing(data))
    stop("Missing input argument. Please provide a numeric matrix, data frame, or time series object (`ts`).")
  if (missing(group))
    stop("Missing input argument. Please provide a vector or the column that indicates the label in your data")
  if (missing(L)) {
    stop("Missing input: `L` must be a natural number indicating time steps to shift.")}
  if (missing(E)) {
    stop("Missing input: `E` must be a single real number or vector of small tilt angles.")}
  if (!is.logical(E.default)) {
    stop("`E.default` must be a logical value.")
  }
  group = as.numeric(as.factor(group))
  groups = levels(as.factor(group))
  G = length(groups)
  corr_matrix = matrix(NA, nrow = G, ncol = G)
  rownames(corr_matrix) <- colnames(corr_matrix) <- groups
  corr.list = list() # collect correlation matrix in each group
  for (g1 in 1:G) {
    for (g2 in g1:G) {
      idx1 = which(group == groups[g1])
      idx2 = which(group == groups[g2])
      cors = c()
      if (g1 == g2) {
        corr.list[[g1]] = matrix(0,nrow = sum(group == groups[g1]), ncol = sum(group == groups[g1]))
        # For mapping the data into the matrix
        index_map = setNames(seq_along(idx1), idx1)
        pairs = combn(idx1, 2)
        cors = apply(pairs, 2, function(idx) {
          val = Timetrend_corr(x = data[idx[1], ] |> as.numeric(), y = data[idx[2], ] |> as.numeric(), L = L , E = E, E.default = E.default)
          i = index_map[as.character(idx[1])]
          j = index_map[as.character(idx[2])]
          corr.list[[g1]][i,j] <<- val
        })
        names(corr.list)[g1] = paste0("g",g1)
      } else {
        for (i in idx1) {
          for (j in idx2) {
            cors = c(cors, Timetrend_corr(x = data[i, ] |> as.numeric(), y= data[j, ] |> as.numeric(), L = L, E = E , E.default = E.default))
          }
        }
      }
      mval = mean(cors, na.rm = TRUE)
      corr_matrix[g1, g2] = mval
      corr_matrix[g2, g1] = mval
    }
  }
  out = list(cross_corr.mat = corr_matrix, cor.list = corr.list)
  return(out)
}
