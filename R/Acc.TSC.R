Acc.TSC <- function(data, group , label.names = "label"){
  if (missing(data))
    stop("Missing input argument. Please provide a numeric matrix, data frame.")
  if(!is.character(label.names))
    stop("Argument 'label.names' must be a character string indicating the true label column name")
  if(!(label.names %in% colnames(data)))
    stop(paste("Column", label.names , "is not in the data frame or matrix"))
  if (missing(group))
    stop("Missing input argument. Please provide a vector or the column that indicates the label in your data")
  if(length(group)!= nrow(data))
    stop("The length of 'group' must be equal to the number of rows in 'data'.")

  data[,label.names] = as.numeric(as.factor(data[,label.names])) #new order label
  k = length(unique(data[,label.names])) # True number of cluster
  u = order(table(data[,label.names]),decreasing = T) # Order labels
  result = data.frame() # For recording result

  cluster = group +100
  cluster2 = cluster
  wold = NULL
  for(i in 1:k){
    ttt = sort(table(cluster[data[,label.names]==u[i]]),decreasing = T)
    for (r in 1:length(ttt)){
      w = as.numeric(names(ttt[r]))
      if (!any(wold==w)){
        break
      }
    }
    cluster2[cluster2==w]=u[i]
    wold = c(wold,w)
  }
  ACC = mean(cluster2==data[,label.names])
  return(ACC)
}
