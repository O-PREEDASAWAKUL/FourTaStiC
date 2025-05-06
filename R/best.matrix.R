best.matrix <- function(data, best.align){
  n.sam = nrow(data)
  direction = best.align$best.align[1,]
  t.shift = best.align$best.shift[1,]
  degree.tilt = best.align$best.tilt[1,]
  shift.mat = matrix(0,n.sam,ncol(data))
  shift_tilt.mat = matrix(0,n.sam,ncol(data))

  # for loop
  for(i in 1:n.sam){
    which.d = direction[i]
    td = t.shift[i]
    ttilt = degree.tilt[i]
    if(which.d == "none"){ #Right
      plot_dat = data[i,]
      tilt_dat = data[i,] + (ttilt*(seq(length(plot_dat))))
    } else if(which.d == "right"){ #Left
      plot_dat = data[i,1:(ncol(data)-td)]
      tilt_dat = data[i,1:(ncol(data)-td)]  + (ttilt*(seq(length(plot_dat))))
    }else if(which.d == "left"){
      plot_dat = data[i,(td+1):(ncol(data))]
      tilt_dat = data[i,(td+1):(ncol(data))] + (ttilt*(seq(length(plot_dat))))
    }
    shift.mat[i,] = c(as.numeric(plot_dat),rep(0,td))
    shift_tilt.mat[i,] = c(as.numeric(tilt_dat),rep(0,td))
  }
  result = list(shift.mat = shift.mat, shift_tilt.mat = shift_tilt.mat)
  return(result)
}


