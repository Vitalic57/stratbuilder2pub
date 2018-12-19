Lag.matrix <- function(x, k = 1){
  if(k > nrow(x)){
    stop("k > nrow(x)")
  }else if (k < 0 || k != as.integer(k)){
    stop("k must be a non-negative integer")
  }else if (k == 0) {
    return(x)
  }else{
    m.na <- matrix(NA, nrow = k, ncol = ncol(x))
    return(rbind(m.na, head(x,-k)))
  }
}

Lag.logical <- function(x, k = 1){
  if(k > length(x)){
    stop("k > nrow(x)")
  }else if (k < 0 || k != as.integer(k)){
    stop("k must be a non-negative integer")
  }else if (k == 0) {
    return(x)
  }else{
    m.na <- rep(NA, nrow = k)
    return(c(m.na, head(x,-k)))
  }
}





