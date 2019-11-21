# Lag.matrix <- function(x, k = 1){
#   if(k > nrow(x)){
#     stop("k > nrow(x)")
#   }else if (k != as.integer(k)){
#     stop("k must be a non-negative integer")
#   }else if(k < 0){
#     m.na <- matrix(NA, nrow = abs(k), ncol = ncol(x))
#     return(rbind(tail(x,k), m.na))
#   }else if (k == 0) {
#     return(x)
#   }else{
#     m.na <- matrix(NA, nrow = k, ncol = ncol(x))
#     return(rbind(m.na, head(x,-k)))
#   }
# }

# Lag.logical <- function(x, k = 1){
#   if(k > length(x)){
#     stop("k > nrow(x)")
#   }else if (k != as.integer(k)){
#     stop("k must be a non-negative integer")
#   }else if (k < 0){
#     m.na <- rep(NA, nrow = abs(k))
#     return(c(tail(x,k), m.na))
#   }else if (k == 0) {
#     return(x)
#   }else{
#     m.na <- rep(NA, nrow = k)
#     return(c(m.na, head(x,-k)))
#   }
# }
# 
# Diff <- function(x, ...){
#   UseMethod('Diff', x)
# }
# 
# Diff.default <- diff
# 
# Diff.matrix <- function(x, k = 1){
#   x - Lag(x, k)
# }


#' @param x matrix
#' @param k numeric, integer lag
#'
#' @export
#' @rdname Lag
Lag.matrix <- function(x, k = 1){
  if(k > nrow(x)){
    stop("k > nrow(x)")
  }else if (k != as.integer(k)){
    stop("k must be a non-negative integer")
  }else if(k < 0){
    m.na <- matrix(NA, nrow = abs(k), ncol = ncol(x))
    ret <- rbind(tail(x, k), m.na)
  }else if (k == 0) {
    return(x)
  }else{
    m.na <- matrix(NA, nrow = k, ncol = ncol(x))
    ret <- rbind(m.na, head(x,-k))
  }
  rownames(ret) <- NULL
  return(ret)
}

lag_fun <- function(x, k = 1){
  if(k > length(x)){
    stop("k > nrow(x)")
  }else if (k != as.integer(k)){
    stop("k must be a non-negative integer")
  }else if (k < 0){
    m.na <- rep(NA, abs(k))
    ret <- c(tail(x, k), m.na)
  }else if (k == 0) {
    return(x)
  }else{
    m.na <- rep(NA, k)
    ret <- c(m.na, head(x,-k))
  }
  return(ret)
}

#' @param x object
#' @param k numeric, integer lag
#'
#' @export
#' @rdname Lag
Lag.character <- function(x, k = 1){
  lag_fun(x, k)
}

#' @param x object
#' @param k numeric, integer lag
#'
#' @export
#' @rdname Lag
Lag.numeric <- function(x, k = 1){
  lag_fun(x, k)
}

#' @param x object
#' @param k numeric, integer lag
#'
#' @export
#' @rdname Lag
Lag.logical <- function(x, k = 1){
  lag_fun(x, k)
}

# environment(Lag.numeric) <- environment(Lag)
# 
# assignInNamespace(ns = 'quantmod', x = "Lag.numeric", value = Lag.numeric)



#' Work as function diff, but remains NA in the beginnig.
#'
#' @param x object
#' @param ... params
#'
#' @export
#' @rdname Diff
Diff <- function(x, ...){
  UseMethod('Diff', x)
}


#' @param k numeric, integer lag
#'
#' @export
#' @rdname Diff
Diff.default <- function(x, k = 1){
  x - Lag(x, k)
}

#' @export
#' @rdname Diff
Diff.matrix <- function(x, k = 1){
  x - Lag(x, k)
}


