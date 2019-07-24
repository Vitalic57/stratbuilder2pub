
#' @export
#' @rdname getExpandingLookback
getExpandingLookback <- function(this){
  UseMethod('getExpandingLookback', this)
}

#' @export
#' @rdname setExpandingLookback
setExpandingLookback <- function(this, x){
  UseMethod('setExpandingLookback', this)
}

#' @export
#' @rdname setBetasByMoney
getBetasByMoney <- function(this){
  UseMethod('getBetasByMoney', this)
}


#' @export
#' @rdname setBetasByMoney
setBetasByMoney <- function(this, x, price){
  UseMethod('setBetasByMoney', this)
}







