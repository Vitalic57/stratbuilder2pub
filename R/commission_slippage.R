#gets and sets commission 

#' Sets commission to model
#'
#' @param this modelStrategy
#' @param q quote, it should depend from pos_change argument, but it can include any variable from model
#' @export
setCommission <- function(this, q){
  UseMethod('setCommission', this)
}

#' @export
setCommission.modelStrategy <- function(this, q){
  if(is.language(q)){
    this$thisEnv$commssion_quote <- q
  }else{
    warning('q must be quote')
  }
}

