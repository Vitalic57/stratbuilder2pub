#gets and sets commission 

#' Sets commission to model
#'
#' @param this modelStrategy
#' @param q quote, it should depend from pos_change argument, but it can include any variable from model
#' @export
#' @rdname setCommission
setCommission <- function(this, q){
  UseMethod('setCommission', this)
}

#' @export
#' @rdname setCommission
#' @method setCommission modelStrategy
setCommission.modelStrategy <- function(this, q){
    q <- rlang::enexpr(q)
    if(as.character(q[[1]]) == 'quote'){
        q <- eval(q)
    }
    this$thisEnv$commssion_quote <- q
}
