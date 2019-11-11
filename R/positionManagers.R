
#' Creater for position manager.
#'
#' @param this modelStrategy
#' @param as character, name
#' @param increase list, the first argument is resposible for condition when this rule will work,
#' the second argument is expression that must calculate size of position(mult_delta)(how many spreads it needs to be bought).
#' In another words the last row must contain 'mult_delta <- ...'
#' @param decrease list, see increase, in expr quote beta_new must be defined
#' @param close list, see increase
#' @param oco charcter, name of namespace
#' @param rebalance list, see increase, in expr quote beta_new must be defined
#' @param change list, see increase, in expr quote mult_delta must be defined
#' @param args list, arguments of that manager
#' @export
#' @rdname addPM
addPM <- function(this,
                  as,
                  increase = list(cond = quote(FALSE), expr = quote({})),
                  decrease = list(cond = quote(FALSE), expr = quote({})),
                  rebalance = list(cond = quote(FALSE), expr = quote({})),
                  change = list(cond = quote(FALSE), expr = quote({})),
                  close = list(cond = quote(FALSE)),
                  oco = "base",
                  args = list()){
  UseMethod('addPM', this)
}

#' @export
#' @rdname addPM
#' @method addPM modelStrategy
addPM.modelStrategy <- function(this,
                                as,
                                increase = list(cond = quote(FALSE), expr = quote({})),
                                decrease = list(cond = quote(FALSE), expr = quote({})),
                                rebalance = list(cond = quote(FALSE), expr = quote({})),
                                change = list(cond = quote(FALSE), expr = quote({})),
                                close = list(cond = quote(FALSE)),
                                oco = "base",
                                args = list()
){
    e <- this$thisEnv
    if(missing(as)){
        as <- paste0('x', length(e$positionManagers) + 1)
    }
    as <- gsub('\\.','_',as)
    if(missing(oco)){
        oco <- as
    }
    e$positionManagers[[as]] <- list(
        as = as,
        increase = quote_list(rlang::enexpr(increase), parent.frame()),
        decrease = quote_list(rlang::enexpr(decrease), parent.frame()),
        rebalance = quote_list(rlang::enexpr(rebalance), parent.frame()),
        change = quote_list(rlang::enexpr(change), parent.frame()),
        close = quote_list(rlang::enexpr(close), parent.frame()),
        oco = oco,
        args = args
    )
    if(!is.null(rule_name)){
        e$positionManagers[[as]][['rule_name']] <- rule_name
    }
}

#' Return all position managers blocks
#' 
#' @param this model
#' 
#' @return list
#' @export
#' @rdname getPM
getPM <- function(this){
  UseMethod('getPM', this)
}


#' @export
#' @rdname getPM
#' @method getPM modelStrategy
getPM.modelStrategy <- function(this){
  return(this$thisEnv$positionManagers)
}





