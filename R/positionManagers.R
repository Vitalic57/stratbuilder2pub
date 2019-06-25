
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
#'
#' @export
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
  as <- gsub('\\.','_',as)
  e$positionManagers[[as]] <- list(
    as = as,
    increase = increase,
    decrease = decrease,
    rebalance = rebalance,
    change = change,
    close = close,
    oco = oco,
    args = args
  )
}







