#' Get pnl as xts series
#'
#' This method should be called only after backtest is done
#'
#' @param this modelStrategy
#'
#' @return xts zoo
#' 
#' @export
#' @rdname getPnL
getPnL <- function(this){
  UseMethod('getPnL', this)
}

#' @export
#' @rdname getPnL
#' @method getPnL modelStrategy
getPnL.modelStrategy <- function(this){
  range <- this$thisEnv$backtests[['base']]$activeField
  range <- range[1]:range[2]
  dates <- getDateByIndex(this, range)
  return(xts(this$thisEnv$backtests[['base']]$results$money[range,], dates))
}



#' @export
#' @rdname getPnL
#' @method getPnL modelPortfolio
getPnL.modelPortfolio <- function(this){
  range <- this$thisEnv$backtests[['base']]$activeField
  range <- range[1]:range[2]
  dates <- this$thisEnv$backtests[['base']]$results$dates[range]
  return(xts(this$thisEnv$backtests[['base']]$results$money[range,], dates))
}



#' @return xts zoo
#' @export
#' @rdname getPnL
#' @method getPnL list
getPnL.list <- function(this){
  lapply(this, getPnL.modelStrategy) 
}



