#' Get pnl as xts series
#'
#' This method should be called only after backtest is done
#'
#' @param this modelStrategy
#'
#' @return xts zoo
#' @export
getPnL.modelStrategy <- function(this){
  range <- this$thisEnv$backtests[['base']]$activeField
  range <- range[1]:range[2]
  dates <- getDateByIndex(this, range)
  return(xts(this$thisEnv$backtests[['base']]$results$money[range,], dates))
}


#' Get pnl as xts series
#'
#' This method should be called only after backtest is done
#'
#' @param this modelPortfolio
#'
#' @return xts zoo
#' @export
getPnL.modelPortfolio <- function(this){
  range <- this$thisEnv$backtests[['base']]$activeField
  range <- range[1]:range[2]
  dates <- this$thisEnv$backtests[['base']]$results$dates[range]
  return(xts(this$thisEnv$backtests[['base']]$results$money[range,], dates))
}


#' Get pnl as list of xts series
#'
#' This method should be called only after backtest is done
#'
#' @param this modelPortfolio
#'
#' @return xts zoo
#' @export
getPnL.list <- function(l){
  lapply(l, getPnL.modelStrategy) 
}



