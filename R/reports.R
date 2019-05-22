#' Report of strategy
#'
#' @param this modelStrategy
#'
#' @return
#' @export
getReportStrategy.modelStrategy <- function(this){
  return(this$thisEnv$backtests$base$reports$strategy %>% t %>% t %>% noquote)
}

#' Report of strategy
#'
#' @param l list of modelStrategies
#'
#' @return
#' @export
getReportStrategy.list <- function(l){
  lapply(l, function(x){
    getReportStrategy(x) 
  }) %>%
    Reduce('cbind', .) %>%
    {
      if(!is.null(names(l))){
        colnames(.) <- names(l)
      }
      .
    } %>%
    return
}

#' Report of strategy
#'
#' @param this modelPortfolio
#'
#' @return
#' @export
getReportStrategy.modelPortfolio <- function(this){
  getReportStrategy.modelStrategy(this)
}

#' Report of strategy year by year
#'
#' @param this modelStrategy
#'
#' @return
#' @export
getReportCalendar.modelStrategy <- function(this){
  return(this$thisEnv$backtests$base$reports$calendar)
}

#' Report of strategy year by year
#'
#' @param l list of modelStrategies
#'
#' @return
#' @export
getReportCalendar.list <- function(l){
  lapply(l, function(x){
    getReportCalendar(x)
  }) %>%
    set_names(names(l)) %>%
    return
}

#' Report of strategy year by year
#'
#' @param this modelPortfolio
#'
#' @return
#' @export
getReportCalendar.modelPortfolio <- function(this){
  return(this$thisEnv$backtests$base$reports$calendar)
}

#' Report of trades
#'
#' @param this modelStrategy
#'
#' @return
#' @export
getReportTrades.modelStrategy <- function(this){
  return(this$thisEnv$backtests$base$reports$trades)
}

#' Report of trades
#'
#' @param l list
#'
#' @return
#' @export
getReportTrades.list <- function(l){
  lapply(l, function(x){
    getReportTrades(x)
  }) %>%
    set_names(names(l)) %>%
    return
}

#' Report of trades
#'
#' @param this modelPortfolio
#'
#' @return
#' @export
getReportTrades.modelPortfolio <- function(this){
  return(this$thisEnv$backtests$base$reports$trades)
}


#' Results of backtest
#'
#' @param this modelStrategy
#' @param paramset.label character, name of paramset
#'
#' @return data.frame
#' @export
getBacktestResults.modelStrategy <- function(this, paramset.label){
  if(missing(paramset.label)){
    paramset.label <- 1
  }
  return(this$thisEnv$paramsets[[paramset.label]][['report']])
}


#' Results of backtest
#'
#' @param this modelStrategy
#' @param paramset.label character, name of paramset
#'
#' @return list of data.frames
#' @export
getBacktestResults.list <- function(l, ...){
  lapply(l, function(x){
    getBacktestResults(x, ...)
  }) %>%
    set_names(names(l))
}


#' Results of backtest
#'
#' @param this modelStrategy
#' @param paramset.label character, name of paramset
#'
#' @return list of data.frames
#' @export
getBacktestResults.modelPortfolio <- function(this, ...){
  getBacktestResults.list(this$thisEnv$models, ...)
}







