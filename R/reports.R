#' Report of strategy
#'
#' @param this modelStrategy
#'
#' @return noquote data.frame with report
#' @export
#' @rdname getReportStrategy
getReportStrategy <- function(this){
  UseMethod('getReportStrategy', this)
}

#' @export
#' @rdname getReportStrategy
#' @method getReportStrategy modelStrategy
getReportStrategy.modelStrategy <- function(this){
  return(this$thisEnv$backtests$base$reports$strategy %>% t %>% t %>% noquote)
}


#' @return data.frame
#' @export
#' @rdname getReportStrategy
#' @method getReportStrategy list
getReportStrategy.list <- function(this){
  lapply(this, function(x){
    getReportStrategy(x) 
  }) %>%
    Reduce('cbind', .) %>%
    {
      if(!is.null(names(this))){
        colnames(.) <- names(this)
      }
      .
    } %>%
    return
}


#' @export
#' @rdname getReportStrategy
#' @method getReportStrategy modelPortfolio
getReportStrategy.modelPortfolio <- function(this){
  getReportStrategy.modelStrategy(this)
}

#' Report of strategy year by year
#'
#' @param this modelStrategy
#'
#' @return
#' @export
#' @rdname getReportCalendar
getReportCalendar <- function(this){
  UseMethod('getReportCalendar', this)
}

#' @export
#' @rdname getReportCalendar
#' @method getReportCalendar modelStrategy
getReportCalendar.modelStrategy <- function(this){
  return(this$thisEnv$backtests$base$reports$calendar)
}


#' @return list of data.frames
#' @export
#' @rdname getReportCalendar
#' @method getReportCalendar list
getReportCalendar.list <- function(this){
  lapply(this, function(x){
    getReportCalendar(x)
  }) %>%
    set_names(names(this)) %>%
    return
}


#' @export
#' @rdname getReportCalendar
#' @method getReportCalendar modelPortfolio
getReportCalendar.modelPortfolio <- function(this){
  return(this$thisEnv$backtests$base$reports$calendar)
}

#' Report of trades
#'
#' @param this modelStrategy
#'
#' @return
#' @export
#' @rdname getReportTrades
getReportTrades <- function(this){
  UseMethod('getReportTrades', this)
}

#' @export
#' @rdname getReportTrades
#' @method getReportTrades modelStrategy
getReportTrades.modelStrategy <- function(this){
  return(this$thisEnv$backtests$base$reports$trades)
}


#' @return
#' @export
#' @rdname getReportTrades
#' @method getReportTrades list
getReportTrades.list <- function(this){
  lapply(this, function(x){
    getReportTrades(x)
  }) %>%
    set_names(names(this)) %>%
    return
}


#' @export
#' @rdname getReportTrades
#' @method getReportTrades modelPortfolio
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
#' @rdname getBacktestResults
getBacktestResults <- function(this, paramset.label){
  UseMethod('getBacktestResults', this)
}

#' @export
#' @rdname getBacktestResults
#' @method getBacktestResults modelStrategy
getBacktestResults.modelStrategy <- function(this, paramset.label){
  if(missing(paramset.label)){
    paramset.label <- 1
  }
  return(this$thisEnv$paramsets[[paramset.label]][['report']])
}



#' @param ... arguments to getBacktestResults.modelStrategy
#'
#' @return list of data.frames
#' @export
#' @rdname getBacktestResults
#' @method getBacktestResults list
getBacktestResults.list <- function(this, ...){
  lapply(this, function(x){
    getBacktestResults(x, ...)
  }) %>%
    set_names(names(this))
}



#' @return list of data.frames
#' @export
#' @rdname getBacktestResults
#' @method getBacktestResults modelPortfolio
getBacktestResults.modelPortfolio <- function(this, ...){
  getBacktestResults.list(this$thisEnv$models, ...)
}







