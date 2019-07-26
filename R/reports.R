#' Report of strategy
#'
#'
#' Returns model statistics, see details
#'
#' trades - Number of trade
#' 
#' trades.year - Average annual number of trades
#' 
#' long.trades - Number of long trade
#' 
#' short.trades - Number of short trade
#' 
#' long.acc - Long position accuracy
#' 
#' short.acc - Short position accuracy
#' 
#' total.acc - Position accuracy
#' 
#' max.loose - Max loose streak
#' 
#' max.win - Max win streak
#' 
#' return.ann - Average annual return
#' 
#' return.avg - Average annual return on capital employed
#' 
#' return.pos.drawdown  - PnL/(max(money in pos) + max.drawdow), where max.drawdow -> https://en.wikipedia.org/wiki/Drawdown_(economics)
#' 
#' drawdown.money - max.drawdown/money_in_strategy
#' 
#' median - median(profit_in_trade/money_in_strategy)
#' 
#' in.pos - time_in_position/ all_time
#' 
#' in.pos.positive  - (time_in_position where unrealized PnL > 0)/ time_in_position
#' 
#' days.in.pos.max - max number of days in position
#' 
#' days.in.pos.mean - mean number of days in position
#' 
#' days.out.pos.max - max number of days out of position
#' 
#' days.out.pos.mean - mean number of days out of position
#' 
#' sharpe.ann - sharpe coefficient https://en.wikipedia.org/wiki/Sharpe_ratio
#' 
#' sortino.ann - sortino coefficiet https://en.wikipedia.org/wiki/Sortino_ratio
#' 
#' maxMAE.money - Max absolute error in trade
#' 
#' profit.drawdown.year - PnL/max.drawdow
#' 
#' 
#' 
#' 
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
#' Returns model statistics, see details
#'
#' sharpe.ann - sharpe coefficient https://en.wikipedia.org/wiki/Sharpe_ratio
#'
#' return.pos.drawdown  - PnL/(max(money in pos) + max.drawdow), where max.drawdow -> https://en.wikipedia.org/wiki/Drawdown_(economics)
#'
#' return.avg - Average annual return on capital employed
#'
#' return.ann - Average annual return
#'
#' return.drawdown.year - PnL/max.drawdow
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
#' Returns model statistics, see details
#' 
#' ind.start - index start trade
#' 
#' index.send - index end trede
#' 
#' date.start - date start trade
#' 
#' date.end - date end trade
#' 
#' side - position 1 - long, -1 - short
#' 
#' beta - beta spread
#' 
#' price.start - price in the date.start
#' 
#' price.end - price in the date.end
#'
#' pnl.asset - pnl by asset
#'
#' com.asset - commission by asset
#'
#' com.sum - commissions on all assets
#'
#' pnl.sum - PnL on all assets
#'
#' MAE.with.com - max loss in position
#'
#' MFE.with.com - max win in position
#'
#' return.from.money - profit/(money_in_strategy)
#'
#'
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
#' Returns model statistics, see details
#'
#' trades - Number of trade
#' 
#' trades.year - Average annual number of trades
#' 
#' long.trades - Number of long trade
#' 
#' short.trades - Number of short trade
#' 
#' long.acc - Long position accuracy
#' 
#' short.acc - Short position accuracy
#' 
#' total.acc - Position accuracy
#' 
#' max.loose - Max loose streak
#' 
#' max.win - Max win streak
#' 
#' return.ann - Average annual return
#' 
#' return.avg - Average annual return on capital employed
#' 
#' return.pos.drawdown  - PnL/(max(money in pos) + max.drawdow), where max.drawdow -> https://en.wikipedia.org/wiki/Drawdown_(economics)
#' 
#' drawdown.money - max.drawdown/money_in_strategy
#' 
#' median - median(profit_in_trade/money_in_strategy)
#' 
#' in.pos - time_in_position/ all_time
#' 
#' in.pos.positive  - (time_in_position where unrealized PnL > 0)/ time_in_position
#' 
#' days.in.pos.max - max number of days in position
#' 
#' days.in.pos.mean - mean number of days in position
#' 
#' days.out.pos.max - max number of days out of position
#' 
#' days.out.pos.mean - mean number of days out of position
#' 
#' sharpe.ann - sharpe coefficient https://en.wikipedia.org/wiki/Sharpe_ratio
#' 
#' sortino.ann - sortino coefficiet https://en.wikipedia.org/wiki/Sortino_ratio
#' 
#' maxMAE.money - Max absolute error in trade
#' 
#' profit.drawdown.year - PnL/max.drawdow
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







