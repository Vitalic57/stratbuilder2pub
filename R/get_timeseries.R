#' Get pnl as xts series
#'
#' This method should be called only after backtest is done
#'
#' @param this modelStrategy
#' @param ... params
#' @return xts zoo
#' 
#' @export
#' @rdname getPnL
getPnL <- function(this, ...){
  UseMethod('getPnL', this)
}

#' @param start_date date type, example: start_date='2008-01-01'
#' @param end_date date type, example: end_date='2018-01-01'
#' @param comOn bool, if true then commission will be included in the 'trades' graph
#' @param leg numeric/character, number of leg, if it is equal to 'all' or 'sum', then all pnl among all legs
#' @export
#' @rdname getPnL
#' @method getPnL modelStrategy
getPnL.modelStrategy <- function(this,
                                 start_date = NULL,
                                 end_date = NULL,
                                 comOn = TRUE,
                                 leg = 'all', 
                                 ...){
  from <- 'base'
  e <- this$thisEnv$backtests[[from]]
  dates <- getDateByIndex(this)
  if (!is.null(start_date)){
    range_start <- max(e$activeField['start'],  sum(dates < start_date) + 1)
  }
  else{
    range_start <- e$activeField['start']
  }
  if(!is.null(end_date)){
    range_end <- min(e$activeField['end'], sum(dates < end_date))
  }
  else{
    range_end <- e$activeField['end']
  }
  if(range_start > range_end){
    stop("start > end")
  }
  range <- range_start:range_end
  init_money <- e$results$money[e$activeField['start'],]
  if(leg %in% c('all', 'sum')){
    df <- cbind(
      data.frame(date=dates[range]), 
      data.frame(PnL = (init_money + apply(e$results$unrealized_money + e$results$realized_money +  
                                             apply( (1 - comOn) * e$results$commissions_table, 2, cumsum), 1, sum)))[range,])
  }else if(is.numeric(leg)){ 
    df <- cbind(
      data.frame(date=dates[range]), 
      data.frame(PnL = init_money + e$results$unrealized_money[,leg] + e$results$realized_money[,leg] +
                   cumsum((1 - comOn) * e$results$commissions_table[, leg]))[range,])
  }else if(leg %in% c('sep', 'separate')){
    leg <- 'sep'
    df <- cbind( 
      data.frame(date=dates[range]), 
      data.frame(init_money + e$results$unrealized_money + e$results$realized_money +  
                   apply( (1 - comOn) * e$results$commissions_table, 2, cumsum)) %>%
        set_colnames(colnames(getModelD(this)$data_diff))[range,])
  }
  return(xts(df[,2], order.by = df[,1]))
}



#' @export
#' @rdname getPnL
#' @method getPnL modelPortfolio
getPnL.modelPortfolio <- function( this,
                                   ...)
{
    dots <- list(...)
    if('leg' %in% names(dots)){
      dots[['leg']] <- NULL
    }
    dots[['this']] <- this
    do.call("getPnL.modelStrategy", args=dots)
}



#' @return xts zoo
#' @export
#' @rdname getPnL
#' @method getPnL list
getPnL.list <- function(this, ...){
  dots <- list(...)
  if('leg' %in% names(dots)){
    dots[['leg']] <- NULL
  }
  df <- lapply(this, function(x){
    dots[['this']] <- x
    do.call('getPnL', args = dots)
    #plotPnL(x, return_type = 'data', ...)
  })
  data.frame(df)%>%set_colnames(1:length(this))
}

#' Return money in position
#' 
#' 
#' 
#' 
#' @param this modelStrategy
#' @param ... params
#' @export
#' @rdname getCapital
getCapital <- function(this, 
                        ...){
  UseMethod('getCapital', this)
}

#' @export
#' @param start_date date type, example: start_date='2008-01-01'
#' @param end_date date type, example: end_date='2018-01-01'
#' @rdname getCapital
#' @method getCapital modelStrategy
getCapital.modelStrategy <- function(this,
                                     start_date = NULL,
                                     end_date = NULL,
                                      ...){
  from <- 'base'
  e <- this$thisEnv$backtests[[from]]
  dates <- getDateByIndex(this)
  if (!is.null(start_date)){
    range_start <- max(e$activeField['start'],  sum(dates < start_date) + 1)
  }
  else{
    range_start <- e$activeField['start']
  }
  if(!is.null(end_date)){
    range_end <- min(e$activeField['end'], sum(dates < end_date))
  }
  else{
    range_end <- e$activeField['end']
  }
  if(range_start > range_end){
    stop("start > end")
  }
  range <- range_start:range_end
  xts(this$thisEnv$backtests$base$results$money_in_pos,dates)[range,]
}

#' @export
#' @rdname getCapital
#' @method getCapital modelPortfolio
getCapital.modelPortfolio <- function(this,
                                       ...){
  dots <- list(...)
  dots[['this']] <- this
  do.call("getCapital.modelStrategy", args=dots)
}
