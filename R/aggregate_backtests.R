aggregate_backtests <- function(this, saveTo = 'base'){
  money_xts <- NULL
  money_in_pos_xts <- NULL
  unrealized_money_xts <- NULL
  realized_money_xts <- NULL
  commissions_table_xts <- NULL
  for(model in this$thisEnv$models){
    e <- model$thisEnv$backtests[[saveTo]]
    start <- e$activeField['start']
    end <- e$activeField['end']
    dates <- getDateByIndex(model, start:end)
    money <- e$results$money[start:end,]
    money_in_pos <- e$results$money_in_pos[start:end,]
    unrealized_money <- e$results$unrealized_money[start:end,] %>% cbind
    realized_money <- e$results$realized_money[start:end,] %>% cbind
    commissions_table <- e$results$commissions_table[start:end,] %>% cbind
    
    if(is.null(money_xts)){
      money_xts <- xts(money, dates)
      money_in_pos_xts <- xts(money_in_pos, dates)
      unrealized_money_xts <- xts(apply(unrealized_money, 1, sum), dates)
      realized_money_xts <- xts(apply(realized_money, 1, sum), dates)
      commissions_table_xts <- xts(apply(commissions_table, 1, sum), dates)
    }else{
      money_xts <- cbind(money_xts, xts(money, dates))
      money_in_pos_xts <- cbind(money_in_pos_xts, xts(money_in_pos, dates))
      unrealized_money_xts <- cbind(unrealized_money_xts, 
                                    xts(apply(unrealized_money, 1, sum), dates))
      realized_money_xts <- cbind(realized_money_xts, 
                                  xts(apply(realized_money, 1, sum), dates))
      commissions_table_xts <- cbind(commissions_table_xts, 
                                     xts(apply(commissions_table, 1, sum), dates))
    }
    
  }
  
  e <- new.env()
  e$activeField <- c('start'=1, 'end'=nrow(money_xts))
  with(e, results <- list())
  e$results <- within(e$results,{
    dates <- index(money_xts)
    money <- money_xts %>% na.locf %>% na.locf(fromLast=TRUE) %>% apply(1, sum) %>% cbind
    money <- money - money[1] + getMoney(this)
    money_in_pos <- money_in_pos_xts %>% na.locf %>% na.locf(fromLast=TRUE) %>% apply(1, sum) %>% cbind 
    unrealized_money <- unrealized_money_xts %>% na.locf %>% na.locf(fromLast=TRUE) %>% apply(1, sum) %>% cbind 
    realized_money <- realized_money_xts %>% na.locf %>% na.locf(fromLast=TRUE) %>% apply(1, sum) %>% cbind 
    commissions_table <- commissions_table_xts %>% na.locf %>% na.locf(fromLast=TRUE) %>% apply(1, sum) %>% cbind 
  })
  this$thisEnv$backtests[[saveTo]] <- e
}

aggregate_prepared_models <- function(this, ...){
  dots <- list(...)
  if(length(this$thisEnv$backtests) == 0){
    nms <- 'base'
    if('from' %in% names(dots)){
      nms <- dots[['from']]
    }
    aggregate_backtests(this, saveTo = nms)
  }
}