
#' @export
#' @rdname plotPnL
plotPnL <- function(this,...){
    UseMethod('plotPnL', this)
}

#' Plots change of pnl through backtest period
#'
#' @param this modelStrategy
#' @param type character, one of c('money','trades','percents')
#' @param from character, name of folder in backtests
#' @param leg numeric/character, number or numbers of legs, if it is equal to 'all' or 'sum', then all pnl among all legs
#' will be summed, if it is equal to 'sep', then pnl among legs will be plotted
#' @param graph_type character, ggplot2 or xts 
#' @param each_year logical, if TRUE, then each graph will start with 0 each year
#' @param adjust logical, if TRUE, then values will be divided by getMoney(this)
#' @param ... params
#' @param comOn bool, if true then commission will be included in the 'trades' graph
#' @param return_type character, plot or data
#' @param cutoff logical, if true then vertical line will plotted on graph where model was created 
#'
#' @export
#' @rdname plotPnL
#' @method plotPnL modelStrategy
plotPnL.modelStrategy <- function(this, 
                                  type = 'money',
                                  comOn = TRUE, 
                                  leg = 'all', 
                                  graph_type = 'ggplot2',
                                  each_year = FALSE,
                                  adjust = FALSE,
                                  return_type = 'plot',
                                  cutoff = FALSE,
                                  ...){
  from = 'base'
  e <- this$thisEnv$backtests[[from]]
  legs <- leg
  leg <- legs[1]
  switch(type,
         money = {
           dates <- getDateByIndex(this)
           range_start <- e$activeField['start']
           range_end <- e$activeField['end']
           if(range_start > range_end){
             stop("start > end")
           }
           range <- range_start:range_end
           init_money <- e$results$money[e$activeField['start'],]
           if(leg %in% c('all', 'sum')){
             df <- cbind( 
               data.frame(date=dates), 
               data.frame(PnL = init_money + apply(e$results$unrealized_money + e$results$realized_money +  
                                                     apply( (1 - comOn) * e$results$commissions_table, 2, cumsum), 1, sum))
             )[range,]
           }else if(is.numeric(leg)){ 
             leg <- legs
             df <- cbind(
               data.frame(date=dates), 
               data.frame(PnL = init_money + e$results$unrealized_money[,leg] + e$results$realized_money[,leg] +
                            cumsum((1 - comOn) * e$results$commissions_table[, leg]))
             )[range,]
           }else if(leg %in% c('sep', 'separate')){
             leg <- 'sep'
             df <- cbind( 
               data.frame(date=dates), 
               data.frame(init_money + e$results$unrealized_money + e$results$realized_money +  
                            apply( (1 - comOn) * e$results$commissions_table, 2, cumsum)) %>%
                 set_colnames(colnames(getModelD(this)$data_raw))
             )[range,]
           }
           if(adjust){
             df[,-1] <- df[,-1] / init_money - 1
           }
           if(each_year){
             if(!adjust){
               df[,-1] <- df[,-1] - init_money
             }
             
             tmp <- xts(df[,-1], df[,1]) %>% set_colnames(colnames(df)[-1])
             
             #last_dates <- apply.yearly(tmp, FUN = nrow ) %>% as.numeric %>% cumsum
             last_dates <- apply.yearly(tmp, FUN = function(x) tail(x, 1) %>% index ) %>% as.numeric %>% head(-1)
             
             df <- apply.yearly(tmp, FUN = function(x) sweep(x, 2, x[1,]) ) %>% 
             {
               res <- list()
               for(i in 1:length(.)){
                 res[[i]] <- .[[i]]
               }
               res
             } %>%
               Reduce('rbind', .) %>%
               coredata %>%
               as.data.frame %>%
               set_colnames(colnames(tmp)) %>%
               dplyr::mutate(date = df[, 'date'])
           }
           if(return_type == 'plot'){
             if(graph_type == 'ggplot2'){
               newdf <- reshape2::melt(df, 'date')
               p <- ggplot(newdf,aes(x=date, y=value, color = variable) ) +
                 geom_line() + theme_bw() + ggtitle("PnL money by date")
               if(each_year){
                 p <- p + geom_vline(xintercept=last_dates, linetype=4, colour="red")
               }
               if(cutoff && 'created' %in% names(this$thisEnv)){
                 p <- p + geom_vline(xintercept=as.numeric(this$thisEnv$created), linetype=4, colour="green")
               }
               if(leg != 'sep'){
                 p + scale_color_manual(
                   values = c(
                     PnL = 'darkblue'
                   )) + theme(legend.position="none")
               }else{
                 p 
               }
               
               
             }else{
               ind <- which(colnames(df) == 'date')
               plot(xts(df[,-ind], df[, ind]), format.labels = '%Y-%m-%d', main = 'PnL', ylab = 'money')
             }
           }else if(return_type == 'data'){
             ind <- which(colnames(df) == 'date')
             return(xts(df[,-ind], df[, ind]))
           }
           
           
         },
         trade =,
         trades =,
         money_trades = ,
         money_trade =,
         trades_money = ,
         trade_money = {
           report <- getReportTrades(this)
           init_money <- e$results$money[e$activeField['start'],]
           if(leg %in% c('all', 'sum')){
             tmp <- report$pnl.sum
             if(comOn){
               tmp <- tmp - report$com.sum
             }
             pnl <- cumsum(c(0,tmp)) + init_money
           }else if(is.numeric(leg)){
             ind_pnl <- which(grepl('pnl.asset', colnames(report)))
             ind_com <- which(grepl('com.asset', colnames(report)))
             pnl <- cumsum(c(0, report[, ind_pnl[leg]] - comOn * report[, ind_com[leg]]))
           }else if(leg %in% c('sep', 'separate')){
             ind_pnl <- which(grepl('pnl.asset', colnames(report)))
             ind_com <- which(grepl('com.asset', colnames(report)))
             pnl <- rbind(rep(0, length(ind_pnl)) , report[, ind_pnl] - comOn * report[, ind_com]) %>%
               apply(2, cumsum)
           }
           
           if(leg == 'sep'){
             df <- data.frame(pnl) %>% 
               set_colnames(colnames(getModelD(this)$data_raw)) %>%
               dplyr::mutate(index = 1:nrow(pnl))
           }else{
             df <- data.frame(PnL = pnl, index = 1:length(pnl))
           }
           
           if(return_type == 'plot'){
             if(graph_type == 'ggplot2'){
               newdf <- reshape2::melt(df,'index')
               p <- ggplot(newdf, aes(x= index, y = value, color = variable) ) +
                 geom_line() + theme_bw() + ggtitle("PnL money by trade")
               if(leg != 'sep'){
                 p + scale_color_manual(
                   values = c(
                     PnL = 'darkblue'
                   )) + theme(legend.position="none")
               }else{
                 p
               }
               
             }else{
               plot(df[,-ncol(df)], type = 'l', main = 'PnL', ylab = 'money', xlab = 'trades')
             }
           }else if(return_type == 'data'){
             return(xts(df[,-ncol(df)], getDateByIndex(this, as.numeric(report[,'end.ind']))))
           }
           
         }
  )
}




#' Plot drawdowns
#'
#' @param this modelStrategy
#' @param ... params
#'
#' @return ggplot/xts
#' @export
#' @rdname plotDrawdowns
plotDrawdowns <- function(this,
                          ...){
    UseMethod('plotDrawdowns', this)
}



#' Plot drawdowns
#'
#' @param this modelStrategy
#' @param from character, name of backtest
#' @param return_type character, plot or data
#' @param graph_type character, ggplot2 or xts
#' @param ... params
#'
#' @return ggplot/xts
#' @export
#' @rdname plotDrawdowns
#' @method plotDrawdowns modelStrategy
plotDrawdowns.modelStrategy <- function(this,
                                        return_type = 'plot',
                                        graph_type = 'ggplot2',
                                        ...){
  from = 'base'
  e <- this$thisEnv$backtests[[from]]
  dates <- getDateByIndex(this)
  range_start <- e$activeField['start']
  range_end <- e$activeField['end']
  if(range_start > range_end){
    stop("start > end")
  }
  range <- range_start:range_end
  df <- cbind( 
    data.frame(date=dates), 
    data.frame(PnL = e$results$money - cummax(e$results$money))
  )[range,]
  if(return_type == 'plot'){
    if(graph_type == 'ggplot2'){
      newdf <- reshape2::melt(df, 'date')
      ggplot(newdf,aes(x=date, y=value, color = variable) ) +
        geom_line() + theme_bw() + theme(legend.position="none") +
        scale_color_manual(
          values = c(
            PnL = 'darkblue'
          ))+
        ggtitle("Drawdowns by date")
    }else{
      plot(xts(df[,'PnL'], df[,'date']), format.labels = '%Y-%m-%d', main = 'PnL', ylab = 'money')
    }
  }else if(return_type == 'data'){
    return(xts(df[,'PnL'], df[,'date']))
  }
}



#' @export
#' @rdname plotReturns
plotReturns <- function(this, type, from){
    UseMethod('plotReturns', this)
}







#' Plot returns vs MAE/MFE
#'
#' @param this modelStrategy 
#' @param type character, MAE or MFE
#' @param from character, name of backtest
#'
#' @return ggplot
#' @export
#' @rdname plotReturns
#' @method plotReturns modelStrategy
plotReturns.modelStrategy <- function(this, type = 'MAE'){
  from = 'base'
  e <- this$thisEnv$backtests[[from]]
  report <- getReportTrades(this) %>%
    dplyr::mutate(ind = 1:dplyr::n())
  rets <- 'pnl.sum.adj'
  switch(type,
         MAE=,
         min=,
         loss=,
         maxloss={
           var <- 'MAE.with.com'
         },
         MFE=,
         max=,
         profit=,
         maxprofit={
           var <- 'MFE.with.com'
         }
  )
  
  df <- report[,c(rets,var)] %>%
    set_colnames(c('rets','var'))
  p <- ggplot(df, aes(abs(rets),var, group = rets < 0 )) +
    geom_point(aes(col = rets < 0, shape = rets  < 0) , size = 3) +
    #geom_point(aes(colour = rets > 0)) +
    #scale_shape(solid = FALSE) +
    scale_shape_manual(values=c(24,25)) +
    scale_color_manual(values=c('green','red')) +
    geom_abline(intercept = 1, linetype = 'dotted') + 
    theme_bw() +
    theme(legend.position="none") +
    labs(y = paste(strsplit(var,'\\.')[[1]][1]),
         x = paste0(strsplit(rets,'\\.')[[1]][1],'s')) +
    ggtitle(paste(strsplit(var,'\\.')[[1]][1] ,'vs', paste0(strsplit(rets,'\\.')[[1]][1],'s') ) )
  return(p)
  
}



#' Plot pnl in month-year matrix
#'
#' @param ... params
#' @param this modelStrategy
#'
#' @export
#' @rdname plotCalendar
plotCalendar <- function(this, ...){
    UseMethod('plotCalendar', this)
}


#' Plot pnl in month-year matrix
#'
#' @param this modelStrategy
#' @param from character, name of backtest
#' @param compounded logical, compounded returns to use or not
#' @param ... params
#'
#' @export
#' @rdname plotCalendar
#' @method plotCalendar modelStrategy
plotCalendar.modelStrategy <- function(this, compounded = FALSE, ...){
  from = 'base'
  M <- apply.monthly(getPnL(this), FUN = function(x){
    if(compounded){
      (tail(x, 1)[[1]] - head(x, 1)[[1]]) / head(x, 1)[[1]] * 100
    }else{
      (tail(x, 1)[[1]] - head(x, 1)[[1]]) / getMoney(this) * 100
    }
  }) %>%
    set_colnames('rets') %>%
    cbind(year = lubridate::year(index(.))) %>%
    cbind(month  = lubridate::month(index(.))) %>%
    coredata %>%
    data.frame %>%
    {
      reshape2::dcast(.,month ~ year,value.var = 'rets')[,-1] 
    } %>%
    set_rownames(month.name) %>%
    as.matrix %>%
    {
      .[is.na(.)] <- 0
      .
    }
  corrplot::corrplot(M, method="color",
                     col=colorRampPalette( c("red", "white", "green"), space="rgb")(200),  
                     addCoef.col = "black", 
                     tl.col="black", 
                     tl.srt=45, #Text label color and rotation
                     insig = "blank", 
                     cl.pos = 'n',
                     is.corr = FALSE
  )
}



#' @export
#' @rdname plotCalendar
#' @method plotCalendar xts
plotCalendar.xts <- function(this, ...){
  M <- apply.monthly(this, FUN = function(x){
    (tail(x, 1)[[1]] - head(x, 1)[[1]]) / head(x, 1)[[1]] * 100
  }) %>%
    set_colnames('rets') %>%
    cbind(year = lubridate::year(index(.))) %>%
    cbind(month  = lubridate::month(index(.))) %>%
    coredata %>%
    data.frame %>%
    {
      reshape2::dcast(.,month ~ year,value.var = 'rets')[,-1] 
    } %>%
    set_rownames(month.name) %>%
    as.matrix %>%
    {
      .[is.na(.)] <- 0
      .
    }
  corrplot::corrplot(M, method="color",
                     col=colorRampPalette( c("red", "white", "green"), space="rgb")(200),  
                     addCoef.col = "black", 
                     tl.col="black", 
                     tl.srt=45, #Text label color and rotation
                     insig = "blank", 
                     cl.pos = 'n',
                     is.corr = FALSE
  )
}


#' Plot Capital of strategy
#' 
#' 
#' @param this modelStrategy
#' @param ... params
#' @export
#' @rdname plotCapital
plotCapital <- function(this, 
                        ...){
  UseMethod('plotCapital', this)
}

#' @param start_date Date / character, example: start_date='2008-01-01'
#' @param interactive_plot logical, if it is TRUE then plot will be intercative
#' @param leg numeric / character, numeric is responsible for capital by legs, character can be "all" then capital will be summed or it can be "sep" then 
#' capital will be plotted for each leg
#' @param end_date Date / character, example: end_date='2018-01-01'
#'
#' @export
#' @rdname plotCapital
#' @method plotCapital modelStrategy
plotCapital.modelStrategy <- function(this,
                                      interactive_plot = TRUE,
                                      start_date = NULL,
                                      end_date = NULL,
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
  legs <- leg
  leg <- legs[1]
  if(leg == 'all'){
    x <- this$thisEnv$backtests$base$results$money_in_pos
    x[x == 0] <- NA
  }else if(leg == 'sep'){
    x <- this$thisEnv$backtests$base$results$money_in_pos_leg 
    x[x == 0] <- NA
  }else if(is.numeric(leg)){
    x <- this$thisEnv$backtests$base$results$money_in_pos_leg[,legs] 
    x[x == 0] <- NA
  }
  
  df <- cbind( 
    data.frame(date=dates), 
    data.frame(Money = x)
  )[range,]
  if(leg == 'sep'){
    colnames(df) <- c('date', colnames(getModelD(this)$data_raw))
  }else if(is.numeric(leg)){
    colnames(df) <- c('date', colnames(getModelD(this)$data_raw)[legs])
  }
  newdf <- reshape2::melt(df, 'date')
  p <- ggplot(newdf,aes_string(x="date", y="value", color = "variable") ) +
    geom_line() + theme_bw() + ggtitle("Money in position") #+ theme(legend.position = "none")
  if(leg != 'sep'){
    p + scale_color_manual(
      values = c(
        PnL = 'darkblue'
      )) + theme(legend.position="none")
  }
  if(interactive_plot){
    return(plotly::ggplotly(p))
  }
  p
}

#' @export
#' @rdname plotCapital
#' @method plotCapital modelPortfolio
plotCapital.modelPortfolio <- function(this,#interactive_plot = TRUE,
                                       ...){
  dots <- list(...)
  #dots[['interactive_plot']]<-interactive_plot
  dots[['this']] <- this
  aggregate_prepared_models(this, ...)
  do.call("plotCapital.modelStrategy", args=dots)
}

#' Plot open and close position
#' 
#' 
#' 
#'
#' @param this modelStrategy
#' @param ... params
#' @export
#' @rdname plotStrategy
plotStrategy <- function(this, 
                         ...){
  UseMethod('plotStrategy', this)
}

#' @param multi_plot logical, if TRUE plot spread and legs
#' @export
#' @param start_date date type, example: start_date='2008-01-01'
#' @param end_date date type, example: end_date='2018-01-01'
#' @rdname plotStrategy
#' @method plotStrategy modelStrategy
plotStrategy.modelStrategy <- function(this, 
                                       multi_plot=FALSE,
                                       start_date = NULL,
                                       end_date = NULL,
                                       ...){
  reports <- getReportTrades(this)
  start <- reports$date.start
  stop <- reports$date.end
  side <- reports$side
  from <- 'base'
  e <- this$thisEnv$backtests[[from]]
  dates <- getDateByIndex(this)
  if (!is.null(start_date)){
    reports <- reports[reports$date.start > start_date,]
    side <- reports$side
    range_start <- max(e$activeField['start'],  sum(dates < start_date) + 1)
  }else{
    range_start <- e$activeField['start']
  }
  if(!is.null(end_date)){
    range_end <- min(e$activeField['end'], sum(dates < end_date))
  }else{
    range_end <- e$activeField['end']
  }
  if(range_start > range_end){
    stop("start > end")
  }
  range <- range_start:range_end
  tryCatch({
    with(this$thisEnv, tmp <- new.env())
    eval(this$thisEnv$pps[[1]]$evolution$data, envir = this$thisEnv$tmp)
    this$thisEnv$tmp <- NULL
  }, error = function(e){
    print(e)
    stop('You are using illegal arguments')
    return()
  })
  df <- cbind( 
    data.frame(date=dates), 
    data.frame(PnL = this$thisEnv$modelD[[this$thisEnv$spreadData]] %*% cbind(this$thisEnv$beta_fun())))[range,] %>%set_colnames(c('date','spread'))
  p1 <- ggplot(df, aes_string("date", 'spread')) + geom_line(size = 0.4)
  increasings <- (Diff(e$results$positions_mult) > 0 & sign(e$results$positions_mult * Lag(e$results$positions_mult)) > 0 &
                    (e$results$positions_side * Lag(e$results$positions_side)) > 0)[range,]
  if (TRUE %in% increasings){
    p1 <- p1 + geom_point(data = df[increasings,], aes_string("date", 'spread'), color='yellow', size = 1.5)
  }
  if (length(stop) != 0){
    p1 <- p1 + geom_point(data = df[df$date %in% stop,], aes_string("date", 'spread'),  color='blue', size = 2)
  }
  if (TRUE %in% (side>0)){
    p1 <- p1 + geom_point(data = df[df$date %in% start,][side>0,], aes_string("date", 'spread'), shape = 24, color='green', size = 2)
  }
  if (TRUE %in% (side<0)){
    p1 <- p1 + geom_point(data = df[df$date %in% start,][side<0,], aes_string("date", 'spread'), shape = 25, color='red', size = 2)
  }
  
  p1 <- plotly::ggplotly( p1 ,dynamicTicks = TRUE)
  if (!multi_plot){
    return(p1)
  }
  beta <- this$thisEnv$beta_fun()
  graph <- list(p1)
  for (i in 1:length(beta)){
    df <- cbind( 
      data.frame(date=dates), 
      data.frame(PnL = this$thisEnv$modelD[[this$thisEnv$spreadData]])[,i])[range,] %>%set_colnames(c('date',paste0("price_leg_",as.character(i))))
    p1 <- ggplot(df, aes_string("date", paste0("price_leg_", as.character(i)))) + geom_line(size = 0.4)
    increasings <- (Diff(e$results$positions_mult) > 0 & sign(e$results$positions_mult * Lag(e$results$positions_mult)) > 0 &
                      (e$results$positions_side * Lag(e$results$positions_side)) > 0)[range,]
    if (TRUE %in% increasings){
      p1 <- p1 + geom_point(data = df[increasings,], aes_string("date", 'spread'), color='yellow', size = 1.5)
    }
    if (length(stop) != 0){
      p1 <- p1 + geom_point(data = df[df$date %in% stop,], aes_string("date", paste0("price_leg_" ,as.character(i))), color='blue', size = 2)
    }
    if (TRUE %in% (beta[i]*side>0)){
      p1 <- p1 + geom_point(data = df[df$date %in% start,][beta[i]*side>0,],aes_string("date", paste0("price_leg_",as.character(i))), shape = 24, color='green', size = 2)
    }
    if (TRUE %in% (beta[i]*side<0)){
      p1 <- p1 + geom_point(data = df[df$date %in% start,][beta[i]*side<0,], aes_string("date", paste0("price_leg_",as.character(i))), shape = 25, color='red', size = 2)
    }       
    graph[[i+1]] <- plotly::ggplotly(p1, dynamicTicks = TRUE)
  }
  
  plotly::subplot( graph, nrows = (length(beta)+1), shareX = TRUE, shareY = TRUE)
}




#' Plot interactive distribution params
#'
#' @param this it is Strategy
#' @rdname plotShiny
#' @param ... params for shinyApp
#' @export
plotShiny <- function(this, 
                      ...){
    UseMethod('plotShiny', this)
}




#' Plot interactive distribution params
#'
#' @param this it is Strategy
#' @rdname plotShiny
#' @param ... params for shinyApp
#' @export

#' @param paramset name of paramset 
#' @param session object of class ssh_session
#' @param delete_save logical, please use TRUE, if you want delite save strategy
#' @param start_date character, initial start date, example: start_date = "2010-01-01"
#' @param end_date  character, initial stop date,  example: end_date = "2010-01-01"
#' @return
#' @export
#' @examples
#' @rdname plotShiny
#' @method plotShiny modelStrategy
plotShiny.modelStrategy <- function(this,session, paramset = 1, delete_save = FALSE, start_date = '1900-01-01', end_date = '2999-01-01',...){
  if(missing(session)){
    session <- .env[['session']]
  }
  if (delete_save)
  {
    this$thisEnv[['save_strategy']] <- c()
  }
  clone <- function(this, ...){
    e <-  this$thisEnv %>%
      ls %>%
      setdiff(., c('backtests', 'data_from_user')) %>%
      mget(.,envir = this$thisEnv) %>%
      as.environment
    parent.env(e) <- parent.frame()
    e$data_from_user <- this$thisEnv$data_from_user
    e$me$thisEnv <- e
    e$thisEnv <- e
    return(e$me)
  }
  this_2 <- clone(this)
  distribution <- this$thisEnv$paramsets[[paramset]]$distributions
  distribution_length <- length(distribution)
  distribution_names <- names(distribution)
  slider = c()
  number_columns <- c()
  char_columns <- c()
  for (i in 1:distribution_length){
    if (is.numeric(distribution[[i]]$variable[[1]][1])){
      number_columns <- c(number_columns,i)
    }
    else{
      char_columns <- c(char_columns,i)
    }
  }
  min_date <- head(index(this$thisEnv$data_from_user), 1)
  max_date <- tail(index(this$thisEnv$data_from_user), 1)
  value <- c()
  e <- rlang::expr(shiny::sliderInput(inputId = 'date', label = 'date', 
                               min = min_date, max = max_date, 
                               value = c(max(min_date, as.Date(start_date)),min(max_date, as.Date(end_date)), step = 1)))
  slider <- c(slider, e)
  for (i in number_columns){
    if (distribution[[i]]$component.type == 'indicators'){
      label <- distribution[[i]]$component.label
      name <- names(distribution[[i]]$variable)
      value <- this$thisEnv$indicators[[label]]$args[[name]]
    } 
    if (distribution[[i]]$component.type == 'params'){
      name <- names(distribution[[i]]$variable)
      label <- distribution[[i]]$component.label
      value <- this$thisEnv$params[[label]][[name]]
    }
    if (distribution[[i]]$component.type == 'lookback'){
      value <- this$thisEnv$lookback
    }
    if (distribution[[i]]$component.type == 'lookforward'){
      value <- this$thisEnv$lookForward
      if (value == Inf){
        value <- 0
      }
    }
    if (distribution[[i]]$component.type == 'rule'){
      label <- distribution[[i]]$component.label
      name <- names(distribution[[i]]$variable)
      value <- this$thisEnv$rules[[label]]$args[[name]]
    }
    e <- rlang::expr(shiny::sliderInput(inputId = distribution_names[!!i], label = distribution_names[!!i], 
                                        min = min(distribution[[!!i]]$variable[[1]],!!value), max = max(distribution[[!!i]]$variable[[1]],!!value), 
                                        value = !!value, step = distribution[[!!i]]$variable[[1]][2] - 
                                          distribution[[!!i]]$variable[[1]][1]))
    slider <- c(slider, e)
  }
  for (i in char_columns){
    e <- expr(shiny::selectInput(inputId = distribution_names[!!i], label = distribution_names[!!i], 
                                 choices = distribution[[!!i]]$variable[[1]]))
    slider <- c(slider, e)
  }
  e <- rlang::expr(shiny::checkboxInput("checkbox", "report", value = TRUE))
  slider <- c(slider, e)
  e <- rlang::expr(shiny::actionButton("action", "Action"))
  slider <- c(slider, e)
  e <- rlang::call2(shiny::sidebarPanel, !!!slider)
  
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      eval(e),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("PnL",
                          shiny::plotOutput('plot'),
                          shiny::tableOutput("values1")
          ),
          shiny::tabPanel("Strategy",
                          plotly::plotlyOutput('plot_2'),
                          shiny::tableOutput("values2")
          )
        )
      )
    )
  )
  
  unique_name = "gvajelsg,kAS:jgkihseKvgfaljgfovhrsjijoAKLF;CLAWEPG"
  
  server <- function(input, output) {
    this_2$thisEnv$paramsets[[unique_name]] <- this$thisEnv$paramsets[[1]]
    Update <- shiny::reactive({
      for (i in distribution_names){
        this_2$thisEnv$paramsets[[unique_name]]$distributions[[i]]$variable[[1]] <- input[[i]]
      }
      performServer(this_2,session, paramset.index = 1, paramset.label = unique_name, 
                    start_date = input[['date']][1], end_date = input[['date']][2], report = input[['checkbox']][1])
      this_2
    })
    
    sliderValues1 <- shiny::reactive({
      x <- getReportStrategy(Update())
      nms <- rownames(x)
      if (input[['checkbox']][1]){
        return(data.frame(nms[1:9],as.numeric(x[1:9]),nms[10:18],as.numeric(x[10:18]), nms[19:27],as.numeric(x[19:27])))
      }
      data.frame()
    })
    shiny::observeEvent(input$action, {
      if (input[["action"]][[1]]){
        this$thisEnv[['save_strategy']][[length(this$thisEnv[['save_strategy']]) + 1]] <- this_2
      }
    })
    output$plot <- shiny::renderPlot({
      plotPnL(Update())
    })
    
    output$plot_2 <- plotly::renderPlotly({
      p <- plotStrategy(Update())
      p
    })
    
    output$values1 <- shiny::renderTable({
      sliderValues1()
    }, width = '100%', colnames = FALSE, na = '', striped = TRUE)
    
    output$values2 <- shiny::renderTable({
      sliderValues1()
    }, width = '100%', colnames = FALSE, na = '', striped = TRUE)
    
  }
  shiny::shinyApp(ui = ui, server = server, ...)
  
}








#' Draws 5-D graph with axis x,y,size,color,symbol, which are contained in data.frame 
#' 
#' @param df data.frame
#' @param x character/expression type, axis x, default NULL
#' @param y character/expression type, axis y, default NULL
#' @param size character/expression type, axis size, default NULL
#' @param color character/expression type, axis color, default NULL
#' @param symbol character/expression type, axis symbol, default NULL
#' @param size_scale numeric type, point size, default 20
#' @param omitcols character vector, names of columns that should be omitted 
#' @return plot_ly object
#' @export
#' @rdname plotTable
plotTable <- function(df, x=NULL, y=NULL ,size=NULL, color=NULL, symbol=NULL, size_scale = 20, omitcols = NULL){
  for(xx in omitcols){
    if(xx %in% colnames(df)){
      df[[xx]] <- NULL
    }
  }
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  size <- rlang::enexpr(size)
  color <- rlang::enexpr(color)
  symbol <- rlang::enexpr(symbol)
  if(is.character(x)){
    x <- as.symbol(x)
  }
  if(is.character(y)){
    y <- as.symbol(y)
  }
  if(is.character(color)){
    color <- as.symbol(color)
  }
  if(is.character(size)){
    size <- as.symbol(size)
  }
  if(is.character(symbol)){
    symbol <- as.symbol(symbol)
  }
  if(is.null(size)){
    size <- NULL
    sizeref <- NULL
  }else{
    sizeref <- call('~', rlang::expr(2.0 * max(!!size) / (!!size_scale)**2 ))
  }
  if(is.null(symbol)){
    symbol <- NULL
  }else{
    if (length(unique(df[[as.character(symbol)]])) > 6){
      stop("The shape palette can deal with a maximum of 6 discrete values because more than 6 becomes difficult to discriminate")
    }
    symbol <- call("~",call("factor",rlang::expr(!!symbol)))
  }
  if(is.null(color)){
    color <- NULL
  }else{
    color <- call("~",rlang::expr(!!color))
  }
  
  q <- rlang::call2('paste', "<br>",!!!sapply(names(df), function(x){c(paste("<br>",x,":"),as.symbol(x))}))
  expr <- rlang::call2(quote(plotly::plot_ly), data = quote(df), x = call("~", x),
                       y = call("~", y),
                       color = color, hoverinfo = "text",
                       text = call("~", q), symbol = symbol, type = "scatter",
                       mode = "markers",
                       marker = rlang::call2("list",
                                             size = call("~", size),
                                             opacity = 0.5,
                                             sizemin = 2,
                                             sizemode = 'area',
                                             sizeref = sizeref))
  #print(expr)
  eval(expr)
}

#' Plot paramsets
#'
#' @param this modelStrategy
#' @param ... params
#'
#' @return ggplot/xts
#' @export
#' @rdname plotParamset

plotParamset <- function(this,
                          ...){
    UseMethod('plotParamset', this)
}


#' Draws 5-D graph with axis x,y,size,color,symbol, which are contained in data.frame 
#' 
#' @param df data.frame
#' @param x character/expression type, axis x, default "sharpe.ann"
#' @param y character/expression type, axis y, default "sortino.ann"
#' @param size character/expression type, axis size, default NULL
#' @param color character/expression type, axis color, default NULL
#' @param symbol character/expression type, axis symbol, default NULL
#' @param omitcols character vector, names of columns that should be omitted 
#' @param size_scale numeric type, point size, default 20
#'
#' @return plot_ly object
#' @export
#' @rdname plotParamset
#' @method plotParamset data.frame
plotParamset.data.frame <- function(df, x = "sharpe.ann", y = "return.pos.drawdown", size = NULL, 
                                    color = "trades.year", symbol = NULL, size_scale = 20, 
                                    omitcols = c("median", "max.loose", "max.win", "in.pos.positive", "straight.t")){
  cl <- rlang::call2('plotTable', !!!rlang::enexprs(df=df, x=x, y=y, size=size, color=color, symbol=symbol, 
                                                    size_scale=size_scale, omitcols=omitcols))
  #print(cl)
  eval(cl)
}

#' Plot backtests results in 5-D graph
#'
#' @param this modelStrategy
#' @param ... params
#'
#' @return plot_ly object
#' @export
#' @rdname plotParamset
#' @method plotParamset modelStrategy
plotParamset.modelStrategy <- function(this, ...){
  cl <- rlang::call2('plotParamset.data.frame', df=quote(getBacktestResults(this)), !!!rlang::enexprs(...))
  eval(cl)
}


