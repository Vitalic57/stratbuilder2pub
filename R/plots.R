# 
#  Plot graph maximum/minimum pnl in trade vs realized pnl
# 
#  @param report data.frame, must contain 3 columns 'MAE.with.com', 'MFE.with.com', 'pnl.sum.adj'. 
# It can be report of trades, returned from simulation
#  @param type character, 'MFE' or 'MAE'
# 
#  @return ggplot
#  @export
# plotReturns <- function(report, type = 'MAE'){
#   switch(type,
#          MAE=,
#          min=,
#          loss=,
#          maxloss={
#            var <- 'MAE.with.com'
#          },
#          MFE=,
#          max=,
#          profit=,
#          maxprofit={
#            var <- 'MFE.with.com'
#          }
#   )
#   rets <- 'pnl.sum.adj'
#   df <- report[,c(rets,var)]
#   colnames(df) <- c('rets','var')
#   p <- ggplot(df, aes(abs(rets),var, group = rets < 0 )) +
#     geom_point(aes(col = rets < 0, shape = rets  < 0) , size = 3) +
#     #geom_point(aes(colour = rets > 0)) +
#     #scale_shape(solid = FALSE) +
#     scale_shape_manual(values=c(24,25)) +
#     scale_color_manual(values=c('green','red')) +
#     geom_abline(intercept = 1, linetype = 'dotted') +
#     theme_bw() +
#     theme(legend.position="none") +
#     labs(y = paste(strsplit(var,'\\.')[[1]][1]),
#          x = paste0(strsplit(rets,'\\.')[[1]][1],'s')) +
#     ggtitle(paste(strsplit(var,'\\.')[[1]][1] ,'vs', paste0(strsplit(rets,'\\.')[[1]][1],'s') ) )
#   return(p)
# }



#' Plot change of pnl through backtest period
#'
#' @param this modelStrategy
#' @param ... params
#' @export
#' @rdname plotPnL
plotPnL <- function(this, 
                    ...){
  UseMethod('plotPnL', this)
}

#' @param type character, one of c('money','trades','percents')
#'
#' @param leg numeric/character, number of leg, if it is equal to 'all' or 'sum', then all pnl among all legs
#' will be summed, if it is equal to 'sep', then pnl among legs will be plotted
#' @param graph_type character, ggplot2 or xts 
#' @param each_year logical, if TRUE, then each graph will start with 0 each year
#' @param adjust logical, if TRUE, then values will be divided by getMoney(this)
#' @param comOn bool, if true then commission will be included in the 'trades' graph
#' @param return_type character, plot or data
#' @param start_date date type, example: start_date='2008-01-01'
#' @param end_date date type, example: end_date='2018-01-01'
#' @param cutoff logical, if TRUE then on plot will be horizonal line indicating when model was created
#' @param on_percentage logical, if TRUE then on plot will be in percentage
#' @param interactive_plot logical, if it is TRUE and graph_type == 'ggplot2', plot will be interactive
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
                                  start_date = NULL,
                                  end_date = NULL,
                                  on_percentage = FALSE,
                                  interactive_plot = FALSE,
                                  ...){
  from <- 'base'
  e <- this$thisEnv$backtests[[from]]
  switch(type,
         money = {
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
               data.frame(date=dates), 
               data.frame(PnL = (init_money + apply(e$results$unrealized_money + e$results$realized_money +  
                                                     apply( (1 - comOn) * e$results$commissions_table, 2, cumsum), 1, sum))/ max(1,e$results$money[range_start,]*on_percentage))
             )[range,]
           }else if(is.numeric(leg)){ 
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
                 set_colnames(colnames(getModelD(this)$data_diff))
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
               p <- ggplot(newdf,aes_string(x="date", y="value", color = "variable") ) +
                 geom_line() + theme_bw() + ggtitle("PnL money by date")
               if(each_year){
                 p <- p + geom_vline(xintercept=last_dates, linetype=4, colour="red")
               }
               if(cutoff && 'created' %in% names(this$thisEnv)){
                 p <- p + geom_vline(xintercept=as.numeric(this$thisEnv$created), linetype=4, colour="green")
               }
               if(leg != 'sep'){
                 p <- p + scale_color_manual(
                   values = c(
                     PnL = 'darkblue'
                   )) + theme(legend.position="none")
               }
               if(interactive_plot){
                 return(plotly::ggplotly(p))
               }
               return(p)
               
               
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
               set_colnames(colnames(getModelD(this)$data_diff)) %>%
               dplyr::mutate(index = 1:nrow(pnl))
           }else{
             df <- data.frame(PnL = pnl, index = 1:length(pnl))
           }
           
           if(return_type == 'plot'){
             if(graph_type == 'ggplot2'){
               newdf <- reshape2::melt(df,'index')
               p <- ggplot(newdf, aes_string(x= "index", y = "value", color = "variable") ) +
                 geom_line() + theme_bw() + ggtitle("PnL money by trade")
               if(leg != 'sep'){
                 p <- p + scale_color_manual(
                   values = c(
                     PnL = 'darkblue'
                   )) + theme(legend.position="none")
               }
               if(interactive_plot){
                 return(plotly::ggplotly(p))
               }
               return(p)
               
             }else{
               plot(df[,-ncol(df)], type = 'l', main = 'PnL', ylab = 'money', xlab = 'trades')
             }
           }else if(return_type == 'data'){
             return(xts(df[,-ncol(df)], getDateByIndex(this, as.numeric(report[,'end.ind']))))
           }
           
         }
         # percents_money =,
         # percents =,
         # returns =,
         # money_percents =,
         # rets = {
         #   rets <- e$results$money/Lag(e$results$money,1) - 1
         #   range_start <- e$activeField['start']
         #   range_end <- e$activeField['end']
         #   if(range_start > range_end){
         #     stop("start > end")
         #   }
         #   range <- range_start:range_end
         #   rets <- rets[range]
         #   rets[1] <- 0
         #   dates <- getDateByIndex(this, range)
         #   df <- cbind(data.frame(date=dates),data.frame(PnL = cumsum(rets)))
         #   newdf <- reshape2::melt(df,'date')
         #   ggplot(newdf,aes(x=date,y=value,color = variable) ) +
         #     geom_line() + theme_bw() + theme(legend.position="none") +
         #     scale_color_manual(
         #       values = c(
         #         PnL = 'darkblue'
         #       ))+
         #     ggtitle("PnL cumulative sum of returns")
         # }
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

#' @param return_type character, plot or data
#' @param graph_type character, ggplot2 or xts
#' @param interactive_plot logical, if graph_type == 'ggplot2' and this option is TRUE, then plot will be interactive
#' @export
#' @rdname plotDrawdowns
#' @method plotDrawdowns modelStrategy
plotDrawdowns.modelStrategy <- function(this,
                                        return_type = 'plot',
                                        graph_type = 'ggplot2',
                                        interactive_plot = FALSE,
                                        ...){
  from <- 'base'
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
      p <- ggplot(newdf,aes_string(x="date", y="value", color = "variable") ) +
        geom_line() + theme_bw() + theme(legend.position="none") +
        scale_color_manual(
          values = c(
            PnL = 'darkblue'
          ))+
        ggtitle("Drawdowns by date")
      if(interactive_plot){
        return(plotly::ggplotly(p))
      }
      return(p)
    }else{
      plot(xts(df[,'PnL'], df[,'date']), format.labels = '%Y-%m-%d', main = 'PnL', ylab = 'money')
    }
  }else if(return_type == 'data'){
    return(xts(df[,'PnL'], df[,'date']))
  }
}

#' Plot returns vs MAE/MFE
#'
#' @param this modelStrategy 
#' @param type character, MAE or MFE
#' @param interactive_plot logical, if it is TRUE, then plot will be interactive
#'
#' @return ggplot
#' @export
#' @rdname plotReturns
plotReturns <- function(this, type = 'MAE', interactive_plot=FALSE){
  UseMethod('plotReturns', this)
}

#' @export
#' @rdname plotReturns
#' @method plotReturns modelStrategy
plotReturns.modelStrategy <- function(this, type = 'MAE', interactive_plot=FALSE){
  e <- this$thisEnv$backtests[['base']]
  report <- getReportTrades(this) %>%
    dplyr::mutate(ind = 1:(dplyr::n()))
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
  if(interactive_plot){
    return(plotly::ggplotly(p))
  }
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

#' @export
#' @param compounded logical, compounded returns to use or not
#' @rdname plotCalendar
#' @method plotCalendar modelStrategy
plotCalendar.modelStrategy <- function(this, compounded = FALSE, ...){
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




#' @param legend logical, if true then legend will be printed on the plot
#' @export
#' @rdname plotPnL
#' @method plotPnL list
plotPnL.list <- function(this, legend = TRUE, interactive_plot=FALSE, ...){
  args <- list(...)
  args['leg'] <- 'sum'
  df <- lapply(this, function(x){
    do.call('plotPnL', args = c(list("this" = x, "return_type" = 'data'), args))
    #plotPnL(x, return_type = 'data', ...)
  }) %>%  
    Reduce('cbind', .) %>%
    {
      if(!is.null(names(this))){
        set_colnames(., names(this))
      }else{
        set_colnames(., paste0('Strategy', seq_len(length(this)))) 
      }
    }
  dates <- index(df)
  df <- data.frame(coredata(df)) %>%
    dplyr::mutate(date = dates)
  newdf <- reshape2::melt(df, 'date')
  
  p <- ggplot(newdf,aes_string(x="date", y="value", color = "variable") ) +
    geom_line() + theme_bw() + 
    ggtitle("PnL money by date")
  if(!legend){
    p <- p + theme(legend.position="none")
  }
  if(interactive_plot){
    return(plotly::ggplotly(p))
  }
  return(p)
}




#' @param legend logical, if true then legend will be printed on the plot
#' @export
#' @rdname plotDrawdowns
#' @method plotDrawdowns list
plotDrawdowns.list <- function(this, legend = TRUE, interactive_plot=FALSE, ...){
  df <- lapply(this, function(x){
    plotDrawdowns(x, return_type = 'data', ...)
  }) %>%  
    Reduce('cbind', .) %>%
    {
      if(!is.null(names(this))){
        set_colnames(., names(this))
      }else{
        . 
      }
    }
  dates <- index(df)
  df <- data.frame(coredata(df)) %>%
    dplyr::mutate(date = dates)
  newdf <- reshape2::melt(df, 'date')
  
  p <- ggplot(newdf,aes_string(x="date", y="value", color = "variable") ) +
    geom_line() + theme_bw() + 
    ggtitle("Drawdowns by date")
  if(!legend){
    p <- p + theme(legend.position="none")
  }
  if(interactive_plot){
    return(plotly::ggplotly(p))
  }
  return(p)
}





#' @export
#' @rdname plotPnL
#' @method plotPnL modelPortfolio
plotPnL.modelPortfolio <- function(this, ...){
  dots <- list(...)
  # if('legend' %in% names(dots)){
  #   dots[['legend']] <- NULL
  # }
  if ('leg' %in% names(dots)){
    if (is.numeric(dots[['leg']])){
      dots[['this']] <- this$thisEnv$models[[dots[['leg']]]]
      do.call("plotPnL.modelStrategy", args=dots)
    }else if(dots[['leg']] == 'sep'){
      dots[['this']] <- this$thisEnv$models
      dots[['legend']] <- T
      do.call("plotPnL.list", args=dots)
    }else {dots[['this']] <- this
    do.call("plotPnL.modelStrategy", args=dots)
    }
  }
  else{
    dots[['this']] <- this
    do.call("plotPnL.modelStrategy", args=dots)
  }
}



#' @export
#' @rdname plotCalendar
#' @method plotCalendar modelPortfolio
plotCalendar.modelPortfolio <- function(this, ...){
  plotCalendar.modelStrategy(this, ...)
}



#' @export
#' @rdname plotDrawdowns
#' @method plotDrawdowns modelPortfolio
plotDrawdowns.modelPortfolio <- function(this, ...){
  dots <- list(...)
  if('legend' %in% names(dots)){
    dots[['legend']] <- NULL
  }
  dots[['this']] <- this
  do.call("plotDrawdowns.modelStrategy", args=dots)
}


#' Plot Capital of strategy
#' 
#' 
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

#' @param interactive_plot, logical, use FALSE if you want to disable the interactive graph
#' @param start_date date type, example: start_date='2008-01-01'
#' @param end_date date type, example: end_date='2018-01-01'
#' @export
#' @rdname plotCapital
#' @method plotCapital modelStrategy
plotCapital.modelStrategy <- function(this,
                                      interactive_plot = TRUE,
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
  x <- this$thisEnv$backtests$base$results$money_in_pos
  x[x == 0] <- NA
  df <- cbind( 
    data.frame(date=dates), 
    data.frame(Money_ = x)
  )[range,]
  newdf <- reshape2::melt(df, 'date')
  p <- ggplot(newdf,aes_string(x="date", y="value", color = "variable") ) +
    geom_line() + theme_bw() + ggtitle("Money in position") + theme(legend.position = "none")
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
  tryCatch({
    eval(this$thisEnv$pps[[1]]$evolution$data, envir = this$thisEnv)
  }, error = function(e){
    stop('You are using illegal arguments')
    return()
  })
  
  df <- cbind( 
    data.frame(date=dates), 
    data.frame(PnL = this$thisEnv$modelD[[this$thisEnv$spreadData]] %*% cbind(this$thisEnv$beta_fun())))[range,] %>%set_colnames(c('date','spread'))
  p1 <- ggplot(df, aes_string("date", 'spread')) + geom_line(size = 0.4)
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
      data.frame(PnL = this$thisEnv$data_from_user)[,i])[range,] %>%set_colnames(c('date',paste0("price_leg_",as.character(i))))
    p1 <- ggplot(df, aes_string("date", paste0("price_leg_", as.character(i)))) + geom_line(size = 0.4)
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
plotShiny <- function(this, 
                         ...){
  UseMethod('plotShiny', this)
}



#' @param paramset name of paramset 
#' @param session object of class ssh_session
#' @return
#' @export
#'
#' @examples
#' @rdname plotShiny
#' @method plotShiny modelStrategy
plotShiny.modelStrategy <- function(this,session, paramset = 1, ...){
  if(missing(session)){
    session <- .env[['session']]
  }
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
  for (i in number_columns){
    e <- expr(shiny::sliderInput(inputId = distribution_names[!!i], label = distribution_names[!!i], 
                          min = min(distribution[[!!i]]$variable[[1]]), max = max(distribution[[!!i]]$variable[[1]]), 
                          value = distribution[[!!i]]$variable[[1]][2], step = distribution[[!!i]]$variable[[1]][2] - 
                            distribution[[!!i]]$variable[[1]][1]))
    slider <- c(slider, e)
  }
  for (i in char_columns){
    e <- expr(shiny::selectInput(inputId = distribution_names[!!i], label = distribution_names[!!i], 
                          choices = distribution[[!!i]]$variable[[1]]))
    slider <- c(slider, e)
  }
  slider
  e <- rlang::call2(shiny::sidebarPanel, !!!slider)
  
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      eval(e),
      shiny::mainPanel(
        shiny::plotOutput('plot'),
        shiny::tableOutput("values1")
      )
    )
  )
  
  unique_name = "gvajelsg,kAS:jgkihseKvgfaljgfovhrsjijoAKLF;CLAWEPG"
  
  server <- function(input, output) {
    
    this$thisEnv$paramsets[[unique_name]] <- this$thisEnv$paramsets[[1]]
    
    Update <- shiny::reactive({
      for (i in distribution_names){
        this$thisEnv$paramsets[[unique_name]]$distributions[[i]]$variable[[1]] <- input[[i]]
      }
      performServer(this,session, paramset.index = c(1), paramset.label = c(unique_name))
      this
    })
    
    sliderValues1 <- shiny::reactive({
      x <- getReportStrategy(Update())
      nms <- rownames(x)
      data.frame(nms[1:9],as.numeric(x[1:9]),nms[10:18],as.numeric(x[10:18]), nms[19:27],as.numeric(x[19:27])) 
      #getReportStrategy(this)
    })
    
    
    output$plot <- shiny::renderPlot({
      plotPnL(Update())
    })
    
    output$values1 <- shiny::renderTable({
      sliderValues1()
    }, width = '100%', colnames = FALSE, na = '', striped = TRUE)
    
  }
  shiny::shinyApp(ui = ui, server = server, ...)
  
}

