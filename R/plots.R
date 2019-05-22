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



#' Plots change of pnl through backtest period
#'
#' @param this modelStrategy
#' @param type character, one of c('money','trades','percents')
#' @param from character, name of folder in backtests
#' @param leg numeric/character, number of leg, if it is equal to 'all' or 'sum', then all pnl among all legs
#' will be summed, if it is equal to 'sep', then pnl among legs will be plotted
#' @param graph_type character, ggplot2 or xts 
#' @param each_year logical, if TRUE, then each graph will start with 0 each year
#' @param adjust logical, if TRUE, then values will be divided by getMoney(this)
#' @param ... 
#' @param comOn bool, if true then commission will be included in the 'trades' graph
#' @param return_type character, plot or data
#'
#' @export
#' @rdname plotPnL
#' @method plotPnL modelStrategy
plotPnL.modelStrategy <- function(this, 
                                  type = 'money', 
                                  from = 'base', 
                                  comOn = TRUE, 
                                  leg = 'all', 
                                  graph_type = 'ggplot2',
                                  each_year = FALSE,
                                  adjust = FALSE,
                                  return_type = 'plot',
                                  cutoff = FALSE,
                                  ...){
  e <- this$thisEnv$backtests[[from]]
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
           report <- getReportTrades(this, from = from)
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
#' @param from character, name of backtest
#' @param return_type character, plot or data
#' @param graph_type character, ggplot2 or xts
#'
#' @return ggplot/xts
#' @export
plotDrawdowns.modelStrategy <- function(this,
                                        from = 'base',
                                        return_type = 'plot',
                                        graph_type = 'ggplot2'){
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

#' Plot returns vs MAE/MFE
#'
#' @param this modelStrategy 
#' @param type character, MAE or MFE
#' @param from character, name of backtest
#'
#' @return ggplot
#' @export
plotReturns.modelStrategy <- function(this, type = 'MAE'){
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
  return(p)
  
}


#' Plot pnl in month-year matrix
#'
#' @param this modelStrategy
#' @param from character, name of backtest
#' @param compounded logical, compounded returns to use or not
#'
#' @export
plotCalendar.modelStrategy <- function(this, compounded = FALSE){
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


#' Plot pnl in month-year matrix
#'
#' @param x xts one column of cumulative profit and loss
#'
#' @export
plotCalendar.xts <- function(x){
  M <- apply.monthly(x, FUN = function(x){
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




#' plot profit and loss graph
#'
#' @param l list, list of strategies
#' @param ... params
#'
#' @export
plotPnL.list <- function(l, legend = TRUE, ...){
  args <- list(...)
  args['leg'] <- 'sum'
  df <- lapply(l, function(x){
    do.call('plotPnL', args = c(list(this = x, return_type = 'data'), args))
    #plotPnL(x, return_type = 'data', ...)
  }) %>%  
    Reduce('cbind', .) %>%
    {
      if(!is.null(names(l))){
        set_colnames(., names(l))
      }else{
        set_colnames(., paste0('Strategy', seq_len(length(l)))) 
      }
    }
  dates <- index(df)
  df <- data.frame(coredata(df)) %>%
    dplyr::mutate(date = dates)
  newdf <- reshape2::melt(df, 'date')
  
  p <- ggplot(newdf,aes(x=date, y=value, color = variable) ) +
    geom_line() + theme_bw() + 
    ggtitle("PnL money by date")
  if(!legend){
    p <- p + theme(legend.position="none")
  }
  return(p)
}


#' plot profit and loss graph
#'
#' @param l list, list of strategies
#' @param ... params
#'
#' @export
plotDrawdowns.list <- function(l, legend = TRUE, ...){
  df <- lapply(l, function(x){
    plotDrawdowns(x, return_type = 'data', ...)
  }) %>%  
    Reduce('cbind', .) %>%
    {
      if(!is.null(names(l))){
        set_colnames(., names(l))
      }else{
        . 
      }
    }
  dates <- index(df)
  df <- data.frame(coredata(df)) %>%
    dplyr::mutate(date = dates)
  newdf <- reshape2::melt(df, 'date')
  
  p <- ggplot(newdf,aes(x=date, y=value, color = variable) ) +
    geom_line() + theme_bw() + 
    ggtitle("Drawdowns by date")
  if(!legend){
    p <- p + theme(legend.position="none")
  }
  return(p)
}




#' plot profit and loss graph
#'
#' @param this modelPortfolio
#' @param ... params
#'
#' @export
plotPnL.modelPortfolio <- function(this, legend = TRUE, ...){
  plotPnL.modelStrategy(this, ...)
}


#' plot pnl in month-year matrix
#'
#' @param this modelPortfolio
#' @param ... params
#'
#' @export
plotCalendar.modelPortfolio <- function(this, ...){
  plotCalendar.modelStrategy(this, ...)
}


#' plot drawdowns
#' 
#' @param this modelPortfolio
#' @param ... params
#'
#' @export
plotDrawdowns.modelPortfolio <- function(this, legend = TRUE, ...){
  plotDrawdowns.modelStrategy(this, ...)
}







