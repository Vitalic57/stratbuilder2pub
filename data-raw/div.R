library(stratbuilder2pub)
library(quantmod)
library(TTR)
downloaded <- lapply(c('ACWI','XLB'), function(x){
  getSymbols(x, from = '2009-01-01', auto.assign = FALSE) %>% Ad
})%>%
  Reduce('cbind', .) %>% 
  na.omit


dividends <- lapply(c('ACWI','XLB'), function(x){
  getDividends(x, from = '2009-01-01', auto.assign = FALSE)
})%>%
  Reduce('cbind', .) 


beta <- c( 1/1.95, -0.95/1.95)
ema_ <- function(X, Y){
  return(EMA(X-Y, n = 9))
  
}

FRAMA <- function (HLC, n = 20, FC = 1, SC = 200) 
{
  if (n%%2 == 1) 
    n = n - 1
  if(has.Hi(HLC) && has.Lo(HLC)){
    N3 <- (runMax(Hi(HLC), n) - runMin(Lo(HLC), n))/n
    N1 <- (runMax(Hi(HLC), n/2) - runMin(Lo(HLC), n/2))/(n/2)
    price <- (Hi(HLC) + Lo(HLC)) / 2
    if(is.xts(HLC)){
      lagSeries <- lag(HLC, n/2)
    }else{
      lagSeries <- stratbuilder2pub:::Lag.matrix(HLC, n / 2)
    }
    N2 <- (runMax(Hi(lagSeries), n/2) - runMin(Lo(lagSeries), 
                                               n/2))/(n/2)
  }else{
    if(has.Cl(HLC)){
      price <- Cl(HLC)
      lagSeries <- quantmod::Lag(price, n / 2)
    }else if(!is.null(dim(HLC)) && length(dim(HLC)) == 2){
      price <- HLC[,1]
      lagSeries <- stratbuilder2pub:::Lag.matrix(HLC, n / 2)
    }else if(is.numeric(HLC)){
      price <- HLC
      lagSeries <- quantmod::Lag(price, n / 2)
    }else{
      stop('HLC can be matrix or vector or xts')
    }
    N3 <- (runMax(price, n) - runMin(price, n))/n
    N1 <- (runMax(price, n/2) - runMin(price, n/2))/(n/2)
    N2 <- (runMax(lagSeries, n/2) - runMin(lagSeries, 
                                           n/2))/(n/2)
  }
  
  
  dimen <- (log(N1 + N2) - log(N3))/log(2)
  w <- log(2/(SC + 1))
  oldAlpha <- exp(w * (dimen - 1))
  oldN <- (2 - oldAlpha)/oldAlpha
  newN <- ((SC - FC) * (oldN - 1)/(SC - 1)) + FC
  alpha <- 2/(newN + 1)
  alpha[which(alpha > 1)] <- 1
  alpha[which(alpha < w)] <- w
  alphaComplement <- 1 - alpha
  initializationIndex <- index(alpha[is.na(alpha)])
  alpha[is.na(alpha)] <- 1
  alphaComplement[is.na(alphaComplement)] <- 0
  FRAMA <- rep(0, length(price))
  FRAMA[1] <- price[1]
  FRAMA <- DSTrading:::computeFRAMA(alpha, alphaComplement, FRAMA, price)
  if(is.xts(price)){
    FRAMA <- xts(FRAMA, order.by = index(price))
  }
  FRAMA[initializationIndex] <- alpha[initializationIndex] <- NA
  out <- cbind(FRAMA = FRAMA)
  return(out)
}
{
  this <- modelStrategy()
  setBeta(this, pryr::partial(function(data, w, ...) return(w), w = beta, .lazy = FALSE))
  setBetasInt(this, FALSE)
  setCommission(this, quote(
    pmin(pmax(abs(pos_change) * 0.0075, 1), abs(0.01 * data_raw[i,] * pos_change)) + data_raw[i,] * abs(pos_change) * 0.00025
  ))
  addIndicator(this, args = list(name = EMA, x = quote(spread), n = 10), as = 'ema_10')
  addIndicator(this, args = list(name = SMA, x = quote(spread), n = 2), as = 'sma_2')
  addIndicator(this, args = list(name = FRAMA, HLC = quote(spread), n = 10), as = 'fr_10')
  addIndicator(this, args = list(name = EMA, x = quote(spread), n = 30), as = 'ema_30')
  addIndicator(this, args = list(name = EMA, x = quote(spread), n = 140), as = 'ema_140')
  addIndicator(this, args = list(name = volatility, OHLC = quote(spread), n =30), as = 'vol')
  addIndicator(this, args = list(name = BBands,  HLC = quote(spread), maType = EMA, sd=0.1, n = 230), as = "B")
  addRule(this, as = 'long',
          condition = fr_10 > ema_30 &
            ema_30 > ema_140 & fr_10 > ema_30, #&
          #vol > EMA(vol, n = 10),
          type = 'enter',
          #pathwise = TRUE,
          side = -1,
          oco = 'short'
  )
  addRule(this, as = 'short',
          condition = fr_10 < ema_30 &
            ema_30 < ema_140,
          #vol >  EMA(vol, n = 10),
          type = 'enter',
          side = 1,
          oco = 'long'
  )
  addRule(this, as = 'short_ext',
          condition = fr_10[iii] > ema_30[iii],
          pathwise = TRUE,
          type = 'exit',
          oco = 'long'
  )
  addRule(this, as = 'long_ext',
          condition = fr_10[iii] < ema_30[iii],
          pathwise = TRUE,
          type = 'exit',
          oco = 'short'
  )
  # addRule(this, as = 'long_take_profit',
  #         condition = sum(unrealized_money_last) > 175,
  #         type = 'exit',
  #         pathwise = TRUE,
  #         oco = 'short'
  # )
  # addRule(this, as = 'short_take_profit',
  #         condition = sum(unrealized_money_last) > 175,
  #         type = 'exit',
  #         pathwise = TRUE,
  #         oco = 'long'
  # )
  # addRule(this, as = 'long_take_profit',
  #         condition = sum(unrealized_money_last) < -150,
  #         type = 'exit',
  #         pathwise = TRUE,
  #         oco = 'short'
  # )
  # addRule(this, as = 'short_take_profit',
  #         condition = sum(unrealized_money_last) < -15,
  #         type = 'exit',
  #         pathwise = TRUE,
  #         oco = 'long'
  # )
  # 
  # addRule(this, as = 'long_take_profit',
  #         condition = sum(unrealized_money_last) > 60,
  #         type = 'exit',
  #         pathwise = TRUE,
  #         oco = 'short'
  # )
  # addRule(this, as = 'short_take_profit',
  #         condition = sum(unrealized_money_last) < -30,
  #         type = 'exit',
  #         pathwise = TRUE,
  #         oco = 'long'
  # )
  
  addProgramPart(this, 
                 'test',
                 list(
                   data = quote({
                     modelD$log <- log(coredata(modelD$data_adj))
                   }),
                   # on_start = quote({
                   #   if (sum(modelD$dividends[i,]) > 0)
                   #   {
                   #     modelD$data_raw[1:(i - 1),] <- sweep(modelD$data_raw[1:(i - 1),], 2, modelD$dividends[i,],  '-') 
                   #     eval(eval(getProgramParts(this)[['data']]))
                   #     updateTrigger <<- TRUE
                   #   }
                   # }),
                   benter = quote({
                     beta_last <<-  beta_price_weights(data_raw[i, ], w = beta_spread_last) %>%
                       Beta_round
                   })
                   # unrealized_money_last = quote({
                   #   unrealized_money_last <<- unrealized_money_last + (data_raw[i,] - data_last + modelD$dividends[i,])*
                   #     side_last*mult_last*beta_last
                   # })
                 )
  )
}
{
  paramset <- "TEST"
  deleteParamset(this, paramset)
  #distributions
  {
    addDistribution(this,
                    paramset.label = paramset, 
                    component.type = 'indicators', 
                    component.label = 'fr_10', 
                    variable = list(n = seq(3, 70, 1)), 
                    label = 'n.small'
    )
    addDistribution(this,
                    paramset.label = paramset, 
                    component.type = 'indicators', 
                    component.label = 'ema_30', 
                    variable = list(n = seq(10, 250, 1)), 
                    label = 'n.medium'
    )
    addDistribution(this,
                    paramset.label = paramset, 
                    component.type = 'indicators', 
                    component.label = 'ema_140', 
                    variable = list(n = seq(50, 400, 1)), 
                    label = 'n.large'
    )
    addDistributionConstraint(this, 
                              paramset.label = paramset,
                              expr = n.small < n.medium, 
                              label = 'constr1'
    )
    addDistributionConstraint(this, 
                              paramset.label = paramset,
                              expr = n.medium < n.large, 
                              label = 'constr2'
    )
  }
}
{
  setUserData(this, downloaded)
  addData(this, dividends=dividends)
  this$thisEnv$spreadData <- 'log'
  this$thisEnv$betaData <- 'data_raw'
  setMoney(this, 10000)
}

#session <- ssh::ssh_connect('rvoikin@142.93.143.142', keyfile = '/home/ruslan/Загрузки/RSA')
performServer(this, start_date='2011-01-01', end_date='2018-01-01')
plotPnL(this, comOn = FALSE)
plotStrategy(this)
getReportStrategy(this)
applyParamsetServer(this,session = session, nsamples = 100, start_date='2011-01-01', end_date='2018-01-01')
getBacktestResults(this) %>% View()
A <- performServer(this,session, paramset.label = paramset,start_date='2017-01-01',end_date='2018-01-01', paramset.index = c(5052081,4430582))
plotPnL(A, on_percentage = TRUE)
plotCapital(this)
getPnL(A, start_date = '2010-01-01', end_date = '2012-01-01')

A<-modelPortfolio(performServer(this,session = session, paramset.label = paramset, end_date='2018-01-01', paramset.index = c(5052081,4430582,4800178)))
performServer(A, session,start_date='2009-01-01', end_date='2018-01-01')
plotPnL(A)
getReportStrategy(A)
applyParamsetServer(this,session, nsamples = 2, start_date='2009-01-01', end_date='2018-01-01')
getBacktestResults(this) %>% View()




#plotPosition <- function(){

start <- rez$ind.start
stop <- rez$ind.end
side <- rez$side
start[start*side>0]
report <- getReportTrades(this) %>%
  dplyr::mutate(ind = 1:(dplyr::n()))
spread <- log(this$thisEnv$data_from_user)[,1]*beta[1] + log(this$thisEnv$data_from_user)[,2]*beta[2]
plot(spread[start])


plot.xts(cbind(spread[start], spread[start]), pch = 2, type = c('l', 'p'), plot.type = 's')
par(new = TRUE)







dates <- getDateByIndex(this)
df <- cbind( 
  data.frame(date=dates), 
  data.frame(PnL = log(this$thisEnv$data_from_user)[,1]*beta[1] + log(this$thisEnv$data_from_user)[,2]*beta[2]))
library(ggplot2)
df <- df %>%set_colnames(c('a','b'))
rez$side
plotly::ggplotly(ggplot(df, aes(a,b)) + geom_line(size = 0.4) +
                   geom_point(data = df[start[start*side>0],], aes(a,b), shape = 24, color='green', size = 2) + 
                   geom_point(data = df[stop[stop*side>0],], aes(a,b), shape = 25, color='green', size = 2) + 
                   geom_point(data = df[start[start*side<0],], aes(a,b), shape = 24, color='red', size = 2) + 
                   geom_point(data = df[stop[stop*side<0],], aes(a,b), shape = 25, color='red', size = 2)
)
#     scale_color_manual(values=c('green','red')) +
#   p <- ggplot(dataFrame(spread), aes(abs(rets),var, group = rets < 0 )) +
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
# }
#}
