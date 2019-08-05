## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(stratbuilder2pub)
library(TTR)
library(quantmod)
session <- ssh_connect("test_backtest_user@142.93.143.142", keyfile = "/home/vitaly/Documents/ilia")

## ------------------------------------------------------------------------
this <- modelStrategy()

## ------------------------------------------------------------------------
addIndicator(this, args = list(name = EMA, x = quote(spread), n = 20), as = 'fast_ma')
addIndicator(this, args = list(name = EMA, x = quote(spread), n = 100), as = 'slow_ma')
addIndicator(this, args = list(name = EMA, x = quote(spread), n = 250), as = 'very_slow_ma')

## ------------------------------------------------------------------------
addRule(this, 
        condition = fast_ma > slow_ma & Diff(very_slow_ma, 1) < 0, 
        type = 'enter', 
        side = -1,
        oco = 'short' 
)

## ------------------------------------------------------------------------
addRule(this,
        condition = !(fast_ma > slow_ma & Diff(very_slow_ma, 1) < 0), 
        type = 'exit',
        oco = 'short' 
)

## ------------------------------------------------------------------------
addRule(this, 
        condition = fast_ma < slow_ma & Diff(very_slow_ma, 1) > 0,
        type = 'enter',
        side = 1,
        oco = 'long'
)
addRule(this, 
        condition = !(fast_ma < slow_ma & Diff(very_slow_ma, 1) > 0),
        type = 'exit',
        oco = 'long'
)

## ---- warning = FALSE, message = FALSE-----------------------------------
data <- getSymbols('RSX', from = Sys.Date() - 365 * 10, src = 'yahoo', auto.assign = FALSE)
setUserData(this, Ad(data))
setMoney(this, 100000)

## ------------------------------------------------------------------------
performServer(this)

## ---- warning = FALSE, message = FALSE, fig.width=12, fig.height=6-------
plotPnL(this, interactive_plot=FALSE)

## ---- warning = FALSE, message = FALSE, fig.width=12, fig.height=6-------
plotCalendar(this)

## ---- warning = FALSE, message = FALSE, fig.width=12, fig.height=6-------
plotCapital(this, interactive_plot=FALSE)

## ---- warning = FALSE, message = FALSE, fig.width=12, fig.height=6-------
plotReturns(this, "MAE", interactive_plot=FALSE)

## ------------------------------------------------------------------------
head(getReportTrades(this))

## ------------------------------------------------------------------------
getReportStrategy(this)
getReportCalendar(this)

## ------------------------------------------------------------------------
addDistribution(this, 
                component.type = 'indicator', 
                component.label = 'fast_ma', 
                variable = list(n = seq(5, 50, 5)), 
                label = 'fast.n')
addDistribution(this, 
                component.type = 'indicator', 
                component.label = 'slow_ma', 
                variable = list(n = seq(40, 150 , 10)), 
                label = 'slow.n')

## ------------------------------------------------------------------------
addDistributionConstraint(this, 
                          expr = fast.n < slow.n)

## ------------------------------------------------------------------------
addDistribution(this, 
                component.type = 'indicator', 
                component.label = c('fast_ma', 'slow_ma'), 
                variable = list(name = c(EMA, SMA, DEMA)))

## ------------------------------------------------------------------------
applyParamsetServer(this, 
                    nsamples = 5,
                    start_date = '2011-01-01',
                    end_date = '2017-01-01',
                    seed = 42)

## ------------------------------------------------------------------------
getBacktestResults(this)

## ---- warning = FALSE, message = FALSE, fig.width=12, fig.height=6-------
performServer(this,
              paramset.index = 103, 
              start_date = '2011-01-01',
              end_date = '2017-01-01')
getReportStrategy(this)
plotPnL(this, interactive_plot=FALSE)

## ------------------------------------------------------------------------
createMeanRevertingModel <- function(){
  this <- modelStrategy()
  addIndicator(this, args = list(name = BBands, HLC = quote(spread), n = 100, sd = 0.5), as = 'bb')
  addRule(this, 
          condition = bb[,'pctB'] > -1 & 
            Lag(bb[,'pctB'] < -1, 1) &
            bb[,'pctB'] < 0,
          type = 'enter',
          side = 1,
          oco = 'long'
  )
  addRule(this, 
          condition = bb[,'pctB'] > 0,
          type = 'exit',
          oco = 'long'
  )
  
  addRule(this, 
          condition = bb[,'pctB'] < 1 & 
            Lag(bb[,'pctB'] > 1, 1) &
            bb[,'pctB'] > 0 ,
          type = 'enter',
          side = -1,
          oco = 'short'
  )
  addRule(this,
          condition = bb[,'pctB'] < 0,
          type = 'exit',
          oco = 'short'
  )
  
  addRule(this, 
          condition = sum(unrealized_money_last) > 0.0025 * getMoney(this),
          type = 'exit',
          pathwise = TRUE,
          oco = 'short'
  )
  addRule(this,
          condition = sum(unrealized_money_last) > 0.0025 * getMoney(this),
          type = 'exit',
          pathwise = TRUE,
          oco = 'long'
  )
  setMoney(this, 100000)
  return(this)
}

stocks <- c("EWW", "EWC", "EWQ", "EWU", "EWG", "EWI", "RSX", "EWH", "SPY",
            "MCHI", "EWY", "EWJ", "EWZ", "EWP", "EZA", "EWS", "EWA", "INDA")
data <- lapply(stocks, function(x){
  getSymbols(x, from = Sys.Date() - 365 * 10, src = 'yahoo', auto.assign = FALSE) %>% Ad
}) %>% set_names(stocks)

models <- list()
for(x in stocks){
  models[[x]] <- createMeanRevertingModel()
  setUserData(models[[x]], data[[x]])
}

## ---- warning = FALSE, message = FALSE, fig.width=12, fig.height=6-------
performServer(models)
getReportStrategy(models)
plotPnL(models, interactive_plot=FALSE)

## ---- warning = FALSE, message = FALSE, fig.width=12, fig.height=6-------
portfolio <- modelPortfolio(models)
performServer(portfolio)
getReportStrategy(portfolio)
plotPnL(portfolio, interactive_plot=FALSE)

## ------------------------------------------------------------------------
this <- createMeanRevertingModel()
setLookback(this, 500)
setLookForward(this, 10)
setBeta(this, function(data, ...){ # dots are arguments, that we do not use, 
    #data is a matrix, that includes lookback + 1 rows
    # Here we define how we will calculate coefficients
    # We will do that with help of linear regression
    colnames(data) <-  c('x', 'y')
    
    # define the model
    model <- lm(y~x, data.frame(data))
    
    # get coefficients
    beta <- c(1, -coefficients(model)[2])
    
    # return coefs, program automatically round them, you can cancel this behavior with function setBetasInt(this, FALSE),
    # but you have to round them by yourself, if you don't do that, the program will work incorrectly
    return(beta)
}) 

## ------------------------------------------------------------------------
pair <- c("EWH", "EWS")
setUserData(this, data[pair])

## ---- warning = FALSE, message = FALSE, fig.width=12, fig.height=6-------
performServer(this)
getReportStrategy(this)
plotPnL(this, interactive_plot=FALSE)

## ------------------------------------------------------------------------
this <- modelStrategy() 
setLookback(this, 1)
setLookForward(this, 1) 
setIgnorePosition(this, TRUE) 
addRule(this, as = 'long', 
        condition = TRUE,
        type = 'enter',
        side = 1,
        oco = 'long'
)

## ------------------------------------------------------------------------
addProgramPart(this, 
               evolution = list(
                 data = quote({
                   modelD[['RSI']] <- apply(modelD[['data_raw']], 2, RSI, n = 20) 
                 })
               ))
setBetaData(this, 'RSI')

## ------------------------------------------------------------------------
setBeta(this, function(data, ...){ 
  # data here is subset of rows of modelD[['RSI']]
  # we need only the last row
  data <- as.numeric(tail(data, 1) )
  
  # sort the values.
  ord <- order(data)
  
  # create array of coefficients
  beta <- numeric(length(data))
  
  # assign 1 to asset with the lowest RSI 
  beta[head(ord, 1)] <- 1
  
  # assign -1 to asset with the highest RSI 
  beta[tail(ord, 1)] <- -1
  return(beta)
}) 
setBetasByMoney(this, TRUE)

## ------------------------------------------------------------------------
setCommission(this, quote({
  abs(pos_change) * data_raw[i,] * 0.0005
}))

## ---- warning = FALSE, message = FALSE, fig.width=12, fig.height=6-------
setUserData(this, data)
performServer(this)
getReportStrategy(this)
plotPnL(this, interactive_plot=FALSE)

