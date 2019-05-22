library(stratbuilder2pub)
library(TTR)
library(xts)

# Example of usage for multiple asset and table of betas
{
  this <- modelStrategy() 
  setLookback(this, 0) 
  setLookForward(this, 10000000)
  setWaitAfterClose(this, TRUE) 
  addIndicator(this, args = list(name = SMA, x = quote(spread), n = 100), as = 'ema',
               lookback = 101) 
  addRule(this, as = 'short', 
          condition = spread > ema, 
          type = 'enter',
          side = -1,
          oco = 'short', 
          osFun = stratbuilder2pub:::sameMoneyOs, 
          osFun_args = alist(amount = getMoney(this))
  )
  
  addRule(this, as = 'long', 
          condition = spread < ema,
          type = 'enter',
          side = 1,
          oco = 'long',
          osFun = stratbuilder2pub:::sameMoneyOs,
          osFun_args = alist(amount = getMoney(this))
  )
  addRule(this, as = 'short_exit',
          condition = spread < ema, 
          type = 'exit',
          oco = 'short'
  )
  addRule(this, as = 'long_exit', 
          condition = spread > ema,
          type = 'exit',
          oco = 'long'
  )
  this$thisEnv$spreadData <- 'data_raw' 
  
  this$thisEnv$betaData <- 'data_raw'
}

setUserData(this, list(dataset = 'Russia', 
                       assets = c('GAZP', 'LKOH'), 
                       period = 'day', 
                       time = 13))

# here you can define your coefficients
setBetaTable(this, xts(data.frame(GAZP = c(1,2,3,4,5), LKOH = -c(5,4,3,2,1)), as.Date('2011-01-01') + 0:4 * 365 ))
# so here we set coefficients for 5 dates. Between them will be coefficients from last date. For example,
# in 2011-06-01 GAZP = 1, LKOH = -5, everywhere after 2014-12-31 coefficients will be c(5, -1)
# Before 2011-01-01 coefficients will be zeros

performServer(this)


