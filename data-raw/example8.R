library(stratbuilder2pub)
library(TTR)

session <- ssh_connect('YOUR ADDRESS', keyfile = 'PATH TO KEY') # create session

# Strategy from the first example with using python talib library
{
  this <- modelStrategy()
  setBeta(this, function(data, ...) 1)
  setLookback(this, 0) 
  setLookForward(this, 1000000) 
  setWaitAfterClose(this, TRUE) 
  addProgramPart(this, as = 'part1',
                 evolution = list(
                   init = quote({
                     library(reticulate) # this library links r and python
                     # we define it here, 
                     talib <- import('talib') # import talib. This package like TTR in R, but it has some functions, that don't present in TTR
                     np <- import('numpy', convert = FALSE) # If you want to work with talib, you need objects, that talib understands, 
                     # for example, numpy ndarray
                   }),
                   coefs = quote({
                     spread_np <- np$array(as.numeric(spread)) # here we convert R matrix to numeric and then to numpy ndarray
                     # then we add this to indicator
                   })
                  )
                )
  # Pay attention that the second argument is without name. It is the first argument in function talib$SMA
  # and it must be positional. Name argument should be character for functions defined in python.
  addIndicator(this, args = list(name = 'talib$SMA', quote(spread_np), timeperiod = 100), as = 'sma',
               lookback = 101) 
  addRule(this, as = 'short',  
          condition = spread > sma, 
          type = 'enter',
          side = -1,
          oco = 'short',
          osFun = stratbuilder2pub:::sameMoneyOs, 
          osFun_args = alist(amount = getMoney(this)) 
  )
  
  addRule(this, as = 'long', 
          condition = spread < sma,
          type = 'enter',
          side = 1,
          oco = 'long',
          osFun = stratbuilder2pub:::sameMoneyOs,
          osFun_args = alist(amount = getMoney(this))
  )
  addRule(this, as = 'short_exit', 
          condition = spread < sma,
          type = 'exit', 
          oco = 'short' 
  )
  addRule(this, as = 'long_exit', 
          condition = spread > sma,
          type = 'exit',
          oco = 'long'
  )
  this$thisEnv$spreadData <- 'data_raw'
  
  this$thisEnv$betaData <- 'data_raw' 
  setMoney(this, 1000)
}

setUserData(this, list(dataset = 'Russia', 
                       assets = 'GAZP', 
                       period = 'day', 
                       time = 13))


x <- performServer(this, session) 

library(xts)
rollapply(1:100, 10, mean, na.pad = TRUE, align = 'right')

# Also you can do it in another way
# Strategy from the first example with using python talib library
{
  this <- modelStrategy()
  setBeta(this, function(data, ...) 1)
  setLookback(this, 0) 
  setLookForward(this, 1000000) 
  setWaitAfterClose(this, TRUE) 
  addProgramPart(this, as = 'part1',
                 evolution = list(
                   init = quote({
                     library(reticulate) 
                     talib <- import('talib') 
                     np <- import('numpy', convert = FALSE) 
                   }),
                   coefs = quote({
                     spread_np <- np$array(as.numeric(spread))
                     sma <- cbind(talib$SMA(spread_np, 100)) # it should be a matrix , if you want to use
                     # variable from here in rules
                   })
                 )
  )
  # if you want results to be the same you must define maxlookback by yourself
  setMaxLookback(this, 101) # By definition it is equal to maximum of lookbacks of indicators. So in our example it is 101
  addRule(this, as = 'short',  
          condition = spread > sma, 
          type = 'enter',
          side = -1,
          oco = 'short',
          osFun = stratbuilder2pub:::sameMoneyOs, 
          osFun_args = alist(amount = getMoney(this)) 
  )
  
  addRule(this, as = 'long', 
          condition = spread < sma,
          type = 'enter',
          side = 1,
          oco = 'long',
          osFun = stratbuilder2pub:::sameMoneyOs,
          osFun_args = alist(amount = getMoney(this))
  )
  addRule(this, as = 'short_exit', 
          condition = spread < sma,
          type = 'exit', 
          oco = 'short' 
  )
  addRule(this, as = 'long_exit', 
          condition = spread > sma,
          type = 'exit',
          oco = 'long'
  )
  this$thisEnv$spreadData <- 'data_raw'
  
  this$thisEnv$betaData <- 'data_raw' 
  setMoney(this, 1000)
}

setUserData(this, list(dataset = 'Russia', 
                       assets = 'GAZP', 
                       period = 'day', 
                       time = 13))

x <- performServer(this, session) 
