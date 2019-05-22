library(stratbuilder2pub)
library(TTR)

# Example of usage for one asset
# Strategy goes long if spread is less then moving average with 100 window and goes short else
{
  this <- modelStrategy() # Create strategy  object
  addIndicator(this, args = list(name = EMA, x = quote(spread), n = 100), as = 'ema') # Here we add indicator, name argument should be function for now, other arguments in args list are arguments of
  # your function. spread is local name, it is name of process. 
  # as argument is responsible for name of your indicator, this name can be used in rules and other indicators
  addRule(this, as = 'short',  # Here we create a rule for short
          condition = spread > ema, # your trigger, spread is local name of process and ema name of moving average indicator
          type = 'enter', # There are only two types enter or exit.
          side = -1, # There are 2 directions 1(long) or -1(short)
          oco = 'short' # This is namespace for rule, we will need it later
  )
  
  addRule(this, as = 'long', # This is another rule, now it is for going long
          condition = spread < ema,
          type = 'enter',
          side = 1,
          oco = 'long'
  )
  addRule(this, as = 'short_exit', # This rule for exiting from position
          condition = spread < ema, # trigger when to exit
          type = 'exit', # the second and the last type of rules
          oco = 'short' # and here namespace, after which entering rule we exit
  )
  addRule(this, as = 'long_exit', # This rule for exiting from long position
          condition = spread > ema,
          type = 'exit',
          oco = 'long'
  )
  setMoney(this, 100000)
}


setUserData(this, list(dataset = 'Russia', # There is only one dataset for now
                       assets = 'GAZP', # Assets, that will take part in backtest
                       period = 'day', # there are 2 available period hour and day
                       time = 13)) # if period equals to day, then you can specify time when you strategy will be traded

performServer(this) # run this function to do backtest. Results will be saved in you remote acc

getReportStrategy(this) 

plotPnL(this)











