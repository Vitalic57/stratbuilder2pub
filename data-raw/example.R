library(stratbuilder2pub) # this downloads package for building models
library(TTR) # this package contains functions for creating indicators from technical analysis

session <- ssh_connect('YOUR ADDRESS', keyfile = 'PATH TO KEY') # create session

# Example of usage for one asset
# Strategy goes long if asset is less then its moving average with 100 window and goes short else
{
  this <- modelStrategy() # Create strategy  object
  addIndicator(this, args = list(name = SMA, x = quote(spread), n = 100), as = 'my_sma',
               lookback = 100) # Here we add indicator to our model
  # There are multiple names in args:
  # * 'name' is where you define your function, that will be an indicator. It should be able to get matrix and return matrix as well 
  # * 'x' is the first argument of function EMA from package TTR. You can use ?EMA command and read about its arguments.
  # * 'n' is the second argument of function EMA.
  # So we see that x = quote(spread). quote is function from base R. It leaves expressions inside it unevaluated. 
  # spread in this example will be just timseries of close prices for one asset. This name will be defined when 
  # construction of model will be completed, now R does not know anything about that variable, so we need to use
  # quote(spread). This command makes possible to call function with arguments, when all will be defined.  
  # 'as' argument is responsible for name of your indicator, this name can be used in rules and other indicators
  # 'lookback' argument always is numeric. It should be more or equal to number of periods that necessary for calculation
  # your indicator. For example, if you want to calculate SMA (simple moving average) with window 10, than data should contain 10 points
  # at least, so lookback is equal to 10.
  addRule(this, # Here we create a rule for short
          as = 'spread_more_sma',  # this your name for this rule
          condition = spread > my_sma, # your trigger, spread is local name for close price and my_sma name of moving average indicator
          type = 'enter', # There are only two types enter or exit.
          side = -1, # There are 2 directions 1(long) or -1(short)
          oco = 'short' # This is namespace for rule, we will need it later. This can be any name for your choice
  )
  
  addRule(this, # This is another rule, now it is for going long
          as = 'spread_less_sma', 
          condition = spread < my_sma,
          type = 'enter',
          side = 1,
          oco = 'long'
  )
  addRule(this, # This rule for exiting from position
          as = 'short_exit', 
          condition = spread < my_sma, # trigger when to exit
          type = 'exit', # the second and the last type of rules
          oco = 'short' # and here namespace, after which entering rule we exit
  )
  addRule(this,  # This rule for exiting from long position
          as = 'long_exit',
          condition = spread > my_sma,
          type = 'exit',
          oco = 'long'
  )
  setMoney(this, 100000) # how much money you want to invest in this strategy. 
}


# This function is needed for selection of data that will be used for backtesting.
setUserData(this, list(dataset = 'Russia', 
                       assets = 'GAZP', # Assets, that will take part in backtest
                       period = 'day', # there are 2 available period hour and day for Russia dataset
                                      # to get available datasets enter getDatasets()
                       time = 13)) # if period equals to day and dataset have intraday data, 
                                   # then you can specify time when you strategy will be traded


x <- performServer(this, session) # run this function to do backtest. Results will be saved in you remote acc
# variable x is a list with results.
# x[[1]] -- full report
# x[[2]] -- report for each year
# x[[3]] -- report of trades

View(x[[3]])

# In the second example we will discuss reports 



















