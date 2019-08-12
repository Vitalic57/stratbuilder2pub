library(stratbuilder2pub)
library(TTR)

session <- ssh_connect('YOUR ADDRESS', keyfile = 'PATH TO KEY') # create session

# Example of usage for one asset
# Strategy goes long if spread is less then moving average with 100 window and goes short else
# And we add simple additional condiction: we go long if fast ema is less then slow ema and vice versa
# we go short if fast ema is more then slow ema
{
  this <- modelStrategy() # Create strategy  object
  setBeta(this, function(...) 1) # create rule for rebalancing, here coefficient is always one
  setLookback(this, 1) # how many periods you need for computing beta 
  setLookForward(this, 1000000) # how many periods you don't need to rebalance
  setWaitAfterClose(this, TRUE) # if TRUE, then algorithm don't do revert
  addIndicator(this, args = list(name = SMA, x = quote(spread), n = 100), as = 'sma_fast') # Here we add indicator, name argument should be function for now, other arguments in args list are arguments of
  # your function. spread is local name, it is name of process. 
  # as argument is responsible for name of your indicator, this name can be used in rules and other indicators
  addIndicator(this, args = list(name = SMA, x = quote(spread), n = 200), as = 'sma_slow')
  addRule(this, as = 'short',  # Here we create a rule for short
          condition = spread > sma_fast & sma_fast > sma_slow, # your trigger, spread is local name of process and ema name of moving average indicator
          type = 'enter', # There are only two types enter or exit.
          side = -1, # There are 2 directions 1(long) or -1(short)
          oco = 'short' # This is namespace for rule, we will need it later
  )
  
  addRule(this, as = 'long', # This is another rule, now it is for going long
          condition = spread < sma_fast & sma_fast < sma_slow,
          type = 'enter',
          side = 1,
          oco = 'long'
  )
  addRule(this, as = 'short_exit', # This rule for exiting from position
          condition = spread < sma_fast, # trigger when to exit
          type = 'exit', # the second and the last type of rules
          oco = 'short' # and here namespace, after which entering rule we exit
  )
  addRule(this, as = 'long_exit', # This rule for exiting from long position
          condition = spread > sma_fast,
          type = 'exit',
          oco = 'long'
  )
}




setUserData(this, list(dataset = 'Russia', # There is only one dataset for now
                       assets = 'GAZP', # Assets, that will take part in backtest
                       period = 'day', # there are 2 available period hour and day
                       time = 13)) # if period equals to day, then you can specify time when you strategy will be traded


performServer(this)

plotPnL(this)

# Now if you want to fit you model you can define distributions and constraints on them and run 
#  function applyParamsetServer to make search



#distribution
{
  paramset <- "TEST"
  deleteParamset(this, paramset)
  #distributions
  {
    addDistribution(this,
                    paramset.label = paramset,          # label of paramset
                    component.type = 'indicators',      # here can be indicators, rules, params, pms(positionManager's args)
                    component.label = 'sma_slow',            # name of component.type
                    variable = list(n = seq(50, 200, 10)), # list, name of variable should be equal to array of values
                    label = 'n.slow' # name of distribution, you can pass it to distribution constraints
    )
    
    addDistribution(this,
                    paramset.label = paramset,     
                    component.type = 'indicators',     
                    component.label = 'sma_fast',            
                    variable = list(n = seq(10, 150, 10)), 
                    label = 'n.fast' 
    )
    
    addDistributionConstraint(this, 
                              paramset.label = paramset,
                              expr = n.fast < n.slow, # here we use name of distributions, we want that window of slow ema will be more then fast ema's window
                              label = 'constr1' # any name you want
                                )
  }
}


applyParamsetServer(this, 
                    nsamples = 50 # how many example to get from paramset
)

getBacktestResults(this) %>% View

# pick one of them
# paramset.label argument can be omitted
performServer(this, paramset.label = paramset, paramset.index = 237)

getReportStrategy(this)
# Now about content of report
# trades                   48.0000 # number of trades 
# trades.year               6.0000 # mean number of trades yearly
# long.trades              26.0000 # number of long trades
# short.trades             22.0000 # number of short trades
# long.acc                  0.8077 # percent of winning long trades
# short.acc                 0.9545 # percent of winning short trades
# total.acc                 0.8750 # percent of winning trades
# max.loose                 1.0000 # max number of loosing trades in a row
# max.win                  14.0000 # max number of winning trades in a row
# return.ann                0.1556 # yearly return on getMoney(this)
# return.avg                0.3450 # yearly return on average capital
# return.pos.drawdown       0.0977 # return on (maximum capital in trades plus maximum drawdown)
# return.pos.drawdown.95    0.1130 # return on (95 quantile of capital in trades plus maximum drawdown)
# return.pos.drawdown.c95   0.1097 # return on (mean capital excedding 95 quantile of capital in trades plus maximum drawdown)
# drawdown.money            0.3009 # maximum drawdown divided by maximum amount of money in trade
# median                    0.0175 # median return from one trade
# in.pos                    0.4683 # percents of time in position
# in.pos.positive           0.3026 # percents of time in profit while in trade
# days.in.pos.max         301.0000 # maximum number of days in trade
# days.in.pos.mean         27.0625 # mean number of days in trade
# sharpe.ann                0.8727 # annual Sharpe ratio
# sortino.ann               1.8969 # annual Sortino ratio
# straight.m                0.0827 # deviation of money from straight line
# straight.t                0.0652 # deviation of money from trades from straight line
# maxMAE                    0.0000 # maximum drawdown in one trade divided by money
# profit.drawdown.year      0.4644 # profit divided by drawdown yearly







