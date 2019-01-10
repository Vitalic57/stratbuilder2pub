library(stratbuilder2pub)
library(TTR)

session <- ssh_connect('YOUR ADDRESS', keyfile = 'PATH TO KEY') # create session

# Example of usage for one asset
# Strategy goes long if spread is less then moving average with 100 window and goes short else
# And we add simple additional condiction: we go long if fast ema is less then slow ema and vice versa
# we go short if fast ema is more then slow ema
{
  this <- modelStrategy() # Create strategy  object
  addIndicator(this, args = list(name = SMA, x = quote(spread), n = 100), as = 'sma_fast',
               lookback = 100) 
  # as argument is responsible for name of your indicator, this name can be used in rules and other indicators
  addIndicator(this, args = list(name = SMA, x = quote(spread), n = 200), as = 'sma_slow',
               lookback = 200)
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

x <- performServer(this, session)
# It will return report and draw a pnl graph

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


xx <- applyParamsetServer(this, 
                    session = session,
                    paramset.label = paramset,
                    nsamples = 50 # how many example to get from paramset
)
# it will return data.frame of results
print(xx)

# pick some of them
x6 <- performServer(this, session, paramset.label = paramset, paramset.index = 6)


# Now about content of the first report
# trades                     # number of trades 
# trades.year                # mean number of trades yearly
# long.trades                # number of long trades
# short.trades               # number of short trades
# long.acc                   # percent of winning long trades
# short.acc                  # percent of winning short trades
# total.acc                  # percent of winning trades
# max.loose                  # max number of loosing trades in a row
# max.win                    # max number of winning trades in a row
# return.ann                 # yearly return on getMoney(this)
# return.avg                 # yearly return on average capital
# return.pos.drawdown        # return on (maximum capital in trades plus maximum drawdown)
# return.pos.drawdown.95     # return on (95 quantile of capital in trades plus maximum drawdown)
# return.pos.drawdown.c95    # return on (mean capital excedding 95 quantile of capital in trades plus maximum drawdown)
# drawdown.money             # maximum drawdown divided by maximum amount of money in trade
# median                     # median return from one trade
# in.pos                     # percents of time in position
# in.pos.positive            # percents of time in profit while in trade
# days.in.pos.max            # maximum number of days in trade
# days.in.pos.mean           # mean number of days in trade
# days.out.pos.max           # maximum number of days out of trade
# days.out.pos.mean          # mean number of days out of trade
# sharpe.ann                 # annual Sharpe ratio
# sortino.ann                # annual Sortino ratio
# straight.m                 # deviation of money from straight line
# straight.t                 # deviation of money from trades from straight line
# maxMAE                     # this field is not working now
# profit.drawdown.year       # profit divided by drawdown yearly

# the second report contains information for each year for subset of above statistics

# the third report contains information for each trade of strategy we have created

# ind.start                 # index when trade started               
# ind.end                   # index when trade ended                
# date.start                # date when trade started
# date.end                  # date when trade ended 
# side                      # side from rule                  
# position.GAZP.Adjusted    # maximum position on that asset in trade                
# price.start.GAZP.Adjusted # price when trade started          
# price.end.GAZP.Adjusted   # price when trade ended             
# pnl.asset.GAZP.Adjusted   # profit and loss for asset without commissions           
# com.asset.GAZP.Adjusted   # commission for asset                 
# com.sum                   # sum of commission for each asset                  
# pnl.sum                   # sum of profit and loss for each asset            
# pnl.sum.adj               # pnl.sum minus com.sum            
# spread.price              # max margin in trade          
# MAE.with.com              # Maximum Adverse Excursion minus com.sum             
# MFE.with.com              # Maximum Favorable Excursion minus com.sum            
# return.from.money         # pnl.sum.adj / money in model  





