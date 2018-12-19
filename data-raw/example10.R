# EXAMPLE OF CREATING PORTFOLIO OF STRATEGIES
library(stratbuilder2pub)
library(TTR)

session <- ssh_connect('YOUR ADDRESS', keyfile = 'PATH TO KEY') 

# DEFINE MODEL
model <- function(n){
  this <- modelStrategy() 
  addIndicator(this, args = list(name = EMA, x = quote(spread), n = n), as = 'ema',
               lookback = n) 
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
  setMoney(this, 100000)
  return(this)
}



# Create 2 models with diffrent data
this1 <- model(100)
setUserData(this1, list(dataset = 'Russia',
                        assets = 'GAZP', 
                        period = 'day', 
                        time = 13))
this2 <- model(100)
setUserData(this2, list(dataset = 'Russia',
                        assets = 'LKOH', 
                        period = 'day', 
                        time = 13))

# create named list
models <- list(GAZP = this1, LKOH = this2)
# create portfolio from list
port <- modelPortfolio(models)
# also you can do it this way
# port <- modelPortfolio(GAZP = this1, LKOH = this2)

# Other things with simulation is working as with modelStrategy object
x <- performServer(port, session, verbose=TRUE, start_date='2016-01-01') 

#backtesting params
{
  paramset <- "TEST"
  deleteParamset(port, paramset)
  #distributions
  {
    addDistribution(port,
                    paramset.label = paramset,
                    component.type = 'lookback', # here also can be lookforward, beta (functions)
                    variable = list(n = c(50, 100, 150, 200, 250, 300, 500)),
                    label = 'lookback'
    )
    
    addDistribution(port,
                    paramset.label = paramset,
                    component.type = 'lookforward', 
                    variable = list(n = seq(30, 200, 10)),
                    label = 'lookforward'
    )
    
    
    addDistributionConstraint(port,
                              paramset.label = paramset,
                              expr = lookforward * 2 < lookback,
                              label = 'cond1'
    )
  }
}

x <- applyParamsetServer(list(this, this), 
                         session = session,
                         paramset.label = paramset,
                         nsamples = 10)

performServer(port, session, paramset.label = paramset, paramset.index = c(14, 18))








