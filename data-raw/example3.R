library(stratbuilder2pub)
library(TTR)

# Example of usage for multiple asset and rebalancing of portfolio
{
  this <- modelStrategy() 
  setLookback(this, 600) # how many timeframes to look back
  setLookForward(this, 50) # this amount of timeframes coeffincients will be unchanged
  setBeta(this, function(data, ...){ # dots are requared arguments, data is a matrix, that includes lookback + 1 rows
    # Here we define how we will calculate coefficients
    # We will do that with help of linear regression
    colnames(data) <-  c('x', 'y')
    # define model
    model <- lm(y~x, data.frame(data))
    # get coefs
    beta <- c(1, -coefficients(model)[2])
    # return coefs, program automatically round them, you can cancel this behavior with function setBetasInt(this, FALSE),
    # but you have to round them by youself, if you don't do that, program will work incorrectly
    return(beta)
  }) 
  setIgnorePosition(this, TRUE) # If it is TRUE, then after lookforward timeframes open positions will be closed and betas will be recalculated
  addIndicator(this, args = list(name = SMA, x = quote(spread), n = 100), as = 'ema',
               lookback = 600) 
  addRule(this, as = 'short', 
          condition = spread > ema, 
          type = 'enter',
          side = -1,
          oco = 'short'
  )
  
  addRule(this, as = 'long', 
          condition = spread < ema,
          type = 'enter',
          side = 1,
          oco = 'long'
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
}

setUserData(this, list(dataset = 'Russia', 
                       assets = c('GAZP', 'LKOH'), 
                       period = 'day', 
                       time = 13)) 


performServer(this)

#backtesting params
{
  paramset <- "TEST"
  deleteParamset(this, paramset)
  #distributions
  {
    addDistribution(this,
                    paramset.label = paramset,
                    component.type = 'lookback', # here also can be lookforward, beta (functions)
                    variable = list(n = c(50, 100, 150, 200, 250, 300, 500)),
                    label = 'lookback'
    )
    addDistribution(this,
                    paramset.label = paramset,
                    component.type = 'indicator', 
                    component.label = 'ema',
                    variable = list(n = c(50, 100, 150, 200, 250, 300, 500)),
                    label = 'ema.n'
    )
    # This distribution for arguments of rules
    # addDistribution(this,
    #                 paramset.label = paramset,
    #                 component.type = 'rule',
    #                 component.label = 'short',
    #                 variable = list(k = seq(30, 200, 10)),
    #                 label = 'var1'
    # )
    
    addDistribution(this,
                    paramset.label = paramset,
                    component.type = 'lookforward', 
                    variable = list(n = seq(30, 200, 10)),
                    label = 'lookforward'
    )
    
    
    addDistributionConstraint(this,
                              paramset.label = paramset,
                              expr = lookforward * 2 < lookback,
                              label = 'cond1'
    )
  }
}

applyParamsetServer(this, nsamples = 10)
performServer(this, paramset.index = 18)



