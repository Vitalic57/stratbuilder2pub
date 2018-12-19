library(stratbuilder2pub)
library(TTR)
session <- ssh_connect('YOUR ADDRESS', keyfile = 'PATH TO KEY') 

# In this example we will create list of models. Rules and indicators will be the same, but data will be diffrent

# this function creates our model.
model <- function(){
  this <- modelStrategy() 
  setBeta(this, function(data, ...) 1)
  setLookback(this, 0)
  setLookForward(this, 1000000) 
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
  setMoney(this, 100000)
  return(this)
}

# here we list all available assets from Russia dataset
assets <- stratbuilder2pub:::datasets[['Russia']]

# define list of models
models <- list()
for(x in assets){
  models[[x]] <- model()
  setUserData(models[[x]], list(dataset = 'Russia',
                         assets = x,
                         period = 'day',
                         time = 14))
}

# let's perform
x <- performServer(list(models[[1]], a = models[[2]], models[[3]]), session)

# also we can define distribution for list of models
{
  paramset <- "TEST"
  deleteParamset(models, paramset)
  #distributions
  {
    addDistribution(models,
                    paramset.label = paramset,
                    component.type = 'lookback', # here also can be lookforward, beta (functions)
                    variable = list(n = c(50, 100, 150, 200, 250, 300, 500)),
                    label = 'lookback'
    )
    
    addDistribution(models,
                    paramset.label = paramset,
                    component.type = 'lookforward', # here also can be lookforward, beta (functions)
                    variable = list(n = seq(30, 200, 10)),
                    label = 'lookforward'
    )
    
    
    addDistributionConstraint(models,
                              paramset.label = paramset,
                              expr = lookforward * 2 < lookback,
                              label = 'cond1'
    )
  }
}


# this function will be executed for each model in list on backend
x <- applyParamsetServer(models[1], 
                         session = session,
                         paramset.label = paramset,
                         nsamples = 30)


# Now lets find the best model according to sharpe ratio
library(magrittr)
res <- sapply(x, function(it){
  it %>%
    dplyr::arrange(dplyr::desc(sharpe.ann)) %>%
    head(1) %>%
    .[,'index']
})

# And now put results to perform 
xx <- performServer(models, session, paramset.label = paramset, paramset.index = res)







