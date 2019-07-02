library(stratbuilder2pub)
library(quantmod)

# This command should be executed only once when you set up docker. 
#session <- ssh_connect("test_backtest_user@142.93.143.142", "/home/vitaly/Documents/ilia")
# addDocker(path = '/home/vitaly/Documents/app/Dockerfile',
#           'test1' # name of docker container
# )

# download Coca cola and Pepsi adjusted prices
downloaded <- lapply(c('KO', 'PEP'), function(x){
  getSymbols(x, from = '2000-01-01', auto.assign = FALSE) %>% Ad
}) %>%
  Reduce('cbind', .) %>%
  na.omit

# define weights 
beta <- c( 0.5, -0.5)

#strat 
{
  this <- modelStrategy()
  setBeta(this, pryr::partial(function(data, w, ...) return(w), w = beta, .lazy = FALSE))
  setLookForward(this, 100)
  setPyModel(this,
             pyfile = '/home/vitaly/Documents/app/model_1.py',
             dockername = 'test1',
             lookback_init = 1500,
             lookback_step = 0,
             lookback = 100,
             vector_step = TRUE,
             data = quote(cbind(
               x1 = as.numeric(spread - EMA(spread, 5)),
               x2 = as.numeric(spread - EMA(spread, 10)),
               x3 = as.numeric(spread - EMA(spread, 20)),
               x4 = as.numeric(spread - EMA(spread, 50)),
               y = as.logical(Diff(spread, -1) > 0)
             )),
             args = list(n_estimators = 100),
             as = 'signal'
  )
  addRule(this, as = 'short',
          condition = signal < 0.5,
          type = 'enter',
          side = -1,
          oco = 'short'
  )
  addRule(this, as = 'long',
          condition = signal > 0.5,
          type = 'enter',
          side = 1,
          oco = 'long'
  )
  addRule(this, as = 'long_ext',
          condition = signal < 0.5,
          type = 'exit',
          oco = 'long'
  )
  addRule(this, as = 'short_ext',
          condition = signal > 0.5,
          type = 'exit',
          oco = 'short'
  )
  addProgramPart(this, 
                 'test',
                 list(
                   data = quote({
                     modelD$log <- log(coredata(modelD$data_raw))
                   })
                 )
  )
}

paramset = "TEST"
deleteParamset(this, paramset)
addDistribution(this, 
                paramset.label = paramset, 
                component.type = 'pymodel', 
                variable = list(n_estimators = c(50, 100, 150)), 
                label = 'n_estimators')



#set data to model
{
  setUserData(this, downloaded)
  this$thisEnv$spreadData <- 'log'
  this$thisEnv$betaData <- 'data_raw'
  setMoney(this, 160000)
}

# For evaluation with current params
performServer(this)
plotPnL(this)
getReportStrategy(this)

# For evaluation of paramset 
applyParamsetServer(this, paramset.label = paramset)
getBacktestResults(this) %>% View()





