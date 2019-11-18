library(stratbuilder2pub)
library(quantmod)
library(magrittr)

# This command should be executed only once when you set up docker. 
session <- ssh_connect("test_backtest_user@142.93.143.142", "/home/vitaly/Documents/ilia")
addDocker(session = session,
          path = '/home/dkazanchyan/stratbuilder2pub Docker/Dockerfile',
          'test1' # name of docker container
          )

# download Coca cola and Pepsi adjusted prices
downloaded <- lapply(c('KO', 'PEP'), function(x){
  getSymbols(x, from = '2000-01-01', auto.assign = FALSE, period = 'week') %>% Ad 
}) %>%
  Reduce('cbind', .) %>%
  na.omit


# define weights 
beta <- c( 0.5, -0.5)


#strat 
{
  this <- modelStrategy()
  setBeta(this, pryr::partial(function(data, w, ...) return(w), w = beta, .lazy = FALSE))
  
  setPyModel(this,
             pyfile = '/home/dkazanchyan/stratbuilder2pub Docker/model.py', # file where Model class is defined
             dockername = 'test1', # name of docker container
             lookback_init = 2, # how many period is needed for initialization of model
             lookback_step = 2, # how many periods is needed on each step
             as = 'signal', 
             args = list(prev = 0, current = 1)
  )
  addRule(this, as = 'short',
          condition = signal < 0.5, # now signal variable can be used in rules
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
                     print(this$thisEnv$paramsets$TEST$distributions)
                     modelD$log <- log(coredata(modelD$data_raw))
                   })
                 )
  )
}

paramset = "TEST"

addDistribution(this, 
                paramset.label = paramset, 
                component.type = 'pymodel', 
                variable = list(prev = c(0, 1, 2)), 
                label = 'prev')

addDistribution(this, 
                paramset.label = paramset, 
                component.type = 'pymodel', 
                variable = list(current = c(0, 1, 2)), 
                label = 'current')






#set data to model
{
  setUserData(this, downloaded)
  this$thisEnv$spreadData <- 'log'
  this$thisEnv$betaData <- 'data_raw'
  setMoney(this, 160000)
}


applyParamsetServer(this, paramset.label = paramset)
getBacktestResults(this) %>% View()



performServer(this, session = session)


this1 <- this
plotPnL(this)
getReportStrategy(this)
this$thisEnv$modelD$data_raw %>% plot
