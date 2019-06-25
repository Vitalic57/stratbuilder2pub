library(stratbuilder2pub)
library(quantmod)

# This command should be executed only once when you set up docker. 
addDocker('path to dockerfile',
          'test' # name of docker container
          )

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
  
  setPyModel(this,
             pyfile = '/home/vitaly/Documents/app/model.py', # file where Model class is defined
             dockername = 'test', # name of docker container
             lookback_init = 1, # how many period is needed for initialization of model
             lookback_step = 1, # how many periods is needed on each step
             as = 'signal' # how to name output of the model
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
                     modelD$log <- log(coredata(modelD$data_raw))
                   })
                 )
  )
}



#set data to model
{
  setUserData(this, downloaded)
  this$thisEnv$spreadData <- 'log'
  this$thisEnv$betaData <- 'data_raw'
  setMoney(this, 160000)
}


performServer(this)

plotPnL(this)
