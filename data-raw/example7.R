library(stratbuilder2pub)
library(readxl)
library(xts)
library(magrittr)
library(TTR)

session <- ssh_connect('YOUR ADDRESS', keyfile = 'PATH TO KEY')

# In this example we will download series from xlsx and put it in our backtest


# download sentiment ---------
# this table includes information about sentiment in sber. We will use it.
# We must make xts series to put it in backtest system
data <- read_excel('/home/vitaly/Documents/models/Sentiment/Data/sber_result_table_1.xlsx') %>%
{xts(dplyr::pull(.[,3]), dplyr::pull(.[,1]), tz = '')} %>%
  lag


############################################
# Strategy
{
  this <- modelStrategy()
  
  # we will use daily sberbank data
  setUserData(this, list(dataset = 'Russia', 
                         assets = c('SBER'), 
                         period = 'day', 
                         time = 13))
  
  #Here you define your data: sent -- will be name for data in the model, it will be stored at this$thisEnv$modelD$sent. 
  # It will be reshaped for you (the size of sent and data_raw will be equal) and it will be matrix not a xts.
  addData(this, sent = data)
  
  
  setParams(this,
            type= 'pp',
            args = list(
              com_coef = 4
            ))
  setParams(this,
            type= 'rules',
            args = list(
              max_time = 1
            ))
  setBeta(this, function(...) 1)
  setLookback(this, 0)
  setLookForward(this, 10000000)
  setWaitAfterClose(this, TRUE)
  setCommission(this, quote({
    abs(pos_change) * data_raw[i,] * 0.00025
  }))
  addIndicator(this, args = list(name = EMA, x = quote(sent), n = 80), as = 'sma_s',
               lookback = 200)
  addIndicator(this, args = list(name = EMA, x = quote(sma_s), n = 10), as = 'sma_ss',
               lookback = 200)
  addRule(this, as = 'short',
          condition = 
            sma_s > sma_ss &
            sma_s > Lag(sma_s) &
            spread < Lag(spread) &
            Lag(spread < Lag(spread)),
          type = 'enter',
          side = -1,
          oco = 'short'
  )
  addRule(this, as = 'buy',
          condition = 
            time_in_trade >= max_time ||
            pnl_trade > min_profit,
          #sma_s < Lag(sma_s),
          pathwise = TRUE,
          type = 'exit',
          oco = 'short'
  )
  addRule(this, as = 'long',
          condition = 
            sma_s > sma_ss &
            sma_s > Lag(sma_s) & 
            spread > Lag(spread) &
            Lag(spread > Lag(spread)),
          type = 'enter',
          side = 1,
          oco = 'long'
  )
  addRule(this, as = 'sell',
          condition = 
            time_in_trade >= max_time ||
            pnl_trade > min_profit,
          #sma_s < Lag(sma_s),
          pathwise = TRUE,
          type = 'exit',
          oco = 'long'
  )
  addProgramPart(this, 
                 'AB_vars',
                 list(
                   init = quote({
                     min_profit <- pnl_trade <- time_in_trade <- 0
                   }),
                   data = quote({
                     modelD$sent <<- coredata(modelD$sent)
                   }),
                   enter = quote({
                     min_profit <- sum(getCommission(this, mult_last * beta_last, e_user)) * com_coef
                     time_in_trade <- 0
                     price_in <- data_raw[i,]
                   }),
                   exit = quote({
                   }),
                   coefs = quote({
                     sent <- modelD$sent[spread_start:spread_end,]
                   }),
                   iter = quote({
                     if(marketState == 'in'){
                       time_in_trade <- time_in_trade + 1
                       pnl_trade <- (data_raw[i,] - price_in) * beta_last * mult_last * side_last
                     }
                   })
                 )
  )
}


setMoney(this, 1000)
x <- performServer(this, session)
x[[1]]
x[[3]] %>% View
#backtesting params price
{
  paramset <- "TEST"
  deleteParamset(this, paramset)
  #distributions
  {
    addDistribution(this,
                    paramset.label = paramset,
                    component.type = 'params',
                    component.label = 'pp',
                    variable = list(com_coef = seq(2, 10, 0.5)),
                    label = 'com_coef'
    )
    
    addDistribution(this,
                    paramset.label = paramset,
                    component.type = 'params',
                    component.label = 'rules',
                    variable = list(max_time = seq(1, 10, 1)),
                    label = 'max_time'
    )
    
    addDistribution(this,
                    paramset.label = paramset, 
                    component.type = 'indicators', 
                    component.label = 'sma_s', 
                    variable = list(n = seq(10, 100, 5)), 
                    label = 'sma_s.n'
    )
    
    addDistribution(this,
                    paramset.label = paramset, 
                    component.type = 'indicators',
                    component.label = 'sma_ss',
                    variable = list(n = seq(5, 30, 5)),
                    label = 'sma_ss.n' 
    )
    
    
  }
}

start_date <- '2010-01-01'
end_date <- '2017-01-01'

res <- applyParamsetServer(this, 
                    session = session,
                    paramset.label = paramset,
                    start_date = start_date,
                    end_date = end_date,
                    nsamples = 10)

# 
res %>% 
  dplyr::arrange(dplyr::desc(sharpe.ann)) %>%
  dplyr::filter(trades.year > 5) %>% 
  head(5)

xx <- performServer(this, session, paramset.label = paramset, paramset.index = 8657, start_date= start_date, end_date=end_date)
xx[[1]]


