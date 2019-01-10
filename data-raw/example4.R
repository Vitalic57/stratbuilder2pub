library(stratbuilder2pub)
library(TTR)

session <- ssh_connect('YOUR ADDRESS', keyfile = 'PATH TO KEY') 

# Now we consider another feature. It makes backtesting system highly customizable.
# For example, let's take the previous strategy and add a such rule: it closes all positions after 20 days in position.
# And we want to use logarithm of data_raw to create process and compute betas
{
  this <- modelStrategy()
  
  setLookback(this, 100) 
  setLookForward(this, 200)
  setIgnorePosition(this, TRUE) 
  setBeta(this, function(data, ...){ 
    colnames(data) <-  c('x', 'y')
    model <- lm(y~x, data.frame(data))
    beta <- c(1, -coefficients(model)[2])
    # beta must be the same length as number of columns in data
    return(beta)
  }) 
  setBetasInt(this, FALSE)
  addIndicator(this, args = list(name = SMA, x = quote(spread), n = 100), as = 'ema',
               lookback = 101) 
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
  # here the main block in this example
  # This code let your program change/add the code inside main cycle of backtest
  # There are multiple places where we can add some code and there some basic variables that can be exploited for 
  # make some new useful variables.
  addProgramPart(this,
                 as = 'pp1',
                 evolution = list(
                   init = quote({ 
                     # this part will be evaled before main cycle starts
                     # Pay attention to syntax. All program parts have to be in quote function
                     # we will need it later
                     time_after_enter <- 0
                   }),
                   data = quote({
                     # As I said earlier there several data types: data_raw, data_roll, data_margin
                     # They are stored in this$thisEnv$modelD object. You don't have access to them here on your local machine,
                     # but you can use them inside backtester.
                     # to access data_raw you can call this this$thisEnv$modelD$data_raw
                     # It will return xts object. Xts objects are comfortable to use, but they are really slow,
                     # so if you want only matrix data without dates use this command coredata(this$thisEnv$modelD$data_raw)
                     # In this section you can create you own data from existing. For example, you want logarithm data, then
                     # use log(this$thisEnv$modelD$data_raw), we you want to use this data for computing beta coefficients,
                     # then name that data somehow and do this this$thisEnv$modelD[['Your name']] <- log(this$thisEnv$modelD$data_raw).
                     # Some names are occupied: data_raw, data_roll, data_margin, pnl, data_diff don't use them!
                     # Only if want to change exactly that data you can do it.
                     this$thisEnv$modelD[['log']] <<- log(this$thisEnv$modelD$data_raw)
                     data_raw <- coredata(this$thisEnv$modelD$data_raw)
                   }),
                   benter = quote({
                     # this part will be evaled before each enter to position
                     # We will compute our beta coefficiets from logarithm, so 
                     # we need to calculate how many instruments to buy
                     # There are inner variables called beta_spread_last and beta_last, they are equal to return of beta_fun function from setBeta(this,...)
                     # beta_last is responsible for number of instruments algo will buy or sell, it should not be not integer, but beta_spread_last can. 
                     # It only stores last output of beta_fun function. Here we change beta_last in order it will be integer
                     beta_last <<- floor(beta_spread_last / sum(abs(beta_spread_last)) * getMoney(this) / 10 / data_raw[i,])
                     # operator <<- must be here because we change variable in main cycle
                     # i -- current row in data in backtest cycle
                     # There are another time variable in loop: ii and iii
                     # They are all about the same time, but with diffrent start.
                     # start of i is 1
                     # start of ii is index where backtest starts, it can differ from i when start_date specified and it is not the first date of data
                     # start of iii is where lookback period of last update of coefficients starts. In other words if we update coefficients on i-th step
                     # then iii = i + lookback. It is index of current element in spread and its derivatives
                   }),
                   enter = quote({
                     # this part will be evaled after each enter to position
                     # this variable will be responsible for number of days in position
                     time_after_enter <- 0
                   }),
                   bexit = quote({
                     # this part will be evaled before each exit from position
                   }),
                   coefs = quote({
                     # After beta evaluation
                   }),
                   table = quote({
                     # After evalution of indicators and rules with pathwise=FALSE argument
                   }),
                   exit = quote({
                     # this part will be evaled after each exit from position
                     # here it must be equal to 0
                     time_after_enter <- 0
                   }),
                   iter = quote({
                     # this quote will be evaled at each iteration. 
                     # There is variable marketState, it equals to 'in' or 'out'. If it is equal to 'in', then robot is in postions now,
                     # if 'out', then not. We want ++ time_after_enter if we are in position.
                     if(marketState == 'in'){
                       time_after_enter <- time_after_enter + 1
                     }
                   })
                 ))
  addRule(this, as = 'long_exit2',
          condition = time_after_enter  > 20,
          pathwise = TRUE, # This rule is not working with tables and matrices, it works with scalar variable,
                           # so pathwise argument should be equal to TRUE. This rule will be evaluated at each iteration
          type = 'exit',
          oco = 'long'
  )
  # The same is here
  addRule(this, as = 'short_exit2',
          condition = time_after_enter  > 20,
          pathwise = TRUE,
          type = 'exit',
          oco = 'short'
  )
  # Here we define what data we will use for calculating betas and spread
  # spread will be equal to linear combination of columns from table with name this$thisEnv$spreadData with
  # coefficients that will return function from setBeta
  this$thisEnv$betaData <- 'log'
  this$thisEnv$spreadData <- 'log' 
  
  
}

setUserData(this, list(dataset = 'Russia', 
                       assets = c('GAZP', 'LKOH'), 
                       period = 'day', 
                       time = 13)) 

x <- performServer(this, session)


# From report we could see that days.in.pos.max == 38, in our program 
# was 20 days, but it was business days. In the report -- all days. Such a diffrence is probably from new year's holidays

