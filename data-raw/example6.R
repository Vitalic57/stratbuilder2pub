library(stratbuilder2pub)
library(TTR)
library(xts)
library(quantmod)


# Now we redefine function from DSTrading package. 
# Now this package are available in backtester. 
# If you need some package for your backtest, please contact us and we make it available too.
# quantmod, xts and TTR functions can be without package::, but with package::: if needed. 
FRAMA <- function (HLC, n = 20, FC = 1, SC = 200) 
{
  if (n%%2 == 1) 
    n = n - 1
  if(has.Hi(HLC) && has.Lo(HLC)){
    N3 <- (runMax(Hi(HLC), n) - runMin(Lo(HLC), n))/n
    N1 <- (runMax(Hi(HLC), n/2) - runMin(Lo(HLC), n/2))/(n/2)
    price <- (Hi(HLC) + Lo(HLC)) / 2
    if(is.xts(HLC)){
      lagSeries <- lag(HLC, n/2)
    }else{
      lagSeries <- stratbuilder2pub:::Lag.matrix(HLC, n / 2)
    }
    N2 <- (runMax(Hi(lagSeries), n/2) - runMin(Lo(lagSeries), 
                                               n/2))/(n/2)
  }else{
    if(has.Cl(HLC)){
      price <- Cl(HLC)
      lagSeries <- quantmod::Lag(price, n / 2)
    }else if(!is.null(dim(HLC)) && length(dim(HLC)) == 2){
      price <- HLC[,1]
      lagSeries <- stratbuilder2pub:::Lag.matrix(HLC, n / 2)
    }else if(is.numeric(HLC)){
      price <- HLC
      lagSeries <- quantmod::Lag(price, n / 2)
    }else{
      stop('HLC can be matrix or vector or xts')
    }
    N3 <- (runMax(price, n) - runMin(price, n))/n
    N1 <- (runMax(price, n/2) - runMin(price, n/2))/(n/2)
    N2 <- (runMax(lagSeries, n/2) - runMin(lagSeries, 
                                               n/2))/(n/2)
  }
  
  
  dimen <- (log(N1 + N2) - log(N3))/log(2)
  w <- log(2/(SC + 1))
  oldAlpha <- exp(w * (dimen - 1))
  oldN <- (2 - oldAlpha)/oldAlpha
  newN <- ((SC - FC) * (oldN - 1)/(SC - 1)) + FC
  alpha <- 2/(newN + 1)
  alpha[which(alpha > 1)] <- 1
  alpha[which(alpha < w)] <- w
  alphaComplement <- 1 - alpha
  initializationIndex <- index(alpha[is.na(alpha)])
  alpha[is.na(alpha)] <- 1
  alphaComplement[is.na(alphaComplement)] <- 0
  FRAMA <- rep(0, length(price))
  FRAMA[1] <- price[1]
  FRAMA <- DSTrading:::computeFRAMA(alpha, alphaComplement, FRAMA, price)
  if(is.xts(price)){
    FRAMA <- xts(FRAMA, order.by = index(price))
  }
  FRAMA[initializationIndex] <- alpha[initializationIndex] <- NA
  out <- cbind(FRAMA = FRAMA)
  return(out)
}


# Now we try strategy with deviation from mean .
# We will open position if spread was outside small range and it returns inside
# We will increase (double) position if spread was outside big range and now inside big range but outside small
# We will close position if spread crosses mean
# Or we will close position if pnl is more then commission, 
# We need to set commission to strategy, by default algo is working without them. Strategy can work perfectly, 
# but fails with commission.

# This strategy only shows functionality and it does not have alpha 
{
  this <- modelStrategy() 
  setLookback(this, 250) 
  setLookForward(this, 125)
  setPmAfterOpen(this, FALSE) # If TRUE, then block of position managing will be evaluated after open in the same time,
  # if FALSE, then it will be evaluated in the next time
  setBeta(this, function(data, ...){ 
    colnames(data) <-  c('x', 'y')
    model <- lm(y~x, data.frame(data))
    beta <- c(1, -coefficients(model)[2])
    # beta must be the same length as number of columns in data
    return(beta)
  })
  setCommission(this, quote({
    # here there is predefined variable pos_change -- it is equal to the second argument of getCommission()
    # pos_change = change of position, how much we should pay for that change.
    # You can use your variables, that defined in programParts.
    # It should return (the last line) commission for each instrument
    # You can do adjustments for slippage and other effects here too
    data_raw[i,] * 0.0005 * abs(pos_change)
  }))
  
  setParams(this, 
            type = 'pm',
            args = list(
              ncom = 2,
              n.sd = 2
            )
  )
  setParams(this, 
            type = 'rules',
            args = list(
              n.sd = 0.5
            )
  )
  addIndicator(this, args = list(name = FRAMA, HLC = quote(spread), n = 20), as = 'ma',
               lookback = 25) 
  # x = quote(spread - ma), ma is the indicator above, if you define it below, it will not work! Order of additing of indicators plays an important role.
  addIndicator(this, args = list(name = runSD, x = quote(spread - ma), n = 20, sample = FALSE), as = 'std',
               lookback = 50) 
  addRule(this, as = 'short', 
          condition = spread > ma &
            spread < ma + std * n.sd &
            Lag(spread > ma + std * n.sd), 
          type = 'enter',
          side = -1,
          oco = 'short', 
          osFun = stratbuilder2pub:::sameMoneyOs, 
          # getMoney(this) equals to 10 millions by default. 
          osFun_args = alist(amount = getMoney(this) / 2)
  )
  
  addRule(this, as = 'long', 
          condition = spread < ma &
            spread > ma - std * n.sd &
            Lag(spread < ma - std * n.sd), 
          type = 'enter',
          side = 1,
          oco = 'long',
          osFun = stratbuilder2pub:::sameMoneyOs,
          osFun_args = alist(amount = getMoney(this) / 2)
  )
  addRule(this, as = 'short_exit',
          condition = spread < ma, 
          type = 'exit',
          oco = 'short'
  )
  addRule(this, as = 'long_exit', 
          condition = spread > ma,
          type = 'exit',
          oco = 'long'
  )
  addProgramPart(this,
                 as = 'pp1',
                 evolution = list(
                   init = quote({
                     #this$thisEnv$params stores all setParams. There are 'rules'(params of rules), 
                     # 'indicators' (params of indicators),
                     # 'pms' (params of position managers) and 'pps' (params of program parts)
                     n.sd.rule <- this$thisEnv$params[['rules']][['n.sd']]
                   }),
                   data = quote({
                     # data_margin stores information about how much asset cost/ now it is equal to data_raw
                     price <- this$thisEnv$modelD$data_margin
                   }),
                   enter = quote({
                     mean_price <- data_raw[i,]
                     # getCommission is a function that returns commission for given number of instruments and it can depend on
                     # current user's environment. It should return a half of full commission, so here we multiply it by 2
                     com_trade <- sum(getCommission(this, mult_last * beta_last * side_last, env = e_user) * 2)
                     increased <- FALSE
                     pnl_trade <- 0
                     
                   }),
                   iter = quote({
                     if(marketState == 'in'){
                       pnl_trade <- sum((data_raw[i,] - mean_price) * mult_last * beta_last * side_last)
                       # There is another inner variable it is called indicatorsTable. It is list, it stores tables of indicators
                       # rulesTable stores tables of rules
                       current.ema <- indicatorsTable[['ma']][iii]
                       current.sd <- indicatorsTable[['std']][iii] 
                       previous.ema <- indicatorsTable[['ma']][iii - 1]
                       previous.sd <- indicatorsTable[['std']][iii - 1]
                     }
                   })
                 )
                 )
  
  # Here is new block. Manager of position, it work only pathwise, another words conditions and expressions of it will
  # calculated at each iteration. There are several parts of it (increase, decrease, close and change)
  addPM(this, # here we specify strategy object
        oco = 'long', # namespace of rules
        as = 'long', # name of pm (Position Manager)
        increase = list( # The first block, when it should increase position. 
          cond = quote({                     # cond defines when to increase
            !increased &&                                              # Have we already increased position
              spread[iii,] > current.ema - current.sd * n.sd &&       # spread is in big range 
              spread[iii,] < current.ema - current.sd * n.sd.rule &&  # spread is not in small range 
              spread[iii - 1,] < previous.ema - previous.sd * n.sd    # previous spread is outside of big range
           }),
          expr = quote({ # expr block defines what program should do, in this block mult_delta should be set
            # Now we will find mult_delta. It is equal to how many spread to buy or sell 
            # we invest in it getMoney(this) / 2
            mult_delta <- floor(getMoney(this) / 2 / sum(price[i,] * beta_last))
            if(mult_delta > 0){
              increased <- TRUE
              # we need to change commssion
              com_trade <- sum(getCommission(this, (mult_last + mult_delta) * beta_last * side_last, env = e_user) * 2)
              # and mean_price for calculating spread
              trade_vol <- side_last * mult_last * beta_last
              mean_price <- (mean_price * abs(trade_vol) + data_raw[i,] * abs(mult_delta * beta_last)) /  
                (abs(trade_vol) + abs(mult_delta * beta_last))
            }
          })),
        close = list( # this block does not have expr, only cond.
          cond = quote(
            any(
              pnl_trade > ncom * com_trade,
              na.rm = TRUE
            )
          )
        )
  )
  # and now almost the same for short
  addPM(this, 
        oco = 'short', 
        as = 'short', 
        increase = list( 
          cond = quote({                   
            !increased && 
              spread[iii,] < current.ema + current.sd * n.sd && 
              spread[iii,] > current.ema + current.sd * n.sd.rule &&  
              spread[iii - 1,] > previous.ema + previous.sd * n.sd
          }),
          expr = quote({ 
            mult_delta <- floor(getMoney(this) / 2 / sum(price[i,] * beta_last))
            if(mult_delta > 0){
              increased <- TRUE
              com_trade <- sum(getCommission(this, (mult_last + mult_delta) * beta_last * side_last, env = e_user) * 2)
              trade_vol <- side_last * mult_last * beta_last
              mean_price <- (mean_price * abs(trade_vol) + data_raw[i,] * abs(mult_delta * beta_last)) /  
                (abs(trade_vol) + abs(mult_delta * beta_last))
            }
          })),
        close = list( 
          cond = quote(
            any(
              pnl_trade > ncom * com_trade,
              na.rm = TRUE
            )
          )
        )
  )
}




setUserData(this, list(dataset = 'Russia', 
                       assets = c('ROSN', 'LKOH'), 
                       period = 'day', 
                       time = 13))


performServer(this)




