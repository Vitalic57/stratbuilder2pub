#' creates modelStrategy object
#'
#' @return modelStrategy object
#' @export
#'
modelStrategy <- function(){
  thisEnv <- environment()
  tradingData <- 'data_raw'
  stats_init <- list()
  stats <- list()
  user_add_data <- list()
  betaData <- 'data_raw'
  spreadData <- 'data_raw'
  beta_fun <- function(...){return(1)}
  saveData <- NULL
  money <- 10^7
  toleranceBeta <- 0.1
  lookForward <- Inf
  lookback <- 0
  maxLookback <- 0
  waitAfterClose <- FALSE
  pmAfterOpen <- TRUE
  indicators <- list()
  rules <- list()
  rules_path_ind <- numeric()
  rules_norm_ind <- numeric()
  data_from_user <- list()
  paramsets <- list()
  backtests <- list()
  pps <- list()
  vars <- list()
  program <- list()
  params <- list(
    vars = list(),
    pps = list(),
    rules = list(),
    pms = list()
    )
  #activeField <- c(start = Inf, end = 0)
  me <- list(thisEnv = thisEnv)


  ## Set the name for the class
  class(me) <- c("modelStrategy")

  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)
  return(me)
}


.settings <- new.env()
.settings[['reload']] <- TRUE

#' Change some settings
#'
#' @param ... parameters
#'
#' @export
#' @example 
#' settings(plot = list(adjust = TRUE, each_year = TRUE))
settings <- function(...){
  l <- list(...)
  list2env(l, .settings)
  invisible()
}


#' Gets waitAfterClose. If it is true, then new open will be after last close, else they may be in the same time
#'
#' @param this modelStrategy
#'
#' @export
getWaitAfterClose.modelStrategy <- function(this){
  return(this$thisEnv$waitAfterClose)
}

#' Sets waitAfterClose. If it is true, then new open will be after last close, else they may be in the same time
#'
#' @param this modelStrategy
#'
#' @export
setWaitAfterClose.modelStrategy <- function(this, x){
  this$thisEnv$waitAfterClose <- x
}

#' Gets pmAfterOpen. 
#' 
#' If it is true, then position manager will be turned on just after opening position
#'
#' @param this modelStrategy
#'
#' @export
getPmAfterOpen.modelStrategy <- function(this){
  if(is.null(this$thisEnv$pmAfterOpen)){
    return(FALSE)
  }
  return(this$thisEnv$pmAfterOpen)
}

#' Gets pmAfterOpen. 
#' 
#' If it is true, then position manager will be turned on just after opening position
#'
#' @param this modelStrategy
#' @param x logical
#'
#' @export
setPmAfterOpen.modelStrategy <- function(this, x){
  this$thisEnv$pmAfterOpen <- x
}



#' Gets variable ignorePosition.
#'
#' If it is true, then when model  simulated, when there was time to change coefs and we had position,
#'  then position would be closed
#'
#' @param this modelStrategy
#'
#' @export
#' @rdname getIgnorePosition
#' @method getIgnorePosition modelStrategy
getIgnorePosition.modelStrategy <- function(this){
  if(is.null(this$thisEnv$ignorePosition)){
    this$thisEnv$ignorePosition <- FALSE
  }
  return(this$thisEnv$ignorePosition)
}

#' Gets variable ignorePosition.
#'
#' @param this modelStrategy
#' @param bool bool, if it is true, then when model  simulated, when there was time to change coefs and we had position,
#'  then position would be closed
#'
#' @export
#' @rdname setIgnorePosition
#' @method setIgnorePosition modelStrategy
setIgnorePosition.modelStrategy <- function(this, bool){
  this$thisEnv$ignorePosition <- bool
}


#' tradingData indicates which data strategy should use for spread calculating
#'
#' @param this modelStrategy
#'
#' @return character
#' @export
#' @rdname getTradingData
#' @method getTradingData modelStrategy
getTradingData.modelStrategy <- function(this){
  return(this$thisEnv$tradingData)
}

#' tradingData indicates which data strategy should use for spread calculating
#'
#' @param this modelStrategy
#'
#' @export
#' @rdname setTradingData
#' @method setTradingData modelStrategy
setTradingData.modelStrategy <- function(this,x){
  this$thisEnv$tradingData <- x
}







#' Sets rule for calculating spread.
#'
#' @param this modelStrategy
#' @param fun function, it calculates coefs for spread
#'
#' @export
#' @rdname setBeta
#' @method setBeta modelStrategy
setBeta.modelStrategy <- function(this, fun, args = NULL){
  if(missing(fun)){
    stop("Please provide name or fun argument")
  }else if(!missing(fun)){
    this$thisEnv$beta_fun_force <- FALSE
    e <- this$thisEnv
    if(!is.null(args)){
      formals(fun) <- modify.args(formals(fun), args)
    }
    e$beta_fun <- fun
  }
}

#' Sets amount of money, that we have at the beginning
#'
#' @param this modelStrategy
#' @param x numeric, amount of money
#'
#' @export
#' @rdname setMoney
#' @method setMoney modelStrategy
setMoney.modelStrategy <- function(this,x){
  e <- this$thisEnv
  e$money <- x
}


#' Gets amount of money that we have at the beginning in specific backtest
#'
#' @param this modelStrategy
#' @param from character, name of folder in backtests
#'
#' @return numeric, amount of money
#' @export
#' @rdname getMoney
#' @method getMoney modelStrategy
getMoney.modelStrategy <- function(this, from = NULL){
  if(is.null(from)){
    return(this$thisEnv$money)
  }else if(from %in% names(this$thisEnv$backtests)){
    return(this$thisEnv$backtests[[from]]$money)
  }else{
    return(this$thisEnv$money)
  }
}


#' Sets window for calculating coefs
#'
#' @param this modelStrategy
#' @param x numeric, window for calculating
#'
#' @export
#' @rdname setLookback
#' @method setLookback modelStrategy
setLookback.modelStrategy <- function(this,x){
  e <- this$thisEnv
  e$lookback <- x
}

#' Gets window for calculating coefs
#'
#' @param this modelStrategy
#'
#' @export
#' @rdname getLookback
#' @method getLookback modelStrategy
getLookback.modelStrategy <- function(this){
  return(this$thisEnv$lookback)
}

#' Sets window how far the same coefs will be used
#'
#' @param this modelStrategy
#' @param x numeric, window
#'
#' @export
#' @rdname setLookForward
#' @method setLookForward modelStrategy
setLookForward.modelStrategy <- function(this,x){
  e <- this$thisEnv
  e$lookForward <- x
}

#' Gets window how far the same coefs will be used
#'
#' @param this modelStrategy
#'
#' @export
#' @rdname getLookForward
#' @method getLookForward modelStrategy
getLookForward.modelStrategy <- function(this){
  return(this$thisEnv$lookForward)
}


#' Sets tolerance of betas computation
#'
#' @param this modelStrategy
#' @param x numeric, tolerance
#'
#' @export
#' @rdname setToleranceBeta
#' @method setToleranceBeta modelStrategy
setToleranceBeta.modelStrategy <- function(this,x){
  e <- this$thisEnv
  e$toleranceBeta <- x
}

#' Gets tolerance of betas computation
#'
#' @param this modelStrategy
#'
#' @return numeric
#' @export
#' @rdname getToleranceBeta
#' @method getToleranceBeta modelStrategy
getToleranceBeta.modelStrategy <- function(this){
  return(this$thisEnv$toleranceBeta)
}


#' Sets window for calculating indicators
#'
#' @param this modelStrategy
#' @param x numeric, window
#'
#' @export
#' @rdname setMaxLookback
#' @method setMaxLookback modelStrategy
setMaxLookback.modelStrategy <- function(this,x){
  e <- this$thisEnv
  e$maxLookback <- x
}


#' Gets window for calculating indicators
#'
#' @param this modelStrategy
#'
#' @export
#' @rdname getMaxLookback
#' @method getMaxLookback modelStrategy
getMaxLookback.modelStrategy <- function(this){
  return(this$thisEnv$maxLookback)
}

#' Return if you have a need to convert betas to int.
#' It is TRUE by default
#' @param model modelStrategy
#'
#' @export
getBetasInt.modelStrategy <- function(model){
  if(is.null(model$thisEnv$betasInt)){
    model$thisEnv$betasInt <- TRUE
  }
  return(model$thisEnv$betasInt)
}

#' Sets if you have a need to convert betas to int.
#'
#' @param model modelStrategy
#' @param x logical
#'
#' @export
setBetasInt.modelStrategy <- function(model, x){
  model$thisEnv$betasInt <- x
}













