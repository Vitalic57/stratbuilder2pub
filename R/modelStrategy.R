## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  globalVariables(c("."))

#' creates modelStrategy object
#'
#' @return modelStrategy object
#' @export
#'
#' @import xts 
#' @import magrittr
#' @import ggplot2
#' @importFrom xts xts
#' @importFrom zoo coredata index
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics plot
#' @importFrom stats runif
#' @importFrom utils capture.output head install.packages installed.packages packageVersion remove.packages tail untar globalVariables
modelStrategy <- function(){
  thisEnv <- environment()
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
  objects <- new.env()
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
#' \dontrun{
#' settings(plot = list(adjust = TRUE, each_year = TRUE))
#' }
settings <- function(...){
  l <- list(...)
  list2env(l, .settings)
  invisible()
}


#' Get waitAfterClose. If it is true, then new open will be after last close, else they may be in the same time
#'
#' @param this modelStrategy
#' @export
#' @rdname getWaitAfterClose
getWaitAfterClose <- function(this){
  UseMethod('getWaitAfterClose', this)
}

#' @export
#' @rdname getWaitAfterClose
#' @method getWaitAfterClose modelStrategy
getWaitAfterClose.modelStrategy <- function(this){
  return(this$thisEnv$waitAfterClose)
}

#' Set waitAfterClose. If it is true, then new open will be after last close, else they may be in the same time
#'
#' @param this modelStrategy
#' @param x logical
#' @export
#' @rdname setWaitAfterClose
setWaitAfterClose <- function(this, x){
  UseMethod('setWaitAfterClose', this)
}

#' @export
#' @rdname setWaitAfterClose
#' @method setWaitAfterClose modelStrategy
setWaitAfterClose.modelStrategy <- function(this, x){
  this$thisEnv$waitAfterClose <- x
}

#' Get pmAfterOpen. 
#' 
#' If it is true, then position manager will be turned on just after opening position
#'
#' @param this modelStrategy
#' @export
#' @rdname getPmAfterOpen
getPmAfterOpen <- function(this){
  UseMethod('getPmAfterOpen', this)
}

#' @export
#' @rdname getPmAfterOpen
#' @method getPmAfterOpen modelStrategy
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
#' @export
#' @rdname setPmAfterOpen
setPmAfterOpen <- function(this, x){
  UseMethod('setPmAfterOpen', this)
}

#' @export
#' @rdname setPmAfterOpen
#' @method setPmAfterOpen modelStrategy
setPmAfterOpen.modelStrategy <- function(this, x){
  this$thisEnv$pmAfterOpen <- x
}



#' Get variable ignorePosition.
#'
#' If it is true, then when model  simulated, when there was time to change coefs and we had position,
#'  then position would be closed
#'
#' @param this modelStrategy
#'
#' @rdname getIgnorePosition
#' @method getIgnorePosition modelStrategy
#' @export
getIgnorePosition <- function(this){
  UseMethod('getIgnorePosition', this)
}

#' @export
#' @rdname getIgnorePosition
#' @method getIgnorePosition modelStrategy
getIgnorePosition.modelStrategy <- function(this){
  if(is.null(this$thisEnv$ignorePosition)){
    this$thisEnv$ignorePosition <- FALSE
  }
  return(this$thisEnv$ignorePosition)
}

#' Get variable ignorePosition.
#'
#' @param this modelStrategy
#' @param bool bool, if it is true, then when model  simulated, when there was time to change coefs and we had position,
#'  then position would be closed
#'

#' @rdname setIgnorePosition
#' @method setIgnorePosition modelStrategy
#' @export
setIgnorePosition <- function(this, bool){
  UseMethod('setIgnorePosition', this)
}

#' @export
#' @rdname setIgnorePosition
#' @method setIgnorePosition modelStrategy
setIgnorePosition.modelStrategy <- function(this, bool){
  this$thisEnv$ignorePosition <- bool
}


#' Set rule for calculating spread.
#'
#' @param this modelStrategy
#' @param fun function, it calculates coefs for spread
#' @param args list, arguments in setBeta function
#' @export
setBeta <- function(this, fun, args = NULL){
  UseMethod('setBeta', this)
}

#' @export
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

#' Set amount of money, that we have at the beginning
#'
#' @param this model
#' @param x numeric, amount of money
#' 
#' @export
#' @rdname setMoney
setMoney <- function(this, x){
  UseMethod('setMoney', this)
}

#' @export
#' @rdname setMoney
#' @method setMoney modelStrategy
setMoney.modelStrategy <- function(this,x){
  e <- this$thisEnv
  e$money <- x
}


#' Get amount of money that we have at the beginning in specific backtest
#'
#' @param this modelStrategy
#'
#' @return numeric, amount of money
#' @export
getMoney <- function(this){
  UseMethod('getMoney', this)
}

#' @export
getMoney.modelStrategy <- function(this){
  return(this$thisEnv$money)
}


#' Set window for calculating coefs
#'
#' @param this modelStrategy
#' @param x numeric, window for calculating
#' @export
setLookback <- function(this, x){
  UseMethod('setLookback', this)
}

#' @export
setLookback.modelStrategy <- function(this,x){
  e <- this$thisEnv
  e$lookback <- x
}

#' Get window for calculating coefs
#'
#' @param this modelStrategy
#' @export
getLookback <- function(this){
  UseMethod('getLookback', this)
}

#' @export
getLookback.modelStrategy <- function(this){
  return(this$thisEnv$lookback)
}

#' Set window how far the same coefs will be used
#'
#' @param this modelStrategy
#' @param x numeric, window
#' @export
setLookForward <- function(this, x){
  UseMethod('setLookForward', this)
}

#' @export
setLookForward.modelStrategy <- function(this,x){
  e <- this$thisEnv
  e$lookForward <- x
}

#' Get window how far the same coefs will be used
#'
#' @param this modelStrategy
#' @export
getLookForward <- function(this){
  UseMethod('getLookForward', this)
}

#' @export
getLookForward.modelStrategy <- function(this){
  return(this$thisEnv$lookForward)
}


#' Set tolerance of betas computation
#'
#' @param this modelStrategy
#' @param x numeric, tolerance
#' @export
setToleranceBeta <- function(this, x){
  UseMethod('setToleranceBeta', this)
}

#' @export
setToleranceBeta.modelStrategy <- function(this,x){
  e <- this$thisEnv
  e$toleranceBeta <- x
}

#' Get tolerance of betas computation
#'
#' @param this modelStrategy
#'
#' @return numeric
#' @export
getToleranceBeta <- function(this){
  UseMethod('getToleranceBeta', this)
}

#' @export
getToleranceBeta.modelStrategy <- function(this){
  return(this$thisEnv$toleranceBeta)
}


#' Set window for calculating indicators
#'
#' @param this modelStrategy
#' @param x numeric, window
#' @export
setMaxLookback <- function(this, x){
  UseMethod('setMaxLookback', this)
}

#' @export
setMaxLookback.modelStrategy <- function(this,x){
  e <- this$thisEnv
  e$maxLookback <- x
}


#' Get window for calculating indicators
#'
#' @param this modelStrategy
#' @export
getMaxLookback <- function(this){
  UseMethod('getMaxLookback', this)
}

#' @export
getMaxLookback.modelStrategy <- function(this){
  return(this$thisEnv$maxLookback)
}

#' Return if you have a need to convert betas to int.
#' It is TRUE by default
#' @param this modelStrategy
#' @export
#' @rdname getBetasInt
getBetasInt <- function(this){
  UseMethod('getBetasInt', this)
}

#' @export
#' @rdname getBetasInt
#' @method getBetasInt modelStrategy
getBetasInt.modelStrategy <- function(this){
  if(is.null(this$thisEnv$betasInt)){
    this$thisEnv$betasInt <- TRUE
  }
  return(this$thisEnv$betasInt)
}

#' Set TRUE if you have a need to convert betas to integers.
#'
#' @param this modelStrategy
#' @param x logical
#' @export
#' @rdname setBetasInt
setBetasInt <- function(this, x){
  UseMethod('setBetasInt', this)
}

#' @export
#' @rdname setBetasInt
#' @method setBetasInt modelStrategy
setBetasInt.modelStrategy <- function(this, x){
  this$thisEnv$betasInt <- x
}


#' Add user-defined objects to modelStrategy for future usage in backtest 
#'
#' @param this modelStrategy
#' @param ... named args
#' @export
#' @rdname addObject
addObject <- function(this, ...){
  UseMethod('addObject', this)
}

#' @export
#' @rdname addObject
#' @method addObject modelStrategy
addObject.modelStrategy <- function(this, ...){
  dots <- list(...)
  if(is.null(names(dots)) || any(names(dots) == '')){
    stop("object must have a name.")
  }
  for(name in names(dots)){
    this$thisEnv[['objects']][[name]] <- dots[[name]]
  } 
}








