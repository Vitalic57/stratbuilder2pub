## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  globalVariables(c("."))

#' creates modelStrategy object
#'
#' @return modelStrategy object
#' @export
#'
#' @import xts 
#' @import zoo
#' @import magrittr
#' @import ggplot2
#' @importFrom zoo coredata index
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics plot
#' @importFrom stats runif
#' @importFrom quantmod Lag
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
#' @param bool bool, if it is true, then when model  simulated, 
#' when there was time to change coefs and we had position,
#'  then position would be closed
#'
#' @export
#' @rdname setIgnorePosition
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
#' @rdname setBeta
setBeta <- function(this, fun, args = NULL){
  UseMethod('setBeta', this)
}

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
#' @rdname getMoney
getMoney <- function(this){
  UseMethod('getMoney', this)
}

#' @export
#' @rdname getMoney
#' @method getMoney modelStrategy
getMoney.modelStrategy <- function(this){
  return(this$thisEnv$money)
}


#' Set window for calculating coefs
#'
#' @param this modelStrategy
#' @param x numeric, window for calculating
#' @export
#' @rdname setLookback
setLookback <- function(this, x){
  UseMethod('setLookback', this)
}

#' @export
#' @rdname setLookback
#' @method setLookback modelStrategy
setLookback.modelStrategy <- function(this,x){
  e <- this$thisEnv
  e$lookback <- x
}

#' Get window for calculating coefs
#'
#' @param this modelStrategy
#' @export
#' @rdname getLookback
getLookback <- function(this){
  UseMethod('getLookback', this)
}

#' @export
#' @rdname getLookback
#' @method getLookback modelStrategy
getLookback.modelStrategy <- function(this){
  return(this$thisEnv$lookback)
}

#' Set window how far the same coefs will be used
#'
#' @param this modelStrategy
#' @param x numeric, window
#' @export
#' @rdname setLookForward
setLookForward <- function(this, x){
  UseMethod('setLookForward', this)
}

#' @export
#' @rdname setLookForward
#' @method setLookForward modelStrategy
setLookForward.modelStrategy <- function(this,x){
  e <- this$thisEnv
  e$lookForward <- x
}

#' Get window how far the same coefs will be used
#'
#' @param this modelStrategy
#' @export
#' @rdname getLookForward
getLookForward <- function(this){
  UseMethod('getLookForward', this)
}

#' @export
#' @rdname getLookForward
#' @method getLookForward modelStrategy
getLookForward.modelStrategy <- function(this){
  return(this$thisEnv$lookForward)
}


#' Set tolerance of betas computation
#'
#' @param this modelStrategy
#' @param x numeric, tolerance
#' @export
#' @rdname setToleranceBeta
setToleranceBeta <- function(this, x){
  UseMethod('setToleranceBeta', this)
}

#' @export
#' @rdname setToleranceBeta
#' @method setToleranceBeta modelStrategy
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
#' @rdname getToleranceBeta
getToleranceBeta <- function(this){
  UseMethod('getToleranceBeta', this)
}

#' @export
#' @rdname getToleranceBeta
#' @method getToleranceBeta modelStrategy
getToleranceBeta.modelStrategy <- function(this){
  return(this$thisEnv$toleranceBeta)
}


#' Set window for calculating indicators
#'
#' @param this modelStrategy
#' @param x numeric, window
#' @export
#' @rdname setMaxLookback
setMaxLookback <- function(this, x){
  UseMethod('setMaxLookback', this)
}

#' @export
#' @rdname setMaxLookback
#' @method setMaxLookback modelStrategy
setMaxLookback.modelStrategy <- function(this,x){
  e <- this$thisEnv
  e$maxLookback <- x
}


#' Get window for calculating indicators
#'
#' @param this modelStrategy
#' @export
#' @rdname getMaxLookback
getMaxLookback <- function(this){
  UseMethod('getMaxLookback', this)
}

#' @export
#' @rdname getMaxLookback
#' @method getMaxLookback modelStrategy
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



#' get expandingLookback
#' 
#' If it is TRUE, then size of data in beta_fun will be equal to current index, else to lookback
#'
#' @param this modelStrategy
#'
#' @export
#' @rdname getExpandingLookback
#' @method getExpandingLookback modelStrategy
getExpandingLookback.modelStrategy <- function(this){
  if(!'expandingLookback' %in% names(this$thisEnv)){
    this$thisEnv$expandingLookback <- FALSE
  }
  return(this$thisEnv$expandingLookback)
}

#' set expandingLookback
#' 
#' If it is TRUE, then size of data in beta_fun will be equal to current index, else to lookback
#'
#' @param this modelStrategy
#' @param x logical
#'
#' @export
#' @rdname setExpandingLookback
#' @method setExpandingLookback modelStrategy
setExpandingLookback.modelStrategy <- function(this, x){
  if(is.logical(x)){
    this$thisEnv$expandingLookback <- x
  }else{
    print('x must be logical')
  }
}


#' Set/Get betasByMoney variable
#' 
#' This variable is responsible for how spread will be trades. If it is TRUE then coefficients before trade will be update in
#'  according to current prices in table data_raw. For example, if coefs is c(0.5, -0.5) and prices c(10, 100) and money is 1000,
#'  then position will be opened with assets amount c(50, -5). But if variable  is FALSE, then assets amount will be c(9, -9).
#'  If betasByMoney is TRUE, then option from setBetasInt will be ignored.
#'
#' @param this modelStrategy
#' @param x logical
#' @param price quote, price of each intrument in moment i
#'
#' @return
#' @export
#' @rdname setBetasByMoney
#' @method setBetasByMoney modelStrategy
setBetasByMoney.modelStrategy <- function(this, x, price=quote(data_raw[i - 1,])){
  if(is.logical(x) && is.language(price)){
    this$thisEnv$betasByMoney <- x
    this$thisEnv$betasByMoneyPrice <- price
  }else{
    stop('x must be a logical and price must be a quote')
  }
}


#' @return
#' @export
#' @rdname setBetasByMoney
#' @method getBetasByMoney modelStrategy
getBetasByMoney.modelStrategy <- function(this){
  if(!'betasByMoney' %in% names(this$thisEnv)){
    this$thisEnv$betasByMoney <- FALSE
  }
  return(this$thisEnv$betasByMoney)
}


#' Return Saved Models
#'
#' @param this modelStrategy
#' @export
#' @rdname getSavedModels
getSavedModels <- function(this){
  UseMethod('getSavedModels', this)
}

#' @export
#' @rdname getSavedModels
#' @method getSavedModels modelStrategy
getSavedModels.modelStrategy <- function(this){
  this$thisEnv$save_strategy
}

