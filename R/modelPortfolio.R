#' creates modelPortfolio object
#'
#' @param ... list of models / model in each argument
#' @param w numeric type, coefficients strategy in portfolio, example: w = c(1,2)
#' @param money_function function type or const, three options are possible
#' @param copy logical, if TRUE then method will copy all models passed to it
#' 
#' 1) money_function = A = const, all strategies will get A money
#' 
#' 2) money_function = scalar function, all strategies will get money_function(start_money_in_strategy) money, example: min
#' 
#' 3) money_function = vector function, strategies[i] will get money_function(start_money_in_strategy)[i] money, example:
#' function \{ 200 + start_money_in_strategy\}
#'
#' @return modelPortfolio object
#' @export
#'
modelPortfolio <- function(..., w=NULL, money_function = NULL, copy = TRUE){
  thisEnv <- environment()
  models <- list(...)
  if(length(models) == 1 && is.list(models[[1]]) && length(models[[1]]) > 1){
    models <- models[[1]]
  }
  if(is.null(names(models))){
    names(models) <- paste0('x', seq_len(length(models)))
  }else{
    if(any(names(models) == '')){
      ind <- which(names(models) == '')
      names(models)[ind] <- paste0('x', ind)
    }
  }
  backtests <- list()
  paramsets <- list()
  if(copy){
    models <- lapply(models, function(x){cloneStrategy(x)})
  }
  me <- list(thisEnv = thisEnv)
  ## Set the name for the class
  class(me) <- c("modelPortfolio")
  ## Define the value of the list within the current environment.
  assign('this', me, envir=thisEnv)
  setWeight(me, w = w, money_function = money_function)
  return(me)
}




#' @export
#' @rdname performServer
#' @method performServer modelPortfolio
performServer.modelPortfolio <- function(this, ...){
  this$thisEnv$data_changed <- TRUE#any(sapply(this$thisEnv$models, function(x) x$thisEnv$data_changed)) 
  x <- performServer.modelStrategy(this, ...)
  if(!is.null(x)){
    return(x)
  }
}



#' @export
#' @rdname applyParamsetServer
#' @method applyParamsetServer modelPortfolio
applyParamsetServer.modelPortfolio <- function(this, ...){
  this$thisEnv$data_changed <- TRUE
  x <- applyParamsetServer.modelStrategy(this, ...)
  return(x)
}





#' @export
#' @return list of modelDatas
#' @rdname getModelD
#' @method getModelD modelPortfolio
getModelD.modelPortfolio <- function(this){
  getModelD.list(this$thisEnv$models)
}



#' @export
#' @rdname setModelD
#' @method setModelD modelPortfolio
setModelD.modelPortfolio <- function(this, x){
  setModelD.list(this$thisEnv$models, x)
}





#' Return sum money of models inside portfolio
#'
#' @export
#' @rdname getMoney
#' @method getMoney modelPortfolio
getMoney.modelPortfolio <- function(this){
  s <- 0
  for(model in this$thisEnv$models){
    s <- s + getMoney(model)
  }
  return(s)
}




#' Add distribution to list of models
#' 
#' This method add the same distribution to each model in list
#'
#' @export
#' @rdname addDistribution
#' @method addDistribution modelPortfolio
addDistribution.modelPortfolio <- function(this, ...){
  addDistribution.modelStrategy(this, ...)
}



#' Add distributions' constraint to list of models
#' 
#' This method add the same distributions' constraint to each model in list
#'
#' @export
#' @rdname addDistributionConstraint
#' @method addDistributionConstraint modelPortfolio
addDistributionConstraint.modelPortfolio <- function(this, ...){
  addDistributionConstraint.modelStrategy(this, ...)
}



#' Remove paramset from each strategy in list of models
#'
#' @export
#' @rdname deleteParamset
#' @method deleteParamset modelPortfolio
deleteParamset.modelPortfolio <- function(this, ...){
  deleteParamset.modelStrategy(this, ...)
}



aggregate_prepared_models <- function(this, ...){
    dots <- list(...)
    if(length(this$thisEnv$backtests) == 0){
        nms <- 'base'
        if('from' %in% names(dots)){
            nms <- dots[['from']]
        }
        aggregate_backtests(this, saveTo = nms)
    }
}


#' @export
#' @rdname plotPnL
#' @method plotPnL modelPortfolio
plotPnL.modelPortfolio <- function(this, ...){
  dots <- list(...)
  if('legend' %in% names(dots)){
    dots[['legend']] <- NULL
  }
  dots[['this']] <- this
  aggregate_prepared_models(this, ...)
  if(is.character(legs)){
    if(legs == 'all'){
      do.call("plotPnL.modelStrategy", args=dots)
    }else if(legs == 'sep'){
      dots[['this']] <- this$thisEnv$models
      do.call("plotPnL.list", args=dots)
    }
  }else if(is.numeric(legs)){
    dots[['this']] <- this$thisEnv$models[legs]
    do.call("plotPnL.list", args=dots)
  }
}



#' @export
#' @rdname plotCalendar
#' @method plotCalendar modelPortfolio
plotCalendar.modelPortfolio <- function(this, ...){
    aggregate_prepared_models(this, ...)
    plotCalendar.modelStrategy(this, ...)
}



#' @export
#' @rdname plotDrawdowns
#' @method plotDrawdowns modelPortfolio
plotDrawdowns.modelPortfolio <- function(this, ...){
    dots <- list(...)
    if('legend' %in% names(dots)){
        dots[['legend']] <- NULL
    }
    dots[['this']] <- this
    aggregate_prepared_models(this, ...)
    do.call("plotDrawdowns.modelStrategy", args=dots)
}


#' @export
#' @rdname setWeight
#' @params ... params 
setWeight <- function(this, ...){
    UseMethod('setWeight', this)
}

#' Set new weight for modelPortfolio object
#'
#' @export
#' @param this modelPortfolio type
#' @param w numeric type, coefficients strategy in portfolio, example: w = c(1,2)
#' @param money_function function type or const, three options are possible
#' 
#' 1) money_function = A = const, all strategies will get A money
#' 
#' 2) money_function = scalar function, all strategies will get money_function(start_money_in_strategy) money, example: min
#' 
#' 3) money_function = vector function, strategies[i] will get money_function(start_money_in_strategy)[i] money, example:
#' function \{ 200 + start_money_in_strategy\}
#' 
#' @rdname setWeight
#' @method setWeight modelPortfolio
setWeight.modelPortfolio <- function(this, w = NULL, money_function = NULL){
  length_model <- length(this$thisEnv$models)
  money_vec <- sapply(this$thisEnv$models, getMoney)
  if (is.null(money_function)){
    Money_f <- money_vec
  }else{
    if (is.numeric(money_function)){
      Money_f <- numeric(length_model) + money_function
    }else{
      Money_f <- money_function(money_vec)
      if (length(Money_f) == 1){
        Money_f <- Money_f + numeric(length_model)
      }
    }
  }
  for (i in 1:length_model){
    if (!is.null(w)){ 
      setMoney(this$thisEnv$models[[i]], Money_f[i] * w[i])
    }else{
      setMoney(this$thisEnv$models[[i]], Money_f[i])
    }
  }
  this$thisEnv$money <- sum(sapply(this$thisEnv$models, getMoney))
  return(invisible(NULL))
}


#' @export
c.modelPortfolio <- function(x, ...){
  dots <- list(...)
  res <- list(x)
  for(m in dots){
    if(class(m) %in% c('modelStrategy', 'modelPortfolio')){
      res <- c(res, list(m))
    }else if(class(m) == 'list'){
      res <- c(res, m)
    }
  }
  return(res)
}


#' Set amount of money to modelPortfolio object. 
#' 
#' It will spread that value across models inside it with old proportion.
#'
#' @export
#' @param this modelStrategy
#' @param x numeric type, new amount of money in strategy
#' @rdname setMoney
#' @method setMoney modelPortfolio
setMoney.modelPortfolio <- function(this, x){
  old_money <- this$thisEnv$money
  this$thisEnv$money <- x
  sapply(this$thisEnv$models, function(model){
    setMoney(model, getMoney(model) / old_money * x)
  })
  return(invisible(NULL))
}


