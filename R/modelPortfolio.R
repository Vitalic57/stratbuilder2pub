#' creates modelPortfolio object
#'
#' @param ... list or models, list of models
#'
#' @return modelPortfolio object
#' @export
#'
modelPortfolio <- function(...){
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
  me <- list(thisEnv = thisEnv)
  
  
  ## Set the name for the class
  class(me) <- c("modelPortfolio")
  
  ## Define the value of the list within the current environment.
  assign('this', me, envir=thisEnv)
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
  addDistribution.list(this$thisEnv$models, ...)
}



#' Add distributions' constraint to list of models
#' 
#' This method add the same distributions' constraint to each model in list
#'
#' @export
#' @rdname addDistributionConstraint
#' @method addDistributionConstraint modelPortfolio
addDistributionConstraint.modelPortfolio <- function(this, ...){
  addDistributionConstraint.list(this$thisEnv$models, ...)
}



#' Remove paramset from each strategy in list of models
#'
#' @export
#' @rdname deleteParamset
#' @method deleteParamset modelPortfolio
deleteParamset.modelPortfolio <- function(this, ...){
  deleteParamset.list(this$thisEnv$models, ...)
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
    do.call("plotPnL.modelStrategy", args=dots)
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



