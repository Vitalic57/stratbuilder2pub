set_names_list <- function(l){
  x <- names(l)
  if(is.null(x)){
    names(l) <- paste0('m_', seq_len(length(l)))
  }else{
    ind <- x == ''
    new_names <- x
    new_names[ind] <- paste0('m_', seq_len(length(l)))[ind]
    names(l) <- new_names
  }
  return(l)
}


#' Get dates by indexes from modelData
#'
#' @param this modelStrategy
#' @param indexes numeric vector or NULL
#'
#' @return vector of dates
#' @export
#' @rdname getDateByIndex
getDateByIndex <- function(this, indexes = NULL){
  UseMethod('getDateByIndex', this)
}

#' @export
#' @rdname getDateByIndex
#' @method getDateByIndex modelStrategy
getDateByIndex.modelStrategy <- function(this, indexes = NULL){
  fun <- function(model, indexes = NULL){
    if(!is.null(indexes)){
      return(index(model$data_margin)[indexes])
    }else{
      return(index(model$data_margin))
    }
  }
  return(fun(getModelD(this), indexes))
}



#' @export
#' @rdname getDateByIndex
#' @method getDateByIndex modelPortfolio
getDateByIndex.modelPortfolio <- function(this, indexes = NULL){
  if(is.null(indexes)){
    return(this$thisEnv$backtests[[1]]$results$dates)
  }
  return(this$thisEnv$backtests[[1]]$results$dates[indexes])
}


#' @param ... arguments 
#'
#' @export
#' @rdname getModelD
#' @method getModelD list
getModelD.list <- function(this, ...){
  lapply(this, function(x){
    getModelD(x, ...)
  }) %>%
    set_names(names(this))
}



#' @export
#' @rdname setModelD
#' @method setModelD list
setModelD.list <- function(this, x, ...){
  for(i in seq_along(this)){
    if(class(x)[1] == 'modelData'){
      setModelD(this[[i]], x, ...)
    }else{
      setModelD(this[[i]], x[[i]], ...)
    }
  }
}



#' Set modelData object to modelStrategy object
#'
#' @param this modelStrategy
#' @param x modelData
#' @param clearBacktests logical, if TRUE then all backtests will be deleted
#' @export
#' @rdname setModelD
setModelD <- function(this, x, clearBacktests = TRUE){
  UseMethod('setModelD', this)
}

#' @export
#' @rdname setModelD
#' @method setModelD modelStrategy
setModelD.modelStrategy <- function(this, x, clearBacktests = TRUE){
  e <- this$thisEnv
  e$modelD <- x
  # setCommissionTable(this)
  # setSlippageTable(this)
  if(clearBacktests){
    e$backtests <- list()
  }
  x <- lapply(names(e$paramsets), function(x){
    e$paramsets[[x]]$results <- NULL
    e$paramsets[[x]]$report <- NULL
  })
  #processBetaTable(this)
}


#' Get modelData object from modelStrategy object
#'
#' @param this modelStrategy
#' @export
#' @rdname getModelD
getModelD <- function(this){
  UseMethod('getModelD', this)
}

#' @export
#' @rdname getModelD
#' @method getModelD modelStrategy
getModelD.modelStrategy <- function(this){
  return(this$thisEnv$modelD)
}



#' @export
#' @rdname getModelD
#' @method getModelD modelPortfolio
getModelD.modelPortfolio <- function(this){
  getModelD.list(this$thisEnv$models)
}




#' @param ... params
#'
#' @export
#' @rdname setModelD
#' @method setModelD modelPortfolio
setModelD.modelPortfolio <- function(this, x, ...){
  setModelD.list(this$thisEnv$models, x, ...)
}



