

#' Add distribution
#'
#' @param this model
#' @param paramset.label character, label of paramset, this argument can be missed
#' @param component.type character, one of the followng names: rule, indicator, params, lookback, lookforward, beta_fun
#' @param component.label character, name of component, argument 'as' is resposible for that. You can write multiple labels. 
#' If component.type is equal to one of lookback, lookforward, beta_fun, then this argument should be missed
#' @param variable list with only one element, for example list(n = 1:10)
#' @param label character, name for this distribution
#' 
#' @export
#' @rdname addDistribution
addDistribution <- function(this,
                            paramset.label,
                            component.type,
                            component.label,
                            variable,
                            label){
  UseMethod('addDistribution', this)
}

#' @export
#' @examples
#' \dontrun{
#' addIndicator(this, args = list(name = BBands, HLC = quote(spread), n = 20, sd = 1), as = 'bb')
#' addDistribution(this,
#'     component.type = 'indicator',
#'     component.label = 'bb',
#'     variable = list(sd = seq(0.5,3,0.05)),
#'     label = 'bb.sd'
#' )
#'
#' addRule(this, as = 'bb_up_dn',
#'      condition = (Lag(spread,1) > Lag(bb[ ,'up'],1)) &
#'                   (spread < bb[, 'up']) &
#'                   (spread > bb[ ,'mavg']) &
#'                   (abs(spread - bb[, 'mavg'])  > n) ,
#'      type = 'enter',
#'      args = list(n = 0.005),
#'      side = -1,
#'      oco = 'short')
#' addDistribution(this,
#'     component.type = 'rule',
#'     component.label = 'bb_up_dn',
#'     variable = list(n = seq(0.005,0.03,0.002)),
#'     label = 'my_distr'
#' )
#' }
#' @rdname addDistribution
#' @method addDistribution modelStrategy
addDistribution.modelStrategy <- function(this,
                                          paramset.label,
                                          component.type,
                                          component.label,
                                          variable,
                                          label
){
  if(!is.list(variable)){
    stop("Variable should be a list")
  }
  if(missing(paramset.label)){
    paramset.label <- 1
  }
  e <- this$thisEnv
  if(missing(label)){
    label <- paste0('distribution', length(e$paramsets[[paramset.label]][['distributions']]) + 1)
  }else{
    label <- make.names(label)
  }
  if(length(variable) > 1){
    for(i in seq_along(variable)){
      addDistribution(this,
                      paramset.label=paramset.label,
                      component.type=component.type,
                      component.label=component.label,
                      variable = list(variable[[i]]) %>% set_names(names(variable)[i]),
                      label = paste(label, names(variable)[i], sep = '.')
      )
      return(invisible(NULL))
    }
  }
  component.type <- switch(component.type,
                           rule = ,
                           rules ={
                             if(!all(component.label %in% names(getRules(this)))){
                               warning('No such component.type in rules')
                               return()
                             }
                             'rules'
                           },
                           indicator =,
                           indicators = {
                             if(!all(component.label %in% names(getIndicators(this)))){
                               warning('No such component.type in indicators')
                               return()
                             }
                             'indicators'
                           },
                           # stop =,
                           # stops={
                           #   if(!(component.label %in% names(getStops(this)))){
                           #     return()
                           #   }
                           #   'stops'
                           # },
                           pm =,
                           pms = {
                             if(!all(component.label %in% names(getPM(this)))){
                               warning('No such component.type in position managers')
                               return()
                             }
                             'pms'
                           },
                           #pymodel = 'pymodel',
                           params =,
                           param =,
                           par =,
                           pars =
                           {
                             component.label <- switch (tolower(component.label),
                                                        rule = ,
                                                        rules = 'rules',
                                                        pm = ,
                                                        pms = 'pms',
                                                        # var = ,
                                                        # vars =,
                                                        # variable = ,
                                                        # variables = 'vars',
                                                        pp = ,
                                                        programparts =,
                                                        programpart =,
                                                        program = 'pps',
                                                        # stop = ,
                                                        # stops = 'stops',
                                                        indicator=,
                                                        indicators='indicators',
                                                        beta=,
                                                        pymodel='pymodel',
                                                        beta_fun= 'beta_fun',
                                                        {warning('wrong component label');return()}
                             )
                             'params'
                           },
                           {
                             if (missing(component.label)) {
                               component.label <- "x"
                             }
                             component.type
                           })
  #print(variable[[1]])
  if(is.list(variable[[1]]) && any(sapply(variable[[1]], is.function))){
      #print(1)
      ee <- new.env()
      q <- substitute(variable)[[-1]]
      #print(q)
      if (is.symbol(q)) {
          if (!is.null(names(variable[[1]]))) {
              nms <- names(variable[[1]])
              nms[nms == ""] <- paste(q, which(nms == ""), sep = '')
          } else {
              nms <- paste(q, 1:length(variable[[1]]), sep = '')
          }
      } else {
          nms <- sapply(q[-1], deparse)
      }
      for(i in 1:length(nms)){
          assign(nms[i], variable[[1]][[i]], envir = ee)
      }
      nms <- list(nms)
      names(nms) <- names(variable)
      l <- list(component.type = component.type,
                component.label = component.label,
                env = ee,
                variable = nms)
  } else  if (is.function(variable[[1]])) {
    #print(2)
      ee <- new.env()
      func_name <- deparse(substitute(variable)[[-1]])
      print(func_name)
      assign(func_name, variable[[1]], envir = ee)
      func_name <- list(func_name)
      names(func_name) <- names(variable)
      l <- list(component.type = component.type,
                component.label = component.label,
                env = ee,
                variable = func_name)
      
  } else {
    #print(3)
      l <- list(component.type = component.type,
                component.label = component.label,
                variable = variable)
  }
  if(!(paramset.label %in% names(e$paramsets)) && !is.numeric(paramset.label) || length(e$paramsets) == 0){
    e$paramsets[[paramset.label]] <- list(constraints = list(), distributions = list())
  }
  e$paramsets[[paramset.label]][['distributions']][[label]] <- l
}




#' @param ... params for addDistribution
#'
#' @export
#' @rdname addDistribution
#' @method addDistribution list
addDistribution.list <- function(this, ...){
  for(x in this){
    addDistribution(x, ...)
  }
}



#' Add constraints to distributions
#'
#' @param this model
#' @param paramset.label character, paramset label
#' @param expr expression, that contains names from labels of distributions
#' @param label character, name of the constraint
#' 
#' @export
#' @rdname addDistributionConstraint
addDistributionConstraint <- function(this,
                                      paramset.label,
                                      expr, 
                                      label){
  UseMethod('addDistributionConstraint', this)
}

#' @export
#' @examples
#' \dontrun{
#' addIndicator(this, args = list(name = BBands, HLC = quote(spread), n = 20, sd = 1), as = 'bb',
#'         lookback = 100)
#' addDistribution(this,
#'     paramset.label = paramset,
#'     component.type = 'indicator',
#'     component.label = 'bb',
#'     variable = list(sd = seq(0.5,3,0.05)),
#'     label = 'bb.sd'
#' )
#'
#' addRule(this, as = 'bb_up_dn',
#'      condition = (Lag(spread,1) > Lag(bb[, 'up'],1)) &
#'                   (spread < bb[, 'up']) &
#'                   (spread > bb[, 'mavg']) &
#'                   (abs(spread - bb[ ,'mavg'])/spread  > n) ,
#'      type = 'enter',
#'      args = list(n = 0.005),
#'      side = -1,
#'      oco = 'short',
#'      osFun = stratbuilder2pub:::sameMoneyOs,
#'      osFun_args = alist(amount = 5000000))
#' addDistribution(this,
#'     paramset.label = paramset,
#'     component.type = 'rule',
#'     component.label = 'bb_up_dn',
#'     variable = list(n = seq(0.005,0.03,0.002)),
#'     label = 'my_distr'
#' )
#'
#' addDistributionConstraint(this,
#'     paramset.label = paramset,
#'     expr = bb.sd > my_distr * 100
#' )
#' }
#' @rdname addDistributionConstraint
#' @method addDistributionConstraint modelStrategy
addDistributionConstraint.modelStrategy <- function(this,
                                                    paramset.label,
                                                    expr, 
                                                    label
){
  e <- this$thisEnv
  if(missing(paramset.label)){
    paramset.label <- 1
  }
  if(missing(label)){
    label <- paste0('constraint', length(e$paramsets[[paramset.label]][['constraints']]) + 1)
  }
  if(!(paramset.label %in% names(e$paramsets)) && !is.numeric(paramset.label)){
    stop('no such a paramset.label in paramsets')
  }else{
    #if(!missing(label)){
    e$paramsets[[paramset.label]][['constraints']][[label]] <- list(expr = substitute(expr))
    # }else{
    #   len <- length(e$paramsets[[paramset.label]][['constraints']])
    #   e$paramsets[[paramset.label]][['constraints']][[len + 1]] <- list(expr = substitute(expr))
    # }

  }
}



#' @param ... params for addDistribution
#'
#' @export
#' @rdname addDistributionConstraint
#' @method addDistributionConstraint list
addDistributionConstraint.list <- function(this, ...){
  for(x in this){
    addDistributionConstraint(x, ...)
  }
}

#' Remove paramset from strategy
#'
#' @param this model
#' @param paramset.label name of paramset
#' 
#' @export
#' @rdname deleteParamset
deleteParamset <- function(this, 
                           paramset.label){
  UseMethod('deleteParamset', this)
}

#' 
#' @export
#' @rdname deleteParamset
#' @method deleteParamset modelStrategy
deleteParamset.modelStrategy <- function(this, 
                                         paramset.label
){
  e <- this$thisEnv
  if(missing(paramset.label)){
    paramset.label <- 1
  }
  if(paramset.label %in% names(e$paramsets) || is.numeric(paramset.label)){
    e$paramsets[[paramset.label]] <- NULL
  }
}



#' @param ... params for addDistribution
#'
#' @export
#' @rdname deleteParamset
#' @method deleteParamset list
deleteParamset.list <- function(this, ...){
  for(x in this){
    deleteParamset(x, ...)
  }
}




#' Get list of distributions
#'
#' @param this model
#' @param paramset.label character
#'
#' @return list of distributions
#' 
#' @export
#' @rdname  getDistributions
getDistributions <- function(this, paramset.label){
  UseMethod('getDistributions', this)
}

#' @export
#' @rdname getDistributions
#' @method getDistributions modelStrategy
getDistributions.modelStrategy <- function(this, paramset.label){
  return(this$thisEnv$paramsets[[paramset.label]]$distributions)
}






