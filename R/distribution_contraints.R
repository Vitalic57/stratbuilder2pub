

#' Add distribution
#'
#' @param this modelStrategy
#' @param paramset.label character, label of paramset
#' @param component.type character, one of rule, indicator, params, lookback, lookforward, beta_fun
#' @param component.label character, name of component, argument 'as' is resposible for that. 
#' If component.type is equal to one of lookback, lookforward, beta_fun, then this argument should be missed
#' @param variable list with only one element, for example list(n = 1:10)
#' @param label character, name for this distribution
#'
#' @export
#' @rdname addDistribution
#' @method addDistribution modelStrategy
#' @examples
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
#'      condition = (Lag(spread,1) > Lag(bb[ ,'up'],1)) &
#'                   (spread < bb[, 'up']) &
#'                   (spread > bb[ ,'mavg']) &
#'                   (abs(spread - bb[, 'mavg'])  > n) ,
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
addDistribution.modelStrategy <- function(this,
                                          paramset.label,
                                          component.type,
                                          component.label,
                                          variable,
                                          label
){
  e <- this$thisEnv
  component.type <- switch(component.type,
                           rule = ,
                           rules ={
                             if(!(component.label %in% names(getRules(this, recalc = TRUE)))){
                               return()
                             }
                             'rules'
                           },
                           var = ,
                           vars ={
                             if(!(component.label %in% names(getVariables(this)))){
                               return()
                             }
                             'vars'
                           },
                           indicator =,
                           indicators = {
                             if(!(component.label %in% names(getIndicators(this)))){
                               return()
                             }
                             'indicators'
                           },
                           stop =,
                           stops={
                             if(!(component.label %in% names(getStops(this)))){
                               return()
                             }
                             'stops'
                           },
                           pm =,
                           pms = {
                             if(!(component.label %in% names(getPM(this)))){
                               return()
                             }
                             'pms'
                           },
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
  }else{
    l <- list(component.type = component.type,
              component.label = component.label,
              variable = variable)
  }
  if(!(paramset.label %in% names(e$paramsets))){
    e$paramsets[[paramset.label]] <- list(constraints = list(), distributions = list())
  }
  e$paramsets[[paramset.label]][['distributions']][[label]] <- l
}

      
    
#' Add distribution to list of models
#' 
#' This method add the same distribution to each model in list
#'
#' @param l list, list of modelStrategy objects
#' @param ... params for addDistribution
#'
#' @export
addDistribution.list <- function(l, ...){
  for(x in l){
    addDistribution(x, ...)
  }
}



#' Adds contraints to distributions
#'
#' @param this modelStrategy
#' @param paramset.label character, paramset label
#' @param expr expression, that contains names from labels of distributions
#' @param label character, name of the constraint
#'
#' @export
#' @rdname addDistributionConstraint
#' @method addDistributionConstraint modelStrategy
#' @examples
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
addDistributionConstraint.modelStrategy <- function(this,
                                                    paramset.label,
                                                    expr, #expression of constraint, it must contain only names
                                                    #in paramset.label
                                                    label #label of this contraint
){
  e <- this$thisEnv
  if(!(paramset.label %in% names(e$paramsets))){
    stop('no such a paramset.label in paramsets')
  }else{
    if(!missing(label)){
      e$paramsets[[paramset.label]][['constraints']][[label]] <- list(expr = substitute(expr))
    }else{
      len <- length(e$paramsets[[paramset.label]][['constraints']])
      e$paramsets[[paramset.label]][['constraints']][[len + 1]] <- list(expr = substitute(expr))
    }

  }
}


#' Add distributions' constraint to list of models
#' 
#' This method add the same distributions' constraint to each model in list
#'
#' @param l list, list of modelStrategy objects
#' @param ... params for addDistribution
#'
#' @export
addDistributionConstraint.list <- function(l, ...){
  for(x in l){
    addDistributionConstraint(x, ...)
  }
}

#' Removes paramset from strategy
#'
#' @param this modelStrategy
#' @param paramset.label name of paramset
#'
#' @export
#' @rdname deleteParamset
#' @method deleteParamset modelStrategy
deleteParamset.modelStrategy <- function(this, 
                                         paramset.label
){
  e <- this$thisEnv
  if(paramset.label %in% names(e$paramsets)){
    e$paramsets[[paramset.label]] <- NULL
  }
}


#' Removes paramset from each strategy in list of models
#'
#' @param l list, list of modelStrategy objects
#' @param ... params for addDistribution
#'
#' @export
deleteParamset.list <- function(l, ...){
  for(x in l){
    deleteParamset(x, ...)
  }
}




#' Gets list of distributions
#'
#' @param this modelStrategy
#' @param paramset.label character
#'
#' @return list of distributions
#' @export
getDistributions.modelStrategy <- function(this, paramset.label){
  return(this$thisEnv$paramsets[[paramset.label]]$distributions)
}






