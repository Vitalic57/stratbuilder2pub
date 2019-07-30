

#' Adds indicators to strategy
#'
#' This indicator will be calculated each time when spread update coefs
#' indicators must return data.frame or matrix or array
#'
#' @param this model
#' @param args list, the first argument should be name, it should be equal to function (your indicator), other arguments of list
#' will be passed to this function
#' @param as character, local name of this indicator
#' @param lookback numeric, how many points does it need to do calculation
#' @param ... addtional params
#' 
#' @export
#' @rdname addIndicator
addIndicator <- function(this, args, as, lookback, ...){
  UseMethod('addIndicator', this)
}

#' @export
#' @examples
#' \dontrun{
#' addIndicator(this, args = list(name = BBands, HLC = quote(spread), n = 20, sd = 1), as = 'bb',
#'     lookback = 100)
#' }
#' @rdname addIndicator
#' @method addIndicator modelStrategy
addIndicator.modelStrategy <- function(this, args, as, lookback, ...){
  e <- this$thisEnv
  if(missing(lookback)){
    lookback <- 0
    for(name in names(args)){
      if(is.numeric(args[[name]])[1] && length(args[[name]]) == 1){
        lookback <- max(lookback, args[[name]])
      }
    }
  }
  if(getMaxLookback(this) < lookback){
    setMaxLookback(this, lookback)
  }
  if(missing(as)){
    as <- paste0('indicator', length(e[['indicators']]) + 1)
  }
  as <- gsub('\\.','_',as)
  e$indicators[[as]] <- c(list(args = args,
                               as = as,
                               lookback = lookback),list(...))
}






#' Adds rule or variable to strategy 
#' 
#' Conditions will be calculated and then action will be done according to type. osFun arg is needed for definition of 
#' how much money you want to invest in enter type rules. 
#'
#' @param this model
#' @param condition expression, it must use name of indicators and local args
#' @param as character, local name of rule
#' @param args list, arguments that can be used in condition
#' @param type character, type must be enter or exit
#' @param side numeric, 1 or -1, this parameter should be specified only for enter type rules
#' @param oco character, environment of rule
#' @param osFun function that calculates how many units of spread should be bought or sold
#' @param osFun_args alist of args of osFun function
#' @param pathwise logical, if it is FALSE, then calculation of rules  will be performed on the tables once per 
#' computation of coefficients and condition should return logical vector, 
#' otherwise calculation of rules will be performed on each iteration and condition should return logical scalar
#' 
#' @export
#' @rdname addRule
addRule <- function(this,
                    condition, 
                    as, 
                    args = list(),
                    type, 
                    side, 
                    oco = 'base',  
                    osFun = sameMoneyOs, 
                    osFun_args = alist(amount = getMoney(this)),
                    pathwise = FALSE){
  UseMethod('addRule', this)
}

#' @export
#' @examples
#' \dontrun{
#'   addRule(this, as = 'bb_dn_up',
#'           condition = (Lag(spread,1) < Lag(bb[,'dn'],1)) &
#'                       (spread > bb[,'dn']) &
#'                       (spread < bb[,mavg']) &
#'                       (abs(spread - bb[,'mavg'])/spread  > n) ,
#'           type = 'enter',
#'           args = list(n = 0.005),
#'           side = 1,
#'           oco = 'long',
#'           osFun = sameMoneyOs,
#'           osFun_args = alist(amount = 5000000))
#'           
#'   addRule(this, as = 'bb_dn_up1',
#'           condition = (Lag(spread < bb[,'dn'],1))[iii,] &
#'                       (spread > bb[,'dn'])[iii,] &
#'                       (spread < bb[,mavg'])[iii,] &
#'                       (abs(spread - bb[,'mavg'])/spread  > n)[iii,] ,
#'           type = 'enter',
#'           pathwise = TRUE,
#'           args = list(n = 0.005),
#'           side = 1,
#'           oco = 'long',
#'           osFun = sameMoneyOs,
#'           osFun_args = alist(amount = 5000000))
#' }
#' @rdname addRule
#' @method addRule modelStrategy
addRule.modelStrategy <- function(this,
                                  condition, 
                                  as, 
                                  args = list(),
                                  type, 
                                  side, 
                                  oco = 'base',  
                                  osFun = sameMoneyOs, 
                                  osFun_args = alist(amount = getMoney(this)),
                                  pathwise = FALSE
){
  if(all(c('enter','exit') != type)){
    stop('wrong type! It must be enter or exit')
  }
  if(missing(side)){
    if(type != 'exit'){
      stop("please provide side of rule, it must be 1 or -1")
    }else{
      side <- 0
    }
  }else if(all(c(1,-1) != side)){
    stop('wrong type! It must be 1 or -1')
  }
  e <- this$thisEnv
  if(missing(as)){
    as <- paste0('rule', length(e[['rules']]) + 1)
  }
  as <- gsub('\\.','_',as)
  if(type == 'enter'){
    e$rules[[as]] <- list(condition = substitute(condition),
                          as = as,
                          args = args,
                          type = type,
                          side = side,
                          oco = oco,
                          osFun = osFun,
                          osFun_args = osFun_args,
                          pathwise = pathwise
    )
  }else if(type == 'exit'){
    e$rules[[as]] <- list(condition = substitute(condition),
                          as = as,
                          args = args,
                          type = type,
                          side = side,
                          oco = oco,
                          pathwise = pathwise
    )
  }
  
}




#' Get inforamtion about indicators in the model
#'
#' @param this model
#'
#' @return list of inforamation
#' 
#' @export
#' @rdname getIndicators
getIndicators <- function(this){
  UseMethod('getIndicators', this)
}

#' @export
#' @rdname getIndicators
#' @method getIndicators modelStrategy
getIndicators.modelStrategy <- function(this){
  return(this$thisEnv$indicators)
}

#' Get inforamtion about rules in the model
#'
#' @param this modelStrategy
#' @param pathwise logical, if TRUE only pathwise rules will be returned
#' @param recalc logical, if TRUE then rules will be recalculated
#'
#' @return list of inforamation
#' @export
#' @rdname getRules
getRules <- function(this, pathwise = FALSE, recalc = FALSE){
  UseMethod('getRules', this)
}

#' @export
#' @rdname getRules
#' @method getRules modelStrategy
getRules.modelStrategy <- function(this, pathwise = FALSE, recalc = FALSE){
  if(length(pathwise) == 0){
    return(this$thisEnv$rules)
  }else{
    if(recalc){
      p <- numeric()
      np <- numeric()
      for(i in seq_along(this$thisEnv$rules)){
        if(this$thisEnv$rules[[i]]$pathwise){
          p[length(p) + 1] <- i
        }else{
          np[length(np) + 1] <- i
        }
      }
      this$thisEnv$rules_path_ind <- p
      this$thisEnv$rules_norm_ind <- np
    }
    if(pathwise){
      return(this$thisEnv$rules[this$thisEnv$rules_path_ind])
    }else{
      return(this$thisEnv$rules[this$thisEnv$rules_norm_ind])
    }
  }
}



#' Remove rule by name
#'
#' @param this modelStrategy
#' @param name character, name of rule
#' @export
#' @rdname removeRule
removeRule <- function(this, name){
  UseMethod('removeRule', this)
}

#' @export
#' @rdname removeRule
#' @method removeRule modelStrategy
removeRule.modelStrategy <- function(this, name){
  table <- getRules(this)
  names <- sapply(table, function(x) x$as)
  ind <- which(names == name)
  if(length(ind) > 0){
    eval(substitute(rules[[ind]] <- NULL), envir = this$thisEnv)
  }
}



#' Remove indicator by name
#'
#' @param this modelStrategy
#' @param name character, name of indicator
#' @export
#' @rdname removeIndicator
removeIndicator <- function(this, name){
  UseMethod('removeIndicator', this)
}

#' @export
#' @rdname removeIndicator
#' @method removeIndicator modelStrategy
removeIndicator.modelStrategy <- function(this, name){
  table <- getIndicators(this)
  names <- sapply(table, function(x) x$as)
  ind <- which(names == name)
  if(length(ind) > 0){
    eval(substitute(indicators[[ind]] <- NULL), envir = this$thisEnv)
  }
}


