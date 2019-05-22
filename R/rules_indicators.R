

#' Adds indicators to strategy
#'
#' This indicator will be calculated each time when spread update coefs
#' indicators must return data.frame or matrix or array
#'
#' @param this modelStrategy
#' @param args list, the first argument should be name, it should be equal to function (your indicator), other arguments of list
#' will be passed to this function
#' @param as character, local name of this indicator
#' @param lookback numeric, how many points does it need to do calculation
#' @param ... addtional params
#'
#' @export
#' @rdname addIndicator
#' @method addIndicator modelStrategy
#' @examples
#' addIndicator(this, args = list(name = BBands, HLC = quote(spread), n = 20, sd = 1), as = 'bb',
#'     lookback = 100)
addIndicator.modelStrategy <- function(this, args, as, lookback, ...){
  e <- this$thisEnv
  if(missing(lookback)){
    for(name in names(args)){
      lookback <- 0
      if(is.numeric(args[[name]])[1] && length(args[[name]]) == 1){
        lookback <- max(lookback, args[[name]])
      }
    }
  }
  if(getMaxLookback(this) < lookback){
    setMaxLookback(this, lookback)
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
#' @param this modelStrategy
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
#' @method addRule modelStrategy
#' @examples
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
addRule.modelStrategy <- function(this, #object
                                  condition, #condition when to apply rule,
                                  #it can include such variables as
                                  #spread and names of indicators
                                  as, #name of rule
                                  args = list(),
                                  type, #enter or exit
                                  side, #1 for buy and -1 for sell
                                  oco = 'base',  #environment of rule
                                  osFun = proportionOs, # how much money you want to use in rule
                                  osFun_args = alist(proportion = 1),
                                  pathwise = FALSE
){
  # if(type == 'var'){
  #   e <- this$thisEnv
  #   as <- gsub('\\.','_',as)
  #   e$rules[[as]] <- list(condition = substitute(condition),
  #                         as = as,
  #                         args = args,
  #                         type = type,
  #                         side = 0,
  #                         oco = NULL,
  #                         osFun = NULL,
  #                         osFun_args = NULL,
  #                         pathwise = pathwise
  #   )
  # }else{
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
    as <- gsub('\\.','_',as)
    if(any(sapply(e$rules,function(x) x$as) == as)){
      warning('"as" must be unique for diffrent rules. Previous rule with the same name will be deleted.')
    }
    #formals(osFun) <- modify.args(formals(osFun), osFun_args)
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
  # }
  
}




#' Gets inforamtion about indicators in the model
#'
#' @param this modelStrategy
#'
#' @return list of inforamation
#' @export
#' @rdname getIndicators
#' @method getIndicators modelStrategy
getIndicators.modelStrategy <- function(this){
  return(this$thisEnv$indicators)
}

#' Gets inforamtion about rules in the model
#'
#' @param this modelStrategy
#'
#' @return list of inforamation
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



#' Removes rule by name
#'
#' @param this modelStrategy
#' @param name character, name of rule
#'
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



#' Removes indicator by name
#'
#' @param this modelStrategy
#' @param name character, name of indicator
#'
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


