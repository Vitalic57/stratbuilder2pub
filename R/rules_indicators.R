try_eval_list <- function(args, env){
    res <- list()
    if(length(args) > 1){
        nms <- names(args)
        for(i in 2:length(args)){
            res[[nms[i]]] <- tryCatch({
                eval(args[[i]], envir = env)
            }, error = function(e){
                args[[i]]
            }
            )
        }
    }
    return(res)
}

quote_list <- function(args, env=NULL){
    res <- list()
    if(length(args) > 0){
        nms <- names(args)
        if(!is.null(nms)){
            for(i in 1:length(args)){
                if(nms[i] == ""){
                    next
                }
                if(is.call(args[[i]]) && as.character(args[[i]][[1]]) == "quote" || is.symbol(args[[i]])){
                    res[[nms[i]]] <- eval(args[[i]], envir = env)
                }else{
                    res[[nms[i]]] <- args[[i]]
                }
            }
        }
        
    }
    return(res)
}

unquote <- function(x){
    x <- rlang::enexpr(x)
    if(is.symbol(x) || as.character(x[[1]]) == 'quote'){
        eval(x, envir = parent.frame())
    }else{
        x
    }
}


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
    #in dots:
    # plot -- main or new tab for graphic
    # columns -- which columns should be plotted
    # col -- colour of lines
    args <- try_eval_list(rlang::enexpr(args), env = parent.frame())
    
    e <- this$thisEnv
    lookback <- rlang::enexpr(lookback)
    if(missing(lookback)){
        res <- 0
        for(name in names(args)){
            if(is.numeric(args[[name]])[1] && length(args[[name]]) == 1){
                if(args[[name]] > res){
                    lookback <- parse(text=name)[[1]]
                    res <- args[[name]]
                }
            }
        }
    }else if(is.character(lookback)){
        lookback <- parse(text=lookback)[[1]]
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
#' @param side numeric, 1 or -1
#' @param oco character, environment of rule
#' @param osFun function that calculates how many units of spread should be bought or sold
#' @param osFun_args alist of args of osFun function
#' @param pathwise logical, if it is FALSE, then calculation of rules and variables will be performed on the tables once per 
#' computation of coefficients, otherwise calculation of rules and variables will be performed on the scalars on each iteration
#' @param money numeric / expression, how much money should be in position after execution of rule. 
#' Position manager will be added for correcting positions at every step
#' @param money_const numeric / expression, the same as money, but without Position manager
#' @param betas numeric / expression, number of instruments to buy and sell, position manager will be added 
#' @param betas_const numeric / expression, the same as betas, but without position manager
#' @param by_money logical, if it is TRUE then expression in betas argument will be interpreted as money positions in instruments, else positions will be in pieces
#' @param ... params
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
                  pathwise = FALSE,
                  money,
                  money_const,
                  betas,
                  betas_const,
                  by_money = TRUE,
                  ...
){
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
                                  pathwise = FALSE,
                                  money,
                                  money_const,
                                  betas,
                                  betas_const,
                                  by_money = TRUE,
                                  ...
){
    if(missing(type)){
        type <- 'enter'
    }
    if(all(c('enter','exit') != type)){
        stop('wrong type! It must be enter or exit')
    }
    if(missing(side)){
        if(type == 'enter'){
            stop("please provide side of rule, it must be 1 or -1")
        }else{
            side <- 0
        }
    }else if(all(c(1,-1) != side) && type != 'none'){
        stop('wrong side! It must be 1 or -1')
    }
    e <- this$thisEnv
    if(missing(as)){
        as <- paste0('rule_', length(e$rules) + 1)
    }
    as <- gsub('\\.','_',as)
    if(missing(oco) && type == 'enter'){
        oco <- as
    }
    
    if(as %in% names(e$rules)){
        if('pm' %in% e$rules[[as]]){
            this$thisEnv$positionManagers[[e$rules[[as]][['pm']]]] <- NULL
        }
    }
    if(type == 'enter'){
        if(oco == 'all'){
            stop("Oco can't be equal to all when type is enter")
        }
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
        if(!missing(money_const)){
            e$rules[[as]][['money_const']] <- rlang::enexpr(money_const)
        }
        if(!missing(betas_const)){
            e$rules[[as]][['betas_const']] <- rlang::enexpr(betas_const)
            e$rules[[as]][['by_money']] <- by_money
        }
        if(!missing(money) || !missing(betas)){
            pm_name <- paste0('pm', length(getPM(this)))
            e$rules[[as]][['pm']] <- pm_name
            reb_q <- rlang::call2("list", cond = rlang::expr(rule_enter == !!as))
            if(!missing(money)){
                reb_q[['money']] <- rlang::enexpr(money)
                if(missing(money_const)){
                    e$rules[[as]][['money_const']] <- 0
                }
            }
            if(!missing(betas)){
                reb_q[['betas']] <- rlang::enexpr(betas)
                reb_q[['by_money']] <- by_money
            }
            addPM(this,
                  oco = oco,
                  as = pm_name,
                  rebalance = !!reb_q,
                  rule_name = as
            )
        }
        
    }else if(type  == 'exit'){
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
#'
#' @return list of inforamation
#' @export
#' @rdname getRules
getRules <- function(this){
  UseMethod('getRules', this)
}

#' @export
#' @rdname getRules
#' @method getRules modelStrategy
getRules.modelStrategy <- function(this){
  return(this$thisEnv$rules)
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



#' Add multiple indicators to strategy
#'
#' These indicators will be calculated each time when spread updates coefficients
#' indicators must return data.frame or matrix or array
#'
#' @param this model
#' @param expr expression
#' @param args list, arguments of expression
#' @param lookback numeric, periods of time for calculation
#' @param names character vector, names of indicators to include in.
#' @param ... params
#' @param as character, name of set of indicators
#' 
#' @export
#' @rdname addIndicators
#' @method addIndicators modelStrategy
addIndicators.modelStrategy <- function(this, expr, names, as, lookback = 0, args = list(), ...){
  e <- this$thisEnv
  
  if(getMaxLookback(this) < lookback){
    setMaxLookback(this, lookback)
  }
  if(missing(as)){
    as <- paste0('indicator', length(e[['indicators']]) + 1)
  }
  as <- gsub('\\.','_',as)
  e$indicators[[as]] <- c(list(args = args,
                               as = as,
                               lookback = lookback,
                               names = names,
                               expr = rlang::enexpr(expr)), list(...))
}



quote_list <- function(args, env=NULL){
    res <- list()
    if(length(args) > 0){
        nms <- names(args)
        if(!is.null(nms)){
            for(i in 1:length(args)){
                if(nms[i] == ""){
                    next
                }
                if(is.call(args[[i]]) && as.character(args[[i]][[1]]) == "quote" || is.symbol(args[[i]])){
                    res[[nms[i]]] <- eval(args[[i]], envir = env)
                }else{
                    res[[nms[i]]] <- args[[i]]
                }
            }
        }
        
    }
    return(res)
}
