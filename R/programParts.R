#' Adds variables to strategy
#'
#' @param this modelStrategy
#' @param as character, name
#' @param evolution list, name of each element is the place where code will be executed, and value is quoted expression
#' @param ... callbacks
#' There are multiple places where you can add your code :
#' i. full name / short name -- description. 
#' 
#' 1. init -- in initialization period before main cycle
#' 
#' 2. after_tables / tables -- after initialization of rules tables
#' 
#' 3. after_coefs / coefs -- after initialization of coeffitients
#' 
#' 4. each_iter / iter -- in the beginnig of each iteration, before execution of rules
#' 
#' 5. after_enter_to_pos / enter -- Right after entering in position
#' 
#' 6. after_indicators / inds -- after initialization of indicators
#' 
#' 7. unrealized_money_last / unreal -- unrealized_money_last variable should be defined as unrealized pnl for each leg
#' 
#' 8. before_enter_to_pos / bexit -- before enter to position
#' 
#' 9. before_exit_from_pos / bexit -- before exit from position
#' 
#' 10. after_exit_from_pos / exit -- after exit from position
#' 
#' 11. data -- initalization of tables and derivative datasets
#' 
#' 12. start_cycle / on_start -- at the start of cycle before any update of spread
#' 
#' 
#' @export
#' @rdname addProgramPart
addProgramPart <- function(this,
                           as,
                           evolution, ...){
  UseMethod('addProgramPart', this)
}

#' @export
#' @rdname addProgramPart
#' @method addProgramPart modelStrategy
addProgramPart.modelStrategy <- function(this, as,  evolution = list(), ...){
    e <- this$thisEnv
    if(missing(as)){
        as <- paste0('pp', length(e[['pps']]) + 1)
    }
    #print(rlang::enexprs(...))
    x <- quote_list(rlang::enexprs(...), parent.frame())
    #print(x)
    evolution <- quote_list(rlang::enexpr(evolution), parent.frame())
    evolution <- c(evolution, x)
    e[['pps']][[as]] <- list(as = as, evolution = evolution)
}

#' Get list of variables(program parts)
#'
#' @param this modelStrategy
#' @export
#' @rdname getProgramParts
getProgramParts <- function(this){
  UseMethod('getProgramParts', this)
}

#' @export
#' @rdname getProgramParts
#' @method getProgramParts modelStrategy
getProgramParts.modelStrategy <- function(this){
  return(this$thisEnv$pps)
}

getPartByName <- function(name){
  return(switch(tolower(name),
                 unreal =,
                 unrealized=,
                 unrealized_money=,
                 unrealized_money_last= 'unrealized_money_last',
                 init = 'init',
                 after_table =,
                 table=,
                 tables=,
                 after_tables = 'after_tables',
                 after_coef =,
                 coefs =,
                 coef =,
                 after_coefs = 'after_coefs',
                 each = ,
                 each_iter = ,
                 iter = ,
                 iteration = 'each_iter',
                 before_enter=,
                 benter= ,
                 before_enter_to_pos = 'before_enter_to_pos',
                 before_exit=,
                 bexit= ,
                 before_exit_from_pos = 'before_exit_from_pos',
                 enter =,
                 after_enter_to_pos =,
                 enter_to_position = ,
                 after_enter_to_position =,
                 enter_position = 'after_enter_to_pos',
                 exit =,
                 after_exit_from_pos =,
                 exit_from_position = ,
                 after_exit_from_position =,
                 exit_position = 'after_exit_from_pos',
                 data =,
                 get_data=,
                 getData= 'data',
                 start_cycle=,
                 start_iter=,
                 on_start = 'start_cycle',
                 {
                   stop('Error with part name')
                 }))
}

#' This variable will be included to program and after calling perform function  it will be updated
#'
#' @param this modelStrategy
#' @param name character
#' @param value not NULL
#' @export
#' @rdname addStat
addStat <- function(this, name, value){
  UseMethod('addStat', this)
}

#' @export
#' @rdname addStat
#' @method addStat modelStrategy
addStat.modelStrategy <- function(this, name, value){
  this$thisEnv$stats_init[[name]] <- value
  this$thisEnv$stats[[name]] <- value
}

#' Deletes variable from stats
#'
#' @param this modelStrategy
#' @param name character
#' @export
#' @rdname removeStat
removeStat <- function(this, name){
  UseMethod('removeStat', this)
}

#' @export
#' @rdname removeStat
#' @method removeStat modelStrategy
removeStat.modelStrategy <- function(this, name){
  this$thisEnv$stats_init[[name]] <- NULL
  this$thisEnv$stats[[name]] <- NULL
}


#' Reinit all variables in stats
#'
#' @param this modelStrategy
#' @export
#' @rdname reinitStat
reinitStat <- function(this){
  UseMethod('reinitStat', this)
}

#' @export
#' @rdname reinitStat
#' @method reinitStat modelStrategy
reinitStat.modelStrategy <- function(this){
  if('stats' %in% names(this$thisEnv)){
    for(name in names(this$thisEnv$stats)){
      this$thisEnv$stats[[name]] <- this$thisEnv$stats_init[[name]]
    }
  }
}



