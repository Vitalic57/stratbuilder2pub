#' Adds variables to strategy
#'
#' @param this modelStrategy
#' @param as character, name
#' @param evolution list, name of each element is the place where code will be executed, and value is quoted expression
#' There are several places :
#' 1. init -- in initialization period before main cycle
#' 2. after_tables -- after initialization of rules tables
#' 3. after_coefs -- after initialization of coeffitients
#' 4. each_iter -- in the beginnig of each iteration, before execution of rules
#' 5. after_enter_to_pos -- Right after entering in position
#' 6. unrealized_money_last -- here you can change evolution of unrealized pnl. in this quote must be following piece of code
#' unrealized_money_last <-  <YOUR CODE HERE>
#' 7. before_enter_to_pos -- it executes just before entering to position
#' 8. befor_exit_from_pos -- it executes just before exiting from position
#' 9. data -- in this section you can define your datasets, that depend on modelData slots
#' 
#'
#' @export
addProgramPart.modelStrategy <- function(this,
                                      as,
                                      evolution = list()){
  e <- this$thisEnv
  e[['pps']][[as]] <- list(as        = as,
                            evolution = evolution)
}

#' Gets list of variables(program parts)
#'
#' @param this modelStrategy
#'
#' @export
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
                 {
                   stop('Error with part name')
                 }))
}

#' Set names of data, that should be saved after finishing of perform function
#'
#' @param this modelStrategy
#' @param s character, array of names
#'
#' @export
saveData.modelStrategy <- function(this, s){
  this$thisEnv$saveData <- s
}



#' This variable will be included to program and after calling perform function  it will be updated
#'
#' @param this modelStrategy
#' @param name character
#' @param value not NULL
#' @export
addStat.modelStrategy <- function(this, name, value){
  this$thisEnv$stats_init[[name]] <- value
  this$thisEnv$stats[[name]] <- value
}

#' Deletes variable from stats
#'
#' @param this modelStrategy
#' @param name character
#' @export
removeStat.modelStrategy <- function(this, name){
  this$thisEnv$stats_init[[name]] <- NULL
  this$thisEnv$stats[[name]] <- NULL
}


#' Reinit all variables in stats
#'
#' @param this modelStrategy
#' @export
reinitStat.modelStrategy <- function(this){
  if('stats' %in% names(this$thisEnv)){
    for(name in names(this$thisEnv$stats)){
      this$thisEnv$stats[[name]] <- this$thisEnv$stats_init[[name]]
    }
  }
}

#' Updates all variables in stats from environment
#'
#' @param e environment
#' @param this modelStrategy
#'
#' @export
updateStat.modelStrategy <- function(this, e){
  if('stats' %in% names(this$thisEnv)){
    for(name in names(this$thisEnv$stats)){
      this$thisEnv$stats[[name]] <- get(name, envir = e)
    }
  }
}

#' Updates environment
#'
#' @param e environment
#' @param this modelStrategy
#'
#' @export
extractStat.modelStrategy <- function(this, e){
  if('stats' %in% names(this$thisEnv)){
    list2env(this$thisEnv$stats, envir = e)
  }
}


