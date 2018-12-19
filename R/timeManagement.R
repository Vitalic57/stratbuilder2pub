tstr_to_sec <- function(t_str) {
  #"09:00:00" to sec of day
  as.numeric(as.POSIXct(paste("1970-01-01", t_str), "UTC")) %% 86400L
}

sec_to_tstr <- function(sec){
  hours <- sec %/% (60*60)
  if(hours == 0){
    hours <- '00'
  }
  mins <- (sec%%(60*60)) %/% 60
  if(mins < 10 ){
    mins <- paste0(0,mins)
  }
  secs <- sec %% 60
  if(secs  < 10){
    secs <- paste0(0,secs)
  }
  return(paste0(hours,':',mins,':',secs))
}

sec_of_day = function(x){
  lt = as.POSIXlt(index(x), tz = indexTZ(x))
  lt$hour *60*60 + lt$min*60 + lt$sec
}


#'
#' Adds time when trading is permitted
#'
#' @param this modelStrategy
#' @param type character, one of enter, open, exit, close
#' @param l list, each element is a string that represents time
#'
#' @rdname addTradeTime
#' @method addTradeTime modelStrategy
#' @export
#' @example
#'
#' addTradeTime(this, 'open', list('10:00:00','12:00:00'))
addTradeTime.modelStrategy <- function(this, type, l){
  #type can be open or close
  #l is time of start and end
  switch(type,
         enter =,
         open ={
           ind <- 'open'
         },
         exit =,
         close = {
           ind <- 'close'
         },
         {
           return()
         })
  this$thisEnv$tradeTime[[ind]][[length(this$thisEnv$tradeTime[[ind]]) + 1]] <- lapply(l,tstr_to_sec)
}

#' Gets tarding time of strategy
#'
#' @param this modelStrategy
#' @param type character, one of  c('all', 'enter', 'open', 'exit', 'close')
#'
#' @rdname getTradeTime
#' @method getTradeTime modelStrategy
#' @export
#'
getTradeTime.modelStrategy <- function(this, type = 'all'){
  switch(type,
         all = {
           this$thisEnv$tradeTime
         },
         enter =,
         open = {
           this$thisEnv$tradeTime[['open']]
         },
         exit=,
         close = {
           this$thisEnv$tradeTime[['close']]
         })
}

#' Remove trading time by type
#'
#' @param this modelStrategy
#' @param type character, one of c('all', 'enter', 'open', 'exit', 'close')
#'
#' @rdname clearTradeTime
#' @method clearTradeTime modelStrategy
#' @export
#'
clearTradeTime.modelStrategy <- function(this, type = 'all'){
  switch(type,
         all = {
           this$thisEnv$tradeTime <- list(open = list(), close = list())
         },
         enter =,
         open = {
           this$thisEnv$tradeTime[['open']] <- list()
         },
         exit =,
         close = {
           this$thisEnv$tradeTime[['close']] <- list()
         })
}

#' Prints trading time in format of time
#'
#' @param this modelStrategy
#' @param type character, one of c('all', 'enter', 'open', 'exit', 'close')
#'
#' @rdname printTradeTime
#' @method printTradeTime modelStrategy
#' @export
printTradeTime.modelStrategy <- function(this, type = 'all'){
  switch(type,
         all = {
           ind <- c('open', 'close')
         },
         enter =,
         open = {
           'open'
         },
         exit =,
         close = {
           'close'
         },
         {
           return()
         })
  x <- lapply(ind, function(x){
    print(paste0(x,' trade time:'))
    x <- lapply(this$thisEnv$tradeTime[[x]],function(l){
      print(paste0('start : ',sec_to_tstr(l[[1]]),', end : ',sec_to_tstr(l[[2]])))
    })
  })
}
