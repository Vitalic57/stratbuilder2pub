#' Set list of parameters
#' 
#' Here you can define parameters for rules, indicators, program part and managers of position
#'
#' @param type character, it can be equal to rules, indicators, pp or pm 
#' @param args list, define your params here
#' @param this modelStrategy
#'
#' @export
#' @examples
#' setParams(this,
#'        type = 'rules',
#'        args = list(n = 5, a = 2)
#' )
setParams.modelStrategy <- function(this,
                                    type,
                                    args){
  type <- switch (tolower(type),
                  rule = ,
                  rules = 'rules',
                  indicator =,
                  indicators = 'indicators',
                  pm = ,
                  pms = 'pms',
                  pp = ,
                  pps = ,
                  programparts =,
                  programpart =,
                  program = 'pps'
  )
  this$thisEnv$params[[type]] <- args
}


#' Get list of parameters according to type.
#'
#' @param type character, it can be equal to rules, indicators, pp or pm 
#' @param this modelStrategy
#'
#' @export
getParams.modelStrategy <- function(this,
                                    type){
  type <- switch (tolower(type),
                  rule = ,
                  rules = 'rules',
                  indicator =,
                  indicators = 'indicators',
                  pm = ,
                  pms = 'pms',
                  pps =,
                  pp = ,
                  programparts =,
                  programpart =,
                  program = 'pps'
  )
  return(this$thisEnv$params[[type]])
}
