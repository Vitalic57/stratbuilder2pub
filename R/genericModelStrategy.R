

#' @export
setUserData <- function(model,...){
  UseMethod('setUserData', model)
}

#' @export
setBetaTable <- function(model,...){
  UseMethod('setBetaTable', model)
}

#' @export
applyParamsetServer <- function(model,...){
  UseMethod('applyParamsetServer', model)
}


#' @export
performServer <- function(model,...){
  UseMethod('performServer', model)
}


#' @export
getIgnorePosition <- function(model,...){
  UseMethod('getIgnorePosition', model)
}

#' @export
setIgnorePosition <- function(model,...){
  UseMethod('setIgnorePosition', model)
}


#' @export
getRules <- function(model,...){
  UseMethod('getRules', model)
}


#' @export
setBeta <- function(model,...){
  UseMethod('setBeta', model)
}

#' @export
setMoney <- function(model,...){
  UseMethod('setMoney', model)
}

#' @export
getMoney <- function(model,...){
  UseMethod('getMoney', model)
}

#' @export
setLookback <- function(model,...){
  UseMethod('setLookback', model)
}

#' @export
getLookback <- function(model,...){
  UseMethod('getLookback', model)
}

#' @export
setLookForward <- function(model,...){
  UseMethod('setLookForward', model)
}

#' @export
getLookForward <- function(model,...){
  UseMethod('getLookForward', model)
}

#' @export
setToleranceBeta <- function(model,...){
  UseMethod('setToleranceBeta', model)
}

#' @export
getToleranceBeta <- function(model,...){
  UseMethod('getToleranceBeta', model)
}

#' @export
getMaxLookback <- function(model){
  UseMethod('getMaxLookback', model)
}

#' @export
setMaxLookback <- function(model,...){
  UseMethod('setMaxLookback', model)
}

#' @export
removeRule <- function(model,...){
  UseMethod('removeRule', model)
}

#' @export
removeIndicator <- function(model,...){
  UseMethod('removeIndicator', model)
}

#' @export
getSpreadPrice <- function(model,...){
  UseMethod('getSpreadPrice', model)
}

#' @export
setCommission <- function(model,...){
  UseMethod('setCommission', model)
}

#' @export
getCommission <- function(model,...){
  UseMethod('getCommission', model)
}


#' @export
addDistribution <- function(model,...){
  UseMethod('addDistribution', model)
}

#' @export
addDistributionConstraint <- function(model,...){
  UseMethod('addDistributionConstraint', model)
}

#' @export
deleteParamset <- function(model,...){
  UseMethod('deleteParamset', model)
}

#' @export
getTradingData <- function(model,...){
  UseMethod('getTradingData', model)
}

#' @export
setTradingData <- function(model,...){
  UseMethod('setTradingData', model)
}




#' @export
addTradeTime <- function(model,...){
  UseMethod('addTradeTime', model)
}


#' @export
getTradeTime <- function(model,...){
  UseMethod('getTradeTime', model)
}

#' @export
clearTradeTime <- function(model,...){
  UseMethod('clearTradeTime', model)
}

#' @export
printTradeTime <- function(model,...){
  UseMethod('printTradeTime', model)
}


#' @export
addRule <- function(model,...){
  UseMethod('addRule', model)
}

#' @export
addIndicator <- function(model,...){
  UseMethod('addIndicator', model)
}

#' @export
getIndicators <- function(model,...){
  UseMethod('getIndicators', model)
}

#' @export
getDistributions <- function(model, ...){
  UseMethod('getDistributions', model)
}




#' @export
getVariables <- function(model){
  UseMethod('getVariables', model)
}

#' @export
getProgramParts <- function(model){
  UseMethod('getProgramParts', model)
}


#' @export
getClearWeakBetasOn <- function(model){
  UseMethod('getClearWeakBetasOn', model)
}

#' @export
setClearWeakBetasOn <- function(model, ...){
  UseMethod('setClearWeakBetasOn', model)
}


#' @export
getRemoveLonely <- function(model){
  UseMethod('getRemoveLonely', model)
}

#' @export
setRemoveLonely <- function(model, ...){
  UseMethod('setRemoveLonely', model)
}

#' @export
getPM <- function(model){
  UseMethod('getPM', model)
}

#' @export
addPM <- function(model, ...){
  UseMethod('addPM', model)
}

#' @export
getWaitAfterClose <- function(model){
  UseMethod('getWaitAfterClose', model)
}

#' @export
setWaitAfterClose <- function(model, ...){
  UseMethod('setWaitAfterClose', model)
}

#' @export
getPmAfterOpen <- function(model){
  UseMethod('getPmAfterOpen', model)
}

#' @export
setPmAfterOpen <- function(model, ...){
  UseMethod('setPmAfterOpen', model)
}

#' @export
getParams <- function(model, ...){
  UseMethod('getParams', model)
}

#' @export
setParams <- function(model, ...){
  UseMethod('setParams', model)
}

#' @export
removeVariable <- function(model, ...){
  UseMethod('removeVariable', model)
}

#' @export
addVariable <- function(model, ...){
  UseMethod('addVariable', model)
}

#' @export
getVariablesTable <- function(model, ...){
  UseMethod('getVariablesTable', model)
}

#' @export
addProgramPart <- function(model, ...){
  UseMethod('addProgramPart', model)
}

#' @export
getRulesCall <- function(model, ...){
  UseMethod('getRulesCall', model)
}


#' @export
saveData <- function(model, ...){
  UseMethod('saveData', model)
}

#' @export
getBetasInt <- function(model){
  UseMethod('getBetasInt', model)
}

#' @export
setBetasInt <- function(model, ...){
  UseMethod('setBetasInt', model)
}

#' @export
addStat <- function(model, ...){
  UseMethod('addStat', model)
}

#' @export
removeStat <- function(model, ...){
  UseMethod('removeStat', model)
}

#' @export
reinitStat <- function(model, ...){
  UseMethod('reinitStat', model)
}

#' @export
updateStat <- function(model, ...){
  UseMethod('updateStat', model)
}

#' @export
extractStat <- function(model, ...){
  UseMethod('extractStat', model)
}

#' @export
addData <- function(model, ...){
  UseMethod('addData', model)
}

#' @export
clearData <- function(model, ...){
  UseMethod('clearData', model)
}


