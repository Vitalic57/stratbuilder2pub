

#' @export
setUserData <- function(this,...){
  UseMethod('setUserData', this)
}

#' @export
setBetaTable <- function(this,...){
  UseMethod('setBetaTable', this)
}

#' @export
applyParamsetServer <- function(this,...){
  UseMethod('applyParamsetServer', this)
}


#' @export
performServer <- function(this,...){
  UseMethod('performServer', this)
}


#' @export
getIgnorePosition <- function(this,...){
  UseMethod('getIgnorePosition', this)
}

#' @export
setIgnorePosition <- function(this,...){
  UseMethod('setIgnorePosition', this)
}


#' @export
getRules <- function(this,...){
  UseMethod('getRules', this)
}


#' @export
setBeta <- function(this,...){
  UseMethod('setBeta', this)
}

#' @export
setMoney <- function(this,...){
  UseMethod('setMoney', this)
}

#' @export
getMoney <- function(this,...){
  UseMethod('getMoney', this)
}

#' @export
setLookback <- function(this,...){
  UseMethod('setLookback', this)
}

#' @export
getLookback <- function(this,...){
  UseMethod('getLookback', this)
}

#' @export
setLookForward <- function(this,...){
  UseMethod('setLookForward', this)
}

#' @export
getLookForward <- function(this,...){
  UseMethod('getLookForward', this)
}

#' @export
setToleranceBeta <- function(this,...){
  UseMethod('setToleranceBeta', this)
}

#' @export
getToleranceBeta <- function(this,...){
  UseMethod('getToleranceBeta', this)
}

#' @export
getMaxLookback <- function(this){
  UseMethod('getMaxLookback', this)
}

#' @export
setMaxLookback <- function(this,...){
  UseMethod('setMaxLookback', this)
}

#' @export
removeRule <- function(this,...){
  UseMethod('removeRule', this)
}

#' @export
removeIndicator <- function(this,...){
  UseMethod('removeIndicator', this)
}

#' @export
getSpreadPrice <- function(this,...){
  UseMethod('getSpreadPrice', this)
}

#' @export
setCommission <- function(this,...){
  UseMethod('setCommission', this)
}

#' @export
getCommission <- function(this,...){
  UseMethod('getCommission', this)
}


#' @export
addDistribution <- function(this,...){
  UseMethod('addDistribution', this)
}

#' @export
addDistributionConstraint <- function(this,...){
  UseMethod('addDistributionConstraint', this)
}

#' @export
deleteParamset <- function(this,...){
  UseMethod('deleteParamset', this)
}

#' @export
getTradingData <- function(this,...){
  UseMethod('getTradingData', this)
}

#' @export
setTradingData <- function(this,...){
  UseMethod('setTradingData', this)
}




#' @export
addTradeTime <- function(this,...){
  UseMethod('addTradeTime', this)
}


#' @export
getTradeTime <- function(this,...){
  UseMethod('getTradeTime', this)
}

#' @export
clearTradeTime <- function(this,...){
  UseMethod('clearTradeTime', this)
}

#' @export
printTradeTime <- function(this,...){
  UseMethod('printTradeTime', this)
}


#' @export
addRule <- function(this,...){
  UseMethod('addRule', this)
}

#' @export
addIndicator <- function(this,...){
  UseMethod('addIndicator', this)
}

#' @export
getIndicators <- function(this,...){
  UseMethod('getIndicators', this)
}

#' @export
getDistributions <- function(this, ...){
  UseMethod('getDistributions', this)
}

#' @export
getPnL <- function(this){
  UseMethod('getPnL', this)
}


#' @export
getVariables <- function(this){
  UseMethod('getVariables', this)
}

#' @export
getProgramParts <- function(this){
  UseMethod('getProgramParts', this)
}


#' @export
getClearWeakBetasOn <- function(this){
  UseMethod('getClearWeakBetasOn', this)
}

#' @export
setClearWeakBetasOn <- function(this, ...){
  UseMethod('setClearWeakBetasOn', this)
}


#' @export
getRemoveLonely <- function(this){
  UseMethod('getRemoveLonely', this)
}

#' @export
setRemoveLonely <- function(this, ...){
  UseMethod('setRemoveLonely', this)
}

#' @export
getPM <- function(this){
  UseMethod('getPM', this)
}

#' @export
addPM <- function(this, ...){
  UseMethod('addPM', this)
}

#' @export
getWaitAfterClose <- function(this){
  UseMethod('getWaitAfterClose', this)
}

#' @export
setWaitAfterClose <- function(this, ...){
  UseMethod('setWaitAfterClose', this)
}

#' @export
getPmAfterOpen <- function(this){
  UseMethod('getPmAfterOpen', this)
}

#' @export
setPmAfterOpen <- function(this, ...){
  UseMethod('setPmAfterOpen', this)
}

#' @export
getParams <- function(this, ...){
  UseMethod('getParams', this)
}

#' @export
setParams <- function(this, ...){
  UseMethod('setParams', this)
}

#' @export
removeVariable <- function(this, ...){
  UseMethod('removeVariable', this)
}

#' @export
addVariable <- function(this, ...){
  UseMethod('addVariable', this)
}

#' @export
getVariablesTable <- function(this, ...){
  UseMethod('getVariablesTable', this)
}

#' @export
addProgramPart <- function(this, ...){
  UseMethod('addProgramPart', this)
}

#' @export
getRulesCall <- function(this, ...){
  UseMethod('getRulesCall', this)
}


#' @export
saveData <- function(this, ...){
  UseMethod('saveData', this)
}

#' @export
getBetasInt <- function(this){
  UseMethod('getBetasInt', this)
}

#' @export
setBetasInt <- function(this, ...){
  UseMethod('setBetasInt', this)
}

#' @export
addStat <- function(this, ...){
  UseMethod('addStat', this)
}

#' @export
removeStat <- function(this, ...){
  UseMethod('removeStat', this)
}

#' @export
reinitStat <- function(this, ...){
  UseMethod('reinitStat', this)
}

#' @export
updateStat <- function(this, ...){
  UseMethod('updateStat', this)
}

#' @export
extractStat <- function(this, ...){
  UseMethod('extractStat', this)
}

#' @export
addData <- function(this, ...){
  UseMethod('addData', this)
}

#' @export
clearData <- function(this, ...){
  UseMethod('clearData', this)
}

#' @export
getReportStrategy <- function(this){
  UseMethod('getReportStrategy', this)
}

#' @export
getReportTrades <- function(this){
  UseMethod('getReportTrades', this)
}

#' @export
getReportCalendar <- function(this){
  UseMethod('getReportCalendar', this)
}

#' @export
plotPnL <- function(this, ...){
  UseMethod('plotPnL', this)
}

#' @export
plotDrawdowns <- function(this, ...){
  UseMethod('plotDrawdowns', this)
}

#' @export
getDateByIndex <- function(this, ...){
  UseMethod('getDateByIndex', this)
}

#' @export
plotCalendar <- function(this, ...){
  UseMethod('plotCalendar', this)
}

#' @export
plotReturns <- function(this, ...){
  UseMethod('plotReturns', this)
}

#' @export
getBacktestResults <- function(this, ...){
  UseMethod('getBacktestResults', this)
}


#' @export
setModelD <- function(this,...){
  UseMethod('setModelD', this)
}

#' @export
getModelD <- function(this,...){
  UseMethod('getModelD', this)
}

#' @export
addObject <- function(this, ...){
  UseMethod('addObject', this)
}

#' @export
submit <- function(this, ...){
  UseMethod('submit', this)
}

#' @export
evaluate <- function(this, ...){
  UseMethod('evaluate', this)
}

#' @export
getPublicName <- function(this){
  UseMethod('getPublicName', this)
}

#' @export
getScores <- function(this, ...){
  UseMethod('getScores', this)
}


