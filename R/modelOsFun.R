#' Function for volume in rule
#' 
#' How many sum(price \* abs(beta)) in money \* proportion
#'
#' @param beta numeric vector, coefficients of spread
#' @param money  numeric, all money that strategy has
#' @param proportion numeric, number between 0 and 1, how much money we can use in rule
#' @param price current price of instruments
#' @param ... additional args
#' 
#' @return numeric multiplier( how much minimal position to buy)
#' @export
proportionOs <- function(beta, money, proportion, price, ...){
  money_proportion <- money * proportion
  price_position <- sum(price * abs(beta))
  return(floor(money_proportion/price_position))
}

#' Function for volume in rule
#' 
#' How many sum(price * abs(beta)) in amount
#'
#' @param beta numeric, number of assets
#' @param amount numeric, amount of money
#' @param price numeric, vector of prices
#' @param ... args
#'
#' @return numeric
#' @export
sameMoneyOs <- function(beta, amount, price, ...){
  price_position <- sum(price * abs(beta))
  return(floor(amount/price_position))
}

