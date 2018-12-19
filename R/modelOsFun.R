# wheights functions
#'beta -- coefficients in spread
#'money -- all money that strategy has
#'proportion -- how much money we can use in rule
#'price -- current price of instruments( how much it costs to buy or sell)
#'it returns multiplier( how much minimal position )
proportionOs <- function(beta, money, proportion, price, ...){
  # if(missing(proportion)){
  #   proportion <- 1
  #   print('proportion is 1')
  # }
  #print(proportion)
  money_proportion <- money * proportion
  price_position <- sum(price * abs(beta))
  return(floor(money_proportion/price_position))
}

#amount - how much money we can use in rule
sameMoneyOs <- function(beta, amount, price, ...){
  money_proportion <- amount
  price_position <- sum(price * abs(beta))
  return(floor(money_proportion/price_position))
}

