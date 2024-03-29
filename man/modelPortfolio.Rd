% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelPortfolio.R
\name{modelPortfolio}
\alias{modelPortfolio}
\title{creates modelPortfolio object}
\usage{
modelPortfolio(..., w = NULL, money_function = NULL, copy = TRUE)
}
\arguments{
\item{...}{list of models / model in each argument}

\item{w}{numeric type, coefficients strategy in portfolio, example: w = c(1,2)}

\item{money_function}{function type or const, three options are possible}

\item{copy}{logical, if TRUE then method will copy all models passed to it

1) money_function = A = const, all strategies will get A money

2) money_function = scalar function, all strategies will get money_function(start_money_in_strategy) money, example: min

3) money_function = vector function, strategies[i] will get money_function(start_money_in_strategy)[i] money, example:
function \{ 200 + start_money_in_strategy\}}
}
\value{
modelPortfolio object
}
\description{
creates modelPortfolio object
}
