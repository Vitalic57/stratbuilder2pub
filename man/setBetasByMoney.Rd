% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genericModelStrategy.R, R/modelStrategy.R
\name{getBetasByMoney}
\alias{getBetasByMoney}
\alias{setBetasByMoney}
\alias{setBetasByMoney.modelStrategy}
\alias{getBetasByMoney.modelStrategy}
\title{Set/Get betasByMoney variable}
\usage{
getBetasByMoney(this)

setBetasByMoney(this, x, price)

\method{setBetasByMoney}{modelStrategy}(this, x, price = quote(data_raw[i - 1,
  ]))

\method{getBetasByMoney}{modelStrategy}(this)
}
\arguments{
\item{this}{modelStrategy}

\item{x}{logical}

\item{price}{quote, price of each intrument in moment i}
}
\description{
This variable is responsible for how spread will be trades. If it is TRUE then coefficients before trade will be update in
 according to current prices in table data_raw. For example, if coefs is c(0.5, -0.5) and prices c(10, 100) and money is 1000,
 then position will be opened with assets amount c(50, -5). But if variable  is FALSE, then assets amount will be c(9, -9).
 If betasByMoney is TRUE, then option from setBetasInt will be ignored.
}
