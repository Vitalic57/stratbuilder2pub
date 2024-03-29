% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plotParamset}
\alias{plotParamset}
\alias{plotParamset.data.frame}
\alias{plotParamset.modelStrategy}
\title{Plot paramsets}
\usage{
plotParamset(this, ...)

\method{plotParamset}{data.frame}(df, x = "sharpe.ann",
  y = "return.pos.drawdown", size = NULL, color = "trades.year",
  symbol = NULL, size_scale = 20, omitcols = c("median", "max.loose",
  "max.win", "in.pos.positive", "straight.t"))

\method{plotParamset}{modelStrategy}(this, ...)
}
\arguments{
\item{this}{modelStrategy}

\item{...}{params}

\item{df}{data.frame}

\item{x}{character/expression type, axis x, default "sharpe.ann"}

\item{y}{character/expression type, axis y, default "sortino.ann"}

\item{size}{character/expression type, axis size, default NULL}

\item{color}{character/expression type, axis color, default NULL}

\item{symbol}{character/expression type, axis symbol, default NULL}

\item{size_scale}{numeric type, point size, default 20}

\item{omitcols}{character vector, names of columns that should be omitted}

\item{this}{modelStrategy}

\item{...}{params}
}
\value{
ggplot/xts

plot_ly object

plot_ly object
}
\description{
Plot paramsets

Draws 5-D graph with axis x,y,size,color,symbol, which are contained in data.frame

Plot backtests results in 5-D graph
}
