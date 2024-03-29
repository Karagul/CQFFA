% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CQF_FA_Functions.R
\name{etlFinData}
\alias{etlFinData}
\title{etlFinData}
\usage{
etlFinData(start.date = as.Date("2019-01-01"),
  end.date = as.Date("2019-05-27"),
  input.tickers.df = data.frame(ticker = c("BA", "AIR.PA"), friendly.name
  = c("BOEING", "AIRBUS")))
}
\arguments{
\item{start.date}{Start Date of the historical price data}

\item{end.date}{End Date of the historical price data}

\item{input.tickers.df}{Data Frame with the products we wish to have the prices, this includes the ticker symbol and a friendly name}
}
\value{
a list with two data frames: cumulated.returns.data.long and cumulated.returns.data.long
}
\description{
etlFinData
}
