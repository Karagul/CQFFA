% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CQF_FA_Functions.R
\name{CVaRPortfolioOptimizer}
\alias{CVaRPortfolioOptimizer}
\title{CVaRPortfolioOptimizer}
\usage{
CVaRPortfolioOptimizer(asset.names, daily.returns.data.wide, alpha)
}
\arguments{
\item{asset.names}{Vector with names of the single assets in the portfolio to be optimized}

\item{daily.returns.data.wide}{data.frame including the daily returns of the specific assets as well as a reference date}

\item{alpha}{Alpha of the CVaR, this is the confidence level from which on the average of the tail risk is being calculated}
}
\value{
cvar CVaR of the specific portfolio or asset with the set alpha
}
\description{
CVaR Portfolio Optimizer
}