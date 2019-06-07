# In this file are all functions that are required for the CQF final project
# Author: Fabian Arter
# Date: 2019-05-29


###############################################################

#' etlFinData
#'
#' @param start.date Start Date of the historical price data
#' @param end.date  End Date of the historical price data
#' @param input.tickers.df Data Frame with the products we wish to have the prices, this includes the ticker symbol and a friendly name
#' @return a list with two data frames: cumulated.returns.data.long and cumulated.returns.data.long
#' @export
etlFinData <- function(start.date=as.Date("2019-01-01"),
                       end.date=as.Date("2019-05-27"),
                       input.tickers.df = data.frame(ticker=c("BA","AIR.PA"),
                                                     friendly.name=c("BOEING","AIRBUS"))
) {

  # load data via BatchGetSymbols
  daily.returns.data.long <- BatchGetSymbols::BatchGetSymbols(
    tickers      = as.character(input.tickers.df$ticker),
    first.date   = start.date,
    last.date    = end.date,
    freq.data    = "daily",
    type.return  = "log",
    cache.folder = file.path(tempdir(), 'BGS_Cache') )$df.tickers


  # add friendly names
  daily.returns.data.long <-  merge(daily.returns.data.long, input.tickers.df, by="ticker")

  daily.returns.data.long[is.na(daily.returns.data.long$ret.adjusted.prices),]$ret.adjusted.prices <- 0
  daily.returns.data.long <- na.omit(daily.returns.data.long)

  daily.returns.data.long <- data.frame(ref.date        = daily.returns.data.long$ref.date,
                                        friendly.name   = as.character(daily.returns.data.long$friendly.name),
                                        price.adjusted  = daily.returns.data.long$price.adjusted,
                                        daily.return    = daily.returns.data.long$ret.adjusted.prices)

  daily.returns.data.wide <- reshape2::dcast(daily.returns.data.long,  ref.date ~ friendly.name, value.var = "daily.return")
  daily.returns.data.wide <- na.omit(daily.returns.data.wide)

  # cumulated returns wide
  cumulated.returns.data.wide  <- as.data.frame(cbind(ref.date = daily.returns.data.wide$ref.date, apply(daily.returns.data.wide[,2:ncol(daily.returns.data.wide)], 2, cumsum)))

  # cumulated returns long
  cumulated.returns.data.long               <- reshape2::melt(cumulated.returns.data.wide, id.vars=1, measure.vars = 2:ncol(cumulated.returns.data.wide))
  cumulated.returns.data.long$ref.date      <- as.Date(cumulated.returns.data.long$ref.date, origin = "1970-01-01")
  cumulated.returns.data.long$name          <- as.character(cumulated.returns.data.long$variable) ; cumulated.returns.data.long$variable <- NULL
  cumulated.returns.data.long$cumul.return  <- cumulated.returns.data.long$value ; cumulated.returns.data.long$value <- NULL

  return(list(daily.returns.data.long     = daily.returns.data.long,
              daily.returns.data.wide     = daily.returns.data.wide,
              cumulated.returns.data.long = cumulated.returns.data.long))

}

#' SingleTitlestats
#'
#' @description This function creates Single Title stats
#' @param daily.returns.data.wide  data.frame including the daily returns of the specific assets as well as a reference date
#' @param num.trade.days.per.year Number of trading days per year, default set to 250
#' @return a list with a data.frame PF.return.result.table that includes all statistics, as well as two graphs - histogram with returns and a correlation matrix
#' @export
SingleTitlestats <- function(daily.returns.data.wide, num.trade.days.per.year=250) {

  single.title.stats <-    data.frame(mean.return     = colMeans(daily.returns.data.wide[,2:ncol(daily.returns.data.wide)]*num.trade.days.per.year),
                                      mean.return.day = colMeans(daily.returns.data.wide[,2:ncol(daily.returns.data.wide)]),
                                      sd.day          = apply(daily.returns.data.wide[,2:ncol(daily.returns.data.wide)], 2, sd),
                                      sd.year         = apply(daily.returns.data.wide[,2:ncol(daily.returns.data.wide)], 2, sd)*num.trade.days.per.year^0.5
                                      )

  return.matrix        <- data.matrix(daily.returns.data.wide[2:ncol(daily.returns.data.wide)], rownames.force=TRUE)

  sample.correl.matrix <- cor(return.matrix)
  sample.covar.matrix  <- cov(return.matrix)
  shrink.correl.matrix <- as.matrix(corpcor::cor.shrink(return.matrix))
  shrink.covar.matrix  <- as.matrix(corpcor::cov.shrink(return.matrix))

  S          <- diag(single.title.stats$sd.day)
  R          <- sample.correl.matrix
  SRS        <- S %*% R %*% S # same as covar matrix

return(list(single.title.stats   = single.title.stats,
            sample.correl.matrix = sample.correl.matrix,
            sample.covar.matrix  = sample.covar.matrix,
            shrink.correl.matrix = shrink.correl.matrix,
            shrink.covar.matrix  = shrink.covar.matrix))
  }




#' PFstats
#'
#' @description This function creates all important descriptive statistics such as VaR, ES, Return for a portfolio
#' @param weights.vector vector that contains the relative weights of the individual assets of the portfolio
#' @param daily.returns.data.wide  data.frame including the daily returns of the specific assets as well as a reference date
#' @param num.trade.days.per.year Number of trading days per year, default set to 250
#' @return a list with a data.frame PF.return.result.table that includes all statistics, as well as two graphs - histogram with returns and a correlation matrix
#' @examples
#' weights.vector          <- c(0.7,0.3)
#' daily.returns.data.wide <- data.frame(ref.date=c(Sys.Date()-2:0), asset1.ret=c(-0.02,0.005,0.004), asset2.ret=c(0,-0.001,0.02))
#' PFstats(weights.vector=weights.vector, daily.returns.data.wide=daily.returns.data.wide)
#' @export
PFstats <- function(weights.vector, daily.returns.data.wide, num.trade.days.per.year=250) {

  daily.returns.data.wide <- daily.returns.data.wide[order(daily.returns.data.wide$ref.date),]


  ## PORTFOLIO STATS
  PF.daily.return <- rep(NA,nrow(daily.returns.data.wide))

  for(i in 1:nrow(daily.returns.data.wide)) {
    PF.daily.return[i]  <-weights.vector %*% as.numeric(daily.returns.data.wide[i,2:ncol(daily.returns.data.wide)])
  }

  PF.cumul.return.start.to.date <- cumsum(PF.daily.return)


  PF.return.result.table <- data.frame(start.date      = min(daily.returns.data.wide$ref.date),
                                       end.date        = max(daily.returns.data.wide$ref.date),
                                       N.days          = length(PF.daily.return),
                                       PF.total.return = tail(PF.cumul.return.start.to.date,1),
                                       PF.min.start.to.date.return   = min(PF.cumul.return.start.to.date),
                                       PF.min.daily.return  = min(PF.daily.return),
                                       PF.mean.daily.return = mean(PF.daily.return),
                                       PF.annualized.return = mean(PF.daily.return)*num.trade.days.per.year,
                                       PF.sd.daily.return   = sd(PF.daily.return), # same as as.numeric(sqrt(t(weights.vector) %*% covar.matrix %*% weights.vector))
                                       PF.annualized.vola   = sd(PF.daily.return)*sqrt(num.trade.days.per.year),
                                       PF.hist.1dVaR95 = as.numeric(quantile(PF.daily.return,0.05)),
                                       PF.hist.1dES95  = mean(head(sort(PF.daily.return),n=floor(0.05*length(PF.daily.return)))),
                                       PF.sharpe.ratio = (mean(PF.daily.return) / sd(PF.daily.return))*sqrt(num.trade.days.per.year),
                                       PF.sortino.ratio = (mean(PF.daily.return) / sd(PF.daily.return[PF.daily.return<0]))*sqrt(num.trade.days.per.year)
  )


  # PLOTS

  #### HISTOGRAM
  plot.hist.daily.return <- ggplot2::ggplot(data=data.frame(PF.daily.return), aes(PF.daily.return))
  plot.hist.daily.return <- plot.hist.daily.return + geom_histogram(aes(y=..density..), color="black", fill = "darkblue",binwidth = 0.002, alpha = 0.2) + xlim(-0.05,0.05)
  plot.hist.daily.return <- plot.hist.daily.return + stat_function(fun = dnorm, colour = "red", args = list(mean = mean(PF.daily.return), sd = sd(PF.daily.return)))
  plot.hist.daily.return <- plot.hist.daily.return + labs(title = paste0("Portfolio daily returns"," - PF ann. vola: ",round(PF.return.result.table$PF.annualized.vola*100,3),"%"), subtitle = paste(PF.return.result.table$start.date, " - ",PF.return.result.table$end.date))



  #### CORREL MATRIX
  return.matrix       <- data.matrix(daily.returns.data.wide[2:ncol(daily.returns.data.wide)], rownames.force=TRUE)
  cor.mat             <- cor(return.matrix)

  plot.correl.matrix <- GGally::ggcorr(return.matrix,  palette='RdGy',digits=3, label=TRUE, label_round = 2, label_size=3, size = 2.2)
  plot.correl.matrix <- plot.correl.matrix + labs(title = "Correlation Matrix", subtitle = paste(min(daily.returns.data.wide$ref.date), " - ", max(daily.returns.data.wide$ref.date)))



  return(list(PF.return.result.table = PF.return.result.table,

              plot.hist.daily.return = plot.hist.daily.return,
              plot.correl.matrix     = plot.correl.matrix))
}



#########################################################################
# PORTFOLIO OPTIMIZATIONS #
#########################################################################

#' meanVariancePortfolioOptimizer
#'
#' @description MPT mean Variance Portfolio Optimizer
#' @param asset.name Name of assets available
#' @param mu.vector vector with estimated returns of investment objects
#' @param sigma.vector vector with the volatilities of the investment objects
#' @param correl.matrix correlation matrix of the investment objects
#' @param target.return which return level is seeked (for which the variance is minimized)
#' @param rf risk free return
#' @return weight.risky.assets a vector with the weights of the risky assets
#' @examples
#' weights.vector          <- c(0.7,0.3)
#' daily.returns.data.wide <- data.frame(ref.date=c(Sys.Date()-2:0), asset1.ret=c(-0.02,0.005,0.004), asset2.ret=c(0,-0.001,0.02))
#' PFstats(weights.vector=weights.vector, daily.returns.data.wide=daily.returns.data.wide)
#' @export
meanVariancePortfolioOptimizer <- function(asset.name, mu.vector, sigma.vector, correl.matrix, target.return, rf, print.out=FALSE, opt.focus.type="return") {

  one.vector <- rep(1,length(mu.vector))
  S          <- diag(sigma.vector)
  R          <- correl.matrix
  SRS        <- S %*% R %*% S

  if(opt.focus.type=="return") {

  if(rf!=0) {
    optimal.weights = ((target.return-rf) * solve(SRS) %*% t(mu.vector-rf%*% one.vector)) /
      as.numeric(((mu.vector-rf%*%one.vector) %*% solve(SRS) %*% t(mu.vector-rf %*% one.vector)))
    optimal.weights.df <- data.frame(asset=1:length(mu.vector), optimal.weight=optimal.weights)
    if(print.out==TRUE) {print(optimal.weights.df)}
  } else {
    A = as.numeric(t(one.vector) %*% solve(SRS) %*% one.vector)
    B = as.numeric(t(mu.vector) %*% solve(SRS) %*% one.vector)
    C = as.numeric(t(mu.vector) %*% solve(SRS) %*% mu.vector)

    lambda = (A*target.return-B)/(A*C-B^2)
    gamma  = (C-B*target.return)/(A*C-B^2)

    optimal.weights <- solve(SRS) %*% (lambda * mu.vector + gamma * one.vector)
    optimal.weights.df <- data.frame(asset=length(mu.vector), optimal.weight=optimal.weights)
    if(print.out==TRUE) { print(optimal.weights.df) }
  }


  weight.risky.assets <- sum(optimal.weights)
  if(print.out==TRUE) {print(paste("Total Weight Risky Assets:",round(weight.risky.assets,4)))}

  if(rf!=0) {
    if(print.out==TRUE) {  print(paste("Weight Risk-Free Asset:",round(1-weight.risky.assets,4)))}
    pf.return <- t(optimal.weights) %*% mu.vector + (1-weight.risky.assets)*rf
  } else {

    pf.return <- t(optimal.weights) %*% mu.vector
  }

  if(print.out==TRUE) {print(paste("PF return:", round(pf.return,4)))}
  pf.vola <- as.numeric(sqrt(t(c(optimal.weights)) %*% SRS %*% c(optimal.weights)))
  if(print.out==TRUE) {print(paste("PF vola:", round(pf.vola,4)))}
  }


  if(opt.focus.type=="min.var") {
    optimal.weights <- solve(SRS) %*% one.vector / as.numeric(t(one.vector) %*% solve(SRS) %*% one.vector)
    weight.risky.assets <- sum(optimal.weights)
    pf.vola <- as.numeric(sqrt(t(c(optimal.weights)) %*% SRS %*% c(optimal.weights)))
    pf.return <- NA
  }

  result.table <- data.frame(asset.name      = c("risk free",asset.name),
                             optimal.weights = c(round(1-weight.risky.assets,4), optimal.weights),
                             pf.return       = pf.return,
                             pf.vola         = pf.vola,
                             rf.free         = rf)

  return(result.table = result.table )
}




#' histCVaRcalc
#'
#' @description calculates the historical Conditional VaR / Expected Shortfall of a portfolio or an asset
#' @param daily.returns Daily historical returns of a portfolio or single asset
#' @param alpha Alpha of the CVaR, this is the confidence level from which on the average of the tail risk is being calculated
#' @return cvar CVaR of the specific portfolio or asset with the set alpha
#' @export
histCVaRcalc = function(daily.returns, alpha){

  i.num = alpha * length(portfolio.returns)

  portfolio.returns <- sort(portfolio.returns)

  cvar = mean(portfolio.returns[1:i.num])

  return(cvar)
}




#' CVaRPortfolioOptimizer
#'
#' @description CVaR Portfolio Optimizer
#' @param asset.names Vector with names of the single assets in the portfolio to be optimized
#' @param daily.returns.data.wide data.frame including the daily returns of the specific assets as well as a reference date
#' @param alpha Alpha of the CVaR, this is the confidence level from which on the average of the tail risk is being calculated
#' @return cvar CVaR of the specific portfolio or asset with the set alpha
#' @export
CVaRPortfolioOptimizer <- function(asset.names, daily.returns.data.wide, alpha) {

  asset.weights <- rep(1/length(asset.names), length(asset.names))

  #linear equality constraint
  #note: nloptr requires all functions to have the same signature
  eval_g0 <- function(asset.weights, ddaily.returns.data.wide=NA, alpha=NA) {
    return( sum(asset.weights) - 1 )
  }

  #numerical approximation of the gradient
  des = function(asset.weights, daily.returns.data.wide=NA, alpha=NA){
    n = length(asset.weights)
    out = asset.weights;
    for (i in 0:n){
      up = asset.weights;
      dn = asset.weights;
      up[i] = up[i]+.0001
      dn[i] = dn[i]-.0001
      out[i] = (histCVaRcalc(up,daily.returns.data.wide=daily.returns.data.wide,alpha=alpha) - histCVaRcalc(dn,daily.returns.data.wide=daily.returns.data.wide,alpha=alpha))/.0002
    }
    return(out)
  }

  #use nloptr to check out gradient
  #check.derivatives(w,es,des,sim=sim, alpha=.05)

  #function to optimize ??? a list of objective and gradient
  toOpt = function(asset.weights, daily.returns.data.wide=NA, alpha=NA){
    list(objective=histCVaRcalc(asset.weights, daily.returns.data.wide,alpha), gradient=des(asset.weights, daily.returns.data.wide,alpha=alpha))
  }

  #equality constraint function.  The jacobian is 1 for all variables
  eqCon = function(asset.weights,daily.returns.data.wide=NA,alpha=NA){
    list(constraints=eval_g0(asset.weights,daily.returns.data.wide=NA,alpha=alpha),jacobian=rep(1,length(asset.weights)))
  }
  #optimization options
  opts <- list( "algorithm" = "NLOPT_LD_SLSQP",
                "xtol_rel" = 1.0e-7,
                "maxeval" = 10000)
  #run optimization and print results
  nl = nloptr(asset.weights,toOpt,
              lb = rep(0,length(asset.weights)),
              ub = rep(1,length(asset.weights)),
              eval_g_eq=eqCon,
              opts=opts,
              daily.returns.data.wide=daily.returns.data.wide,alpha=alpha)

  s = nl$solution
  obj = nl$objective

  return(s)
}




#' generateOptimizationResultStats
#'
#' @description generate Optimization Result Stats
#' @param out.of.sample.period.months how many months are considered for the out of sample period
#' @param investment.period.months number of months of the investment horizon before the next rebalancing happens
#' @param daily.returns.data.wide data frame with returns with out of sample return data as well as data for the actual investment period
#' @param pf.opt.type type of optimization, min.var,
#' @param covar.type either sample of
#' @param correl.type either sample of
#' @param vola.type sample
#' @param exp.return expected return of the assets, default is sample - meaning that we take historical asset return as a best predictor
#' @param target.return default is NA
#' @param
#' @return list with result data.frames: weights.result.table,
#' @export
generateOptimizationResultStats <- function(out.of.sample.period.months = 24,
                                            investment.period.months    = 12,
                                            daily.returns.data.wide,
                                            pf.opt.type   = "mpt.min.var",
                                            covar.type    = "sample",
                                            correl.type     = "sample",
                                            vola.type     = "sample",
                                            exp.return    = "sample",
                                            target.return = NA) {

first.out.of.sample.date <- lubridate::floor_date(min(daily.returns.data.wide$ref.date),"month")
last.out.of.sample.date  <- lubridate::ceiling_date(max(daily.returns.data.wide$ref.date)  %m-% months(out.of.sample.period.months),"month")

first.investment.date    <- lubridate::floor_date(min(daily.returns.data.wide$ref.date)  %m+% months(out.of.sample.period.months),"month")
last.investment.date     <- lubridate::ceiling_date(max(daily.returns.data.wide$ref.date),"month")


N.pf.rebalances         <- (length(seq(from=first.investment.date, to=last.investment.date, by="months"))-1)/investment.period.months

portfolio.results.list     <- vector("list", N.pf.rebalances)
weights.result.table.list  <- vector("list", N.pf.rebalances)

for(i in 1:N.pf.rebalances) {
  print(paste("Rebalancing Portfolio - RUN:",i))

  # 1) GENERATE OUT OF SAMPLE DATA
  out.of.sample.start.date <- lubridate::floor_date(first.out.of.sample.date %m+% months(investment.period.months*(i-1)),"month")
  out.of.sample.end.date   <- out.of.sample.start.date %m+% months(out.of.sample.period.months)-1

  print(paste("Out of Sample Period:",out.of.sample.start.date,"-",out.of.sample.end.date))

  # load out of sample data
  daily.returns.data.wide.out.of.sample <- daily.returns.data.wide[daily.returns.data.wide$ref.date >= out.of.sample.start.date &
                                                                   daily.returns.data.wide$ref.date  <= out.of.sample.end.date,]

  single.title.stats.list  <- SingleTitlestats(daily.returns.data.wide = daily.returns.data.wide.out.of.sample)

  asset.name     <- as.character(rownames(single.title.stats.list$single.title.stats))

  sigma.vector   <- if(vola.type    == "sample") {
                      single.title.stats.list$single.title.stats$sd.day}

  correl.matrix  <- if(correl.type    == "sample") {
                       single.title.stats.list$sample.correl.matrix}

  covar.matrix  <- if(covar.type    == "sample") {
                      single.title.stats.list$sample.covar.matrix}


  mu.vector     <- if(exp.return    == "sample") {
                      single.title.stats.list$single.title.stats$mean.return.day
                     }


  ## 2) RUN OPTIMIZATION AND GET WEIGHTS

                    # 1/N
  weights.vector <- if(pf.opt.type=="1/N") {
                      print(paste("OPTIMIZATION TYPE:",pf.opt.type))
                      weights.vector <- rep(1/length(asset.name),length(asset.name))
                      print(weights.vector)
                      # mpt.min.var
                    } else if(pf.opt.type=="mpt.min.var") {
                      print(paste("OPTIMIZATION TYPE:",pf.opt.type))
                      weights.vector <- meanVariancePortfolioOptimizer(asset.name, mu.vector, sigma.vector, correl.matrix, target.return, rf=0, opt.focus.type="min.var")$optimal.weights
                      weights.vector <- weights.vector[2:length(weights.vector)] # first value is for risk free, which is here ignored for the min variance pf
                     # mpt.tar.ret
                     } else if(pf.opt.type=="mpt.tar.ret") {
                       print(paste("OPTIMIZATION TYPE:",pf.opt.type))
                       weights.vector <- meanVariancePortfolioOptimizer(asset.name, mu.vector, sigma.vector, correl.matrix, target.return, rf=0, opt.focus.type="return")$optimal.weights
                       weights.vector <- weights.vector[2:length(weights.vector)] # first value is for risk free, which is here ignored for the min variance pf
                     }

  weights.result.table <- data.frame(start.out.of.sample = min(daily.returns.data.wide.out.of.sample$ref.date),
                                     end.out.of.sample   = max(daily.returns.data.wide.out.of.sample$ref.date),
                                     asset.name          = asset.name,
                                     weights             = weights.vector)


  weights.result.table.list[[i]] <- weights.result.table

  # 3) GENERATE DATA FOR INVESTMENT PERIOD

  investment.period.start.date <- lubridate::floor_date(first.investment.date %m+% months(investment.period.months*(i-1)),"month")
  investment.period.end.date   <- investment.period.start.date %m+% months(investment.period.months)-1

  daily.returns.data.wide.investment.period <- daily.returns.data.wide[daily.returns.data.wide$ref.date  >= investment.period.start.date &
                                                                       daily.returns.data.wide$ref.date  <= investment.period.end.date,]

  # 4) GENERATE PORTFOLIO RESULTS FOR INVESTMENT PERIOD
   pf.result <- PFstats(weights.vector          = weights.vector,
                       daily.returns.data.wide = daily.returns.data.wide.investment.period,
                       num.trade.days.per.year = 250)
  portfolio.results.list[[i]] <- pf.result$PF.return.result.table
}

portfolio.results.table <- dplyr::bind_rows(portfolio.results.list)
weights.result.table    <- dplyr::bind_rows(weights.result.table.list)

portfolio.results.table <- data.frame(        Year    = as.character(lubridate::year(portfolio.results.table$start.date)),
                                              Return  = portfolio.results.table$PF.total.return,
                                              Lowest  = portfolio.results.table$PF.min.start.to.date.return,
                                              Vola    = portfolio.results.table$PF.annualized.vola,
                                              SR      = portfolio.results.table$PF.sharpe.ratio,
                                              VaR     = portfolio.results.table$PF.hist.1dVaR95,
                                              CVaR    = portfolio.results.table$PF.hist.1dES95,
                                              Sortino = portfolio.results.table$PF.sortino.ratio
)


results.table.avg <- cbind(data.frame(Year="Average"),as.data.frame.list(colMeans(portfolio.results.table[,2:ncol(portfolio.results.table)])))

results.table.x <- rbind(portfolio.results.table, results.table.avg)

return(list(portfolio.results.table = portfolio.results.table,
            weights.result.table    = weights.result.table,
            results.table.x         = results.table.x))
}
