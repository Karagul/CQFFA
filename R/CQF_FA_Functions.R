# In this file are all functions that are required for the CQF final project
# Author: Fabian Arter
# Date: 2019-05-29

# generate PDF manual
# system("R CMD Rd2pdf /Users/fabianarter/Library/Mobile Documents/com~apple~CloudDocs/Education/CQF/Final_Project/CQFFA")

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
  cumulated.returns.data.wide  <- if(nrow(input.tickers.df) !=1) {
    as.data.frame(cbind(ref.date = daily.returns.data.wide$ref.date, apply(daily.returns.data.wide[,2:ncol(daily.returns.data.wide)], 2, cumsum)))

    }


  # cumulated returns long
  cumulated.returns.data.long <- if(nrow(input.tickers.df) !=1) {
  cumulated.returns.data.long               <- reshape2::melt(cumulated.returns.data.wide, id.vars=1, measure.vars = 2:ncol(cumulated.returns.data.wide))
  cumulated.returns.data.long$ref.date      <- as.Date(cumulated.returns.data.long$ref.date, origin = "1970-01-01")
  cumulated.returns.data.long$name          <- as.character(cumulated.returns.data.long$variable) ; cumulated.returns.data.long$variable <- NULL
  cumulated.returns.data.long$cumul.return  <- cumulated.returns.data.long$value ; cumulated.returns.data.long$value <- NULL
  cumulated.returns.data.long
  } else {
    data.frame(ref.date = daily.returns.data.long$ref.date, name = daily.returns.data.long$friendly.name, cumul.return=cumsum(daily.returns.data.long$daily.return))
  }

  cumulated.returns.data.wide$ref.date      <- as.Date(as.numeric(cumulated.returns.data.wide$ref.date), origin = "1970-01-01")

  return(list(daily.returns.data.long     = daily.returns.data.long,
              daily.returns.data.wide     = daily.returns.data.wide,
              cumulated.returns.data.long = cumulated.returns.data.long,
              cumulated.returns.data.wide = cumulated.returns.data.wide))

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
PFstats <- function(weights.vector,
                    daily.returns.data.wide,
                    num.trade.days.per.year=250,
                    long.term.mean.return=0,
                    long.term.sd.return=0.01) {

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
  plot.hist.daily.return <- ggplot2::ggplot(data=data.frame(PF.daily.return), ggplot2::aes(PF.daily.return))
  plot.hist.daily.return <- plot.hist.daily.return + geom_histogram(aes(y=..density..), color="black", fill = "steelblue",binwidth = 0.003, alpha = 0.2) + xlim(-0.06,0.06)  + ylim(0,120)
  plot.hist.daily.return <- plot.hist.daily.return + geom_density()
  plot.hist.daily.return <- plot.hist.daily.return + stat_function(fun = dnorm, colour = "red", args = list(mean = long.term.mean.return, sd = long.term.sd.return))
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



#' histCVaRcalc
#'
#' @description calculates the historical Conditional VaR / Expected Shortfall of a portfolio or an asset
#' @param daily.returns Daily historical returns of a portfolio or single asset
#' @param alpha.cvar Alpha of the CVaR, this is the confidence level from which on the average of the tail risk is being calculated
#' @return cvar CVaR of the specific portfolio or asset with the set alpha
#' @export
histCVaRcalc = function(asset.weights=1, daily.returns.data.wide, alpha.cvar){

  i.num = ceiling(alpha.cvar * nrow(daily.returns.data.wide))

  portfolio.returns <- as.matrix(daily.returns.data.wide[,2:length(daily.returns.data.wide)]) %*% c(asset.weights)

  portfolio.returns <- sort(portfolio.returns)

  cvar = mean(portfolio.returns[1:i.num])

  return(cvar)
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
#' @param alpha.cvar Alpha parameter for CVaR optimization only, default is NA
#' @param in.sample default is FALSE
#' @param sum.weight default is 1
#' @param min.single.weight default is -100
#' @param max.single.weight default is +100
#'
#' @return list with result data.frames: weights.result.table,
#' @export
generateOptimizationResultStats <- function(out.of.sample.period.months = 24,
                                            investment.period.months    = 12,
                                            daily.returns.data.wide,
                                            pf.opt.type                 = "mpt.min.var",
                                            use.covar.matrix            = FALSE,
                                            covar.type                  = "sample",
                                            correl.type                 = "sample",
                                            vola.type                   = "sample",
                                            exp.return                  = "sample",
                                            target.return               = NA,
                                            sum.weight                  = 1,
                                            min.single.weight           = -100,
                                            max.single.weight           = 100,
                                            alpha.cvar                  = NA,
                                            bl.risk.aversion.coeff      = 3.07,
                                            bl.tau                      = 0.025,
                                            bl.market.cap.weights       = NA,
                                            bl.ident.view.matrix        = NA,
                                            bl.diag.covar.error.matrix  = NA,
                                            bl.view.vector              = NA,
                                            in.sample                   = FALSE) {

first.out.of.sample.date <- lubridate::floor_date(min(daily.returns.data.wide$ref.date),"month")
#last.out.of.sample.date  <- lubridate::ceiling_date(max(daily.returns.data.wide$ref.date)  %m-% months(out.of.sample.period.months),"month")

first.investment.date    <- lubridate::floor_date(min(daily.returns.data.wide$ref.date)  %m+% months(out.of.sample.period.months),"month")
last.investment.date     <- lubridate::ceiling_date(max(daily.returns.data.wide$ref.date),"month")


N.pf.rebalances         <- (length(seq(from=first.investment.date, to=last.investment.date, by="months"))-1)/investment.period.months

portfolio.results.list     <- vector("list", N.pf.rebalances)
weights.result.table.list  <- vector("list", N.pf.rebalances)

for(i in 1:N.pf.rebalances) {
  print(paste(pf.opt.type, "Rebalancing Portfolio - RUN:",i))

  # 1) GENERATE OUT OF SAMPLE DATA
  out.of.sample.start.date <- lubridate::floor_date(first.out.of.sample.date %m+% months(investment.period.months*(i-1)),"month")
  out.of.sample.end.date   <- out.of.sample.start.date %m+% months(out.of.sample.period.months)-1
  print(paste("Out of Sample Period:",out.of.sample.start.date,"-",out.of.sample.end.date))

  investment.period.start.date <- lubridate::floor_date(first.investment.date %m+% months(investment.period.months*(i-1)),"month")
  investment.period.end.date   <- investment.period.start.date %m+% months(investment.period.months)-1
  print(paste("Investment Period:",investment.period.start.date,"-",investment.period.end.date))


  # load out of sample data
  daily.returns.data.wide.out.of.sample <- if(in.sample == FALSE) { daily.returns.data.wide[daily.returns.data.wide$ref.date >= out.of.sample.start.date &
                                                                                           daily.returns.data.wide$ref.date <= out.of.sample.end.date,] } else {
                                                                     daily.returns.data.wide[daily.returns.data.wide$ref.date  >= investment.period.start.date &
                                                                                             daily.returns.data.wide$ref.date  <= investment.period.end.date,]
                                                                   }

   single.title.stats.list <- SingleTitlestats(daily.returns.data.wide = daily.returns.data.wide.out.of.sample)

  #1b) GET MARKET WEIGHTS (OPTIONAL, FOR BLACK LITTERMAN ONLY)
   if(exists("bl.market.cap.weights") && is.data.frame(get("bl.market.cap.weights"))) {
     bl.market.cap.weights.current <- bl.market.cap.weights[bl.market.cap.weights$ref.date <= out.of.sample.end.date,]
     bl.market.cap.weights.current <- bl.market.cap.weights.current[bl.market.cap.weights.current$ref.date == max(bl.market.cap.weights.current$ref.date),]
     bl.market.cap.weights.current <- as.numeric(bl.market.cap.weights.current[,2:ncol(bl.market.cap.weights)])
     print(paste("MARKET WEIGHTS AS OF:",out.of.sample.end.date,":",paste(shQuote(bl.market.cap.weights.current), collapse=", ")))
   }

  asset.name     <- as.character(rownames(single.title.stats.list$single.title.stats))

  sigma.vector   <- if(vola.type    == "sample") {
                      single.title.stats.list$single.title.stats$sd.day}

  correl.matrix  <- if(correl.type    == "sample") {
                       single.title.stats.list$sample.correl.matrix}

  covar.matrix  <- if(covar.type    == "sample") {
                      single.title.stats.list$sample.covar.matrix } else
                    if(covar.type    == "shrink") {
                      single.title.stats.list$shrink.covar.matrix
                    } else stop("WRONG covar.type")
  print(paste("COVAR Type:",covar.type))



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
                    } else if(pf.opt.type == "mpt.min.var") {
                      print(paste("OPTIMIZATION TYPE:",pf.opt.type))
                      weights.vector <- meanVariancePortfolioOptimizerQP(mu.vector         = mu.vector,
                                                                         sigma.vector      = sigma.vector,
                                                                         correl.matrix     = correl.matrix,
                                                                         covar.matrix      = covar.matrix,
                                                                         use.covar.matrix  = use.covar.matrix,
                                                                         target.return     = target.return,
                                                                         sum.weight        = sum.weight,
                                                                         min.single.weight = min.single.weight,
                                                                         max.single.weight = max.single.weight,
                                                                         mvp               = TRUE)

                      # mpt.tar.ret
                     } else if(pf.opt.type == "mpt.tar.ret") {
                       print(paste("OPTIMIZATION TYPE:",pf.opt.type))
                       weights.vector <- meanVariancePortfolioOptimizerQP(mu.vector         = mu.vector,
                                                                          sigma.vector      = sigma.vector,
                                                                          correl.matrix     = correl.matrix,
                                                                          covar.matrix      = covar.matrix,
                                                                          use.covar.matrix  = use.covar.matrix,
                                                                          target.return     = target.return,
                                                                          sum.weight        = sum.weight,
                                                                          min.single.weight = min.single.weight,
                                                                          max.single.weight = max.single.weight,
                                                                          mvp               = FALSE)
                       }

                       # cvar
                       else if(pf.opt.type == "cvar") {
                       print(paste("OPTIMIZATION TYPE:",pf.opt.type))
                       weights.vector <- CVaRPortfolioOptimizer(daily.returns.data.wide = daily.returns.data.wide.out.of.sample,
                                                                alpha.cvar = alpha.cvar,
                                                                rmin = 0,
                                                                wmin = min.single.weight,
                                                                wmax = max.single.weight,
                                                                weight.sum = sum.weight)
                       }

                       # bl
                        else if(pf.opt.type == "bl.no.view") {
                       weights.vector <- BLPortfolioOptimizer(risk.aversion.coeff = bl.risk.aversion.coeff,
                                                              tau                 = bl.tau,
                                                              covar.matrix        = covar.matrix,
                                                              market.cap.weights  = bl.market.cap.weights.current,
                                                              view.vector         = NA)
                     }

  weights.result.table <- data.frame(start.out.of.sample = min(daily.returns.data.wide.out.of.sample$ref.date),
                                     end.out.of.sample   = max(daily.returns.data.wide.out.of.sample$ref.date),
                                     investment.start    = investment.period.start.date,
                                     investment.end      = investment.period.end.date,
                                     pf.opt.type         = pf.opt.type,
                                     asset.name          = asset.name,
                                     weights             = weights.vector)


  weights.result.table.list[[i]] <- weights.result.table

  # 3) GENERATE DATA FOR INVESTMENT PERIOD


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
