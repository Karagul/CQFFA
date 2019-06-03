# In this file are all functions that are required for the CQF final project
# Author: Fabian Arter
# Date: 2019-05-29


###############################################################

#' etlFinData
#'
#' @description This function loads historical price data from Yahoo finance and transforms the data in a long and wide table with daily returns
#' @param start.date Start Date of the historical price data
#' @param end.date  End Date of the historical price data
#' @param input.tickers.df Data Frame with the products we wish to have the prices, this includes the ticker symbol and a friendly name
#' @return a list with two data frames: cumulated.returns.data.long and cumulated.returns.data.long
#' @examples
#' etlFinData()
#' @export
etlFinData <- function(start.date=as.Date("2019-01-01"),
                       end.date=as.Date("2019-05-27"),
                       input.tickers.df = data.frame(ticker=c("BA","AIR.PA"),
                                                     friendly.name=c("BOEING","AIRBUS"))
) {

  # load data via BatchGetSymbols
  daily.market.data <- BatchGetSymbols::BatchGetSymbols(
    tickers      = as.character(input.tickers.df$ticker),
    first.date   = start.date,
    last.date    = end.date,
    freq.data    = "daily",
    type.return  = "log",
    cache.folder = file.path(tempdir(), 'BGS_Cache') )$df.tickers


  # add friendly names
  daily.market.data <-  merge(daily.market.data, input.tickers.df, by="ticker")

  daily.market.data[is.na(daily.market.data$ret.adjusted.prices),]$ret.adjusted.prices <- 0
  daily.market.data <- na.omit(daily.market.data)

  daily.market.data <- data.frame(ref.date             = daily.market.data$ref.date,
                                  friendly.name   = as.character(daily.market.data$friendly.name),
                                  price.adjusted  = daily.market.data$price.adjusted,
                                  daily.return    = daily.market.data$ret.adjusted.prices)

  daily.returns.data.wide <- reshape2::dcast(daily.market.data,  ref.date ~ friendly.name, value.var = "daily.return")
  daily.returns.data.wide <- na.omit(daily.returns.data.wide)

  # cumulated returns wide
  cumulated.returns.data.wide  <- as.data.frame(cbind(ref.date = daily.returns.data.wide$ref.date, apply(daily.returns.data.wide[,2:ncol(daily.returns.data.wide)], 2, cumsum)))

  # cumulated returns long
  cumulated.returns.data.long          <- reshape2::melt(cumulated.returns.data.wide, id.vars=1, measure.vars = 2:ncol(cumulated.returns.data.wide))
  cumulated.returns.data.long$ref.date <- as.Date(cumulated.returns.data.long$ref.date, origin = "1970-01-01")
  cumulated.returns.data.long$name <- as.character(cumulated.returns.data.long$variable) ; cumulated.returns.data.long$variable <- NULL
  cumulated.returns.data.long$cumul.return  <- cumulated.returns.data.long$value ; cumulated.returns.data.long$value <- NULL

  return(list(daily.returns.data.wide     = daily.returns.data.wide,
              cumulated.returns.data.long = cumulated.returns.data.long))

}



#' PFstats
#'
#' @description This function creates all important descriptive statistics such as VaR, ES, Return for a portfolio
#' @param weights.vector vector that contains the relative weights of the individual assets of the portfolio
#' @param daily.returns.data.wide  data.frame including the daily returns of the specific assets as well as a reference date
#' @return a list with a data.frame PF.return.result.table that includes all statistics, as well as two graphs - histogram with returns and a correlation matrix
#' @examples
#' weights.vector          <- c(0.7,0.3)
#' daily.returns.data.wide <- data.frame(ref.date=c(Sys.Date()-2:0), asset1.ret=c(-0.02,0.005,0.004), asset2.ret=c(0,-0.001,0.02))
#' PFstats(weights.vector=weights.vector, daily.returns.data.wide=daily.returns.data.wide)
#' @export

PFstats <- function(weights.vector, daily.returns.data.wide) {


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
                                       PF.annualized.return = mean(PF.daily.return)*250,
                                       PF.sd.daily.return   = sd(PF.daily.return), # same as as.numeric(sqrt(t(weights.vector) %*% covar.matrix %*% weights.vector))
                                       PF.annualized.vola   = sd(PF.daily.return)*sqrt(250),
                                       PF.hist.1dVaR95 = as.numeric(quantile(PF.daily.return,0.05)),
                                       PF.hist.1dES95  = mean(head(sort(PF.daily.return),n=floor(0.05*length(PF.daily.return)))),
                                       PF.sharpe.ratio = (mean(PF.daily.return) / sd(PF.daily.return))*sqrt(250),
                                       PF.sortino.ratio = (mean(PF.daily.return) / sd(PF.daily.return[PF.daily.return<0]))*sqrt(250)
  )


  # PLOTS

  #### HISTOGRAM
  plot.hist.daily.return <- ggplot2::ggplot(data=data.frame(PF.daily.return), aes(PF.daily.return))
  plot.hist.daily.return <- plot.hist.daily.return + geom_histogram(aes(y=..density..), color="black", fill = "darkblue",binwidth = 0.002, alpha = 0.2) + xlim(-0.05,0.05)
  plot.hist.daily.return <- plot.hist.daily.return + stat_function(fun = dnorm, colour = "red", args = list(mean = mean(PF.daily.return), sd = sd(PF.daily.return)))
  plot.hist.daily.return <- plot.hist.daily.return + labs(title = paste0("Portfolio daily returns"," - PF ann. vola: ",round(PF.return.result.table$PF.annualized.vola*100,3),"%"), subtitle = paste(PF.return.result.table$start.date, " - ",PF.return.result.table$end.date))



  #### CORREL MATRIX
  #hist.return.df.long <- na.omit(data.frame(log.return=hist.daily.market.data$ret.closing.prices, asset=hist.daily.market.data$ticker))
  return.matrix       <- data.matrix(daily.return.table[2:ncol(daily.return.table)], rownames.force=TRUE)
  cor.mat             <- cor(return.matrix)

  plot.correl.matrix <- GGally::ggcorr(return.matrix,  palette='RdGy',digits=3, label=TRUE, label_round = 2, label_size=3, size = 2.2)
  plot.correl.matrix <- plot.correl.matrix + labs(title = "Correlation Matrix", subtitle = paste(min(daily.return.table$ref.date), " - ", max(daily.return.table$ref.date)))



  return(list(PF.return.result.table = PF.return.result.table,
              plot.hist.daily.return = plot.hist.daily.return,
              plot.correl.matrix     = plot.correl.matrix))
}


###############################################################
