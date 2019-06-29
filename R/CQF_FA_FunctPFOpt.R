# In this file are all PORTFOLIO OPTIMIZATION functions that are required for the CQF final project
# Author: Fabian Arter
# Date: 2019-05-29

# generate PDF manual
# system("R CMD Rd2pdf /Users/fabianarter/Library/Mobile Documents/com~apple~CloudDocs/Education/CQF/Final_Project/CQFFA")

###############################################################


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
#' @param covar.matrx covariance matrix
#' @param use.covar.matrix use covariance matrix directly or build it via sigma vector and the correlation matrix, default is FALSE
#' @param target.return which return level is seeked (for which the variance is minimized)
#' @param rf risk free return
#' @return weight.risky.assets a vector with the weights of the risky assets
#' @examples
#' weights.vector          <- c(0.7,0.3)
#' daily.returns.data.wide <- data.frame(ref.date=c(Sys.Date()-2:0), asset1.ret=c(-0.02,0.005,0.004), asset2.ret=c(0,-0.001,0.02))
#' PFstats(weights.vector=weights.vector, daily.returns.data.wide=daily.returns.data.wide)
#' @export
meanVariancePortfolioOptimizer <- function(asset.name,
                                           mu.vector,
                                           sigma.vector,
                                           correl.matrix,
                                           covar.matrix=NA,
                                           use.covar.matrix=FALSE,
                                           target.return,
                                           rf,
                                           print.out=FALSE,
                                           opt.focus.type="return") {

  one.vector <- rep(1,length(mu.vector))

  SRS <- if(use.covar.matrix==FALSE) {
  S          <- diag(sigma.vector)
  R          <- correl.matrix
  SRS        <- S %*% R %*% S
  } else {covar.matrix}

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


#########################################################################

#' meanVariancePortfolioOptimizerQP
#'
#' @description Mean Variance Portfolio Optimizer using Quadratic Programming
#' @param asset.name Name of assets available
#' @param mu.vector vector with estimated returns of investment objects
#' @param sigma.vector vector with the volatilities of the investment objects
#' @param correl.matrix correlation matrix of the investment objects
#' @param covar.matrx covariance matrix
#' @param use.covar.matrix use covariance matrix directly or build it via sigma vector and the correlation matrix, default is FALSE
#' @param target.return which return level is seeked (for which the variance is minimized)
#' @param sum.weight default is 1
#' @param min.single.weight default is -100
#' @param max.single.weight default is +100
#' @return weight.risky.assets a vector with the weights of the risky assets
#' @examples
#' weights.vector          <- c(0.7,0.3)
#' daily.returns.data.wide <- data.frame(ref.date=c(Sys.Date()-2:0), asset1.ret=c(-0.02,0.005,0.004), asset2.ret=c(0,-0.001,0.02))
#' PFstats(weights.vector=weights.vector, daily.returns.data.wide=daily.returns.data.wide)
#' @export
meanVariancePortfolioOptimizerQP <- function(
                                           mu.vector,
                                           sigma.vector=NA,
                                           correl.matrix=NA,
                                           covar.matrix=NA,
                                           use.covar.matrix=FALSE,
                                           target.return,
                                           sum.weight = 1,
                                           min.single.weight =-100,
                                           max.single.weight = 100,
                                           mvp = FALSE

                                           ) {

  covar.matrix <- if(use.covar.matrix==FALSE) {
    S          <- diag(sigma.vector)
    R          <- correl.matrix
    SRS        <- S %*% R %*% S
    SRS
  } else {covar.matrix}




  if(mvp==FALSE) {
  Dmat    <- covar.matrix
  dvec    <- mu.vector
  A.equal <- matrix(rep(sum.weight,length(mu.vector)), ncol=1)
  Amat    <- cbind(A.equal, dvec, diag(length(mu.vector)), -diag(length(mu.vector)))
  bvec    <- c(sum.weight, target.return, rep(min.single.weight, length(mu.vector)), rep(-max.single.weight, length(mu.vector)))

  weight.solution <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq=1)
  optimal.weights <- weight.solution$solution
  } else {
    Dmat    <- covar.matrix
    dvec    <- rep(0,nrow(covar.matrix))
    A.equal <- matrix(rep(sum.weight,length(mu.vector)), ncol=1)
    Amat <- cbind(A.equal, dvec, diag(length(mu.vector)), -diag(length(mu.vector)))
    bvec    <- c(sum.weight, 0, rep(min.single.weight, length(mu.vector)), rep(-max.single.weight, length(mu.vector)))

    weight.solution <- solve.QP(Dmat=covar.matrix,dvec=rep(0,nrow(covar.matrix)),Amat,bvec,meq=1)
    optimal.weights <- weight.solution$solution
}
return(optimal.weights)
}

#########################################################################

#' CVaRPortfolioOptimizer
#'
#' @description CVaR Portfolio Optimizer based on Yollin (2009)
#' @param daily.returns.data.wide data.frame including the daily returns of the specific assets as well as a reference date
#' @param alpha Alpha of the CVaR, this is the confidence level from which on the average of the tail risk is being calculated
#' @param rmin Required return, default is 0
#' @param wmin Minimal weight of a single asset, default is 0
#' @param wmax Maximal weight of a single asset, default is 1
#' @param weight.sum Total weight of all assets, default is 1
#' @return cvar CVaR of the specific portfolio or asset with the set alpha
#' @export
CVaRPortfolioOptimizer = function(daily.returns.data.wide, alpha.cvar=0.05, rmin=0, wmin=0, wmax=1, weight.sum=1)
{

  rmat = as.matrix(daily.returns.data.wide[,2:ncol(daily.returns.data.wide)])


  n = ncol(rmat) # number of assets
  s = nrow(rmat) # number of scenarios i.e. periods
  averet = colMeans(rmat)
  # creat objective vector, constraint matrix, constraint rhs
  Amat = rbind(cbind(rbind(1,averet),matrix(data=0,nrow=2,ncol=s+1)),
               cbind(rmat,diag(s),1))
  objL = c(rep(0,n), rep(-1/(alpha.cvar*s), s), -1)
  bvec = c(weight.sum,rmin,rep(0,s))
  # direction vector
  dir.vec = c("==",">=",rep(">=",s))
  # bounds on weights
  bounds = list(lower = list(ind = 1:n, val = rep(wmin,n)),
                upper = list(ind = 1:n, val = rep(wmax,n)))
  res = Rglpk::Rglpk_solve_LP(obj=objL, mat=Amat, dir=dir.vec, rhs=bvec,
                       types=rep("C",length(objL)), max=T, bounds=bounds)
  optimal.weights = as.numeric(res$solution[1:n])
  return(optimal.weights)
}

#########################################################################

#' BLPortfolioOptimizer
#'
#' @description Black Litterman Portfolio Optimizer
#'
#' @param risk.aversion.coeff (lambda) risk aversion coefficient, default is 3
#' @param market.cap.weights is a vector with the market capitalization weights
#'
#' @param tau default is 0.025
#' @param covar.matrix Covariance Matrix of N assets (N x N Matrix)
#' @param ident.view.matrix (BL: P) Matrix that identifies the assets involved in the views (K x N Matrix)
#' @param diag.covar.error.matrix (BL: Omega)
#'                                A diagonal covariance matrix of error terms from the expressed views
#'                                 representing the uncertainty in each view ( K x K Matrix)
#' @param view.vector (BL: Q) Vector including the Views (K x 1 column vector)
#' @return cvar CVaR of the specific portfolio or asset with the set alpha
#' @export
BLPortfolioOptimizer = function(risk.aversion.coeff=3.07,
                                tau=0.025,
                                covar.matrix,
                                market.cap.weights,
                                ident.view.matrix=NA,
                                diag.covar.error.matrix=NA,
                                view.vector=NA
                                )
{


  # 1) Implied Excess Equilibrium Market Returns
  Pi <- risk.aversion.coeff * covar.matrix %*% market.cap.weights

  # 2a) Weights without Views
  BL.weights <- solve(risk.aversion.coeff * covar.matrix) %*% Pi

  ##### WITH VIEWS #####
  if( !(is.na(view.vector))) {
  Q <- view.vector # K x 1
  P <- ident.view.matrix # K x N , check that each row sums to 0

  Omega <- diag.covar.error.matrix # K x K

  print(paste("Number of Views:",length(Q)))

  if(all(rowSums(P)==0)==FALSE) {stop("Check your ident.view.matrix, rows do not sum to zero")}

  if(nrow(P)!=length(Q)) {stop("Number of Rows in ident.view.matrix differ from Number of Views ")}

  if(ncol(P)!=nrow(covar.matrix)) {stop("Number of Rows in ident.view.matrix differ from Number of Assets")}

  if(nrow(Omega)!=length(Q) && ncol(Omega)!=length(Q)) {stop("diag.covar.error.matrix not K x K length")}


  # 2) Expected Returns
  expected.returns.vector <- solve(solve(tau * covar.matrix) + t(P) %*% solve(Omega)%*% P) %*%
    (solve(tau * covar.matrix) %*% Pi + t(P) %*% solve(Omega) %*% Q)


  # 3) Uncertainty of Returns
  #uncert.returns.vector <-solve(solve(tau * covar.matrix) + t(ident.view.matrix) %*% solve(diag.covar.error.matrix) %*% view.vector)


  # 4) New Covariance Matrix
  # new.covar.matrix <- covar.matrix + uncert.returns.vector

  # 5) Final Weights
  BL.weights <- solve(risk.aversion.coeff * new.covar.matrix) %*% new.expected.returns.vector
}

  BL.weights <- c(BL.weights)

return(BL.weights)
}

#########################################################################
#########################################################################
