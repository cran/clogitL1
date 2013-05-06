\name{clogitL1}
\alias{clogitL1}
\title{Conditional logistic regression with elastic net penalties}
\description{
 Fit a sequence of conditional logistic regression models with lasso or elastic net penalties
}

\usage{
 clogitL1 (y, X, strata, numLambda=100, 
	minLambdaRatio=0.000001, switch=floor(0.9*numLambda), alpha = 1)
}

\arguments{
\item{y}{a vector of binary responses with 1 for cases and 0 for controls.}
\item{X}{a matrix with the same number of rows as the length of y and p columns. Contains the p-vector regressor values as rows}
\item{strata}{vector with stratum membership of each observation.}
\item{numLambda}{number of different values of the regularisation parameter \eqn{\lambda} at which to parameter estimates. First fit is made at value just below smallest regularisation parameter value at which all parameter estimates are 0; last fit made at this value multipled by \code{minLambdaRatio}}
\item{minLambdaRatio}{ratio of smallest to larget value of regularisation parameter \eqn{\lambda} at which we find parameter estimates.}
\item{switch}{index (between 0 and \code{numLambda}) at which we transition from linear to logarithmic jumps.}
\item{alpha}{parameter controling trade off between lasso and ridge penalties. At value 1, we have a pure lasso penalty; at 0, pure ridge. Intermediate values provide a mixture of the two.}
}

\details{
The sequence of models implied by \code{numLambda} and \code{minLambdaRatio} is fit by coordinate descent with warm starts and sequential strong rules. If \code{alpha=1}, we fit using a lasso penalty. Otherwise we fit with an elastic net penalty. Note that a pure ridge penalty is never obatined, because the function sets a floor for \code{alpha} at 0.000001. This improves the stability of the algorithm. A similar lower bound is set for \code{minLambdaRatio}. The sequence of models can be truncated at fewer than \code{numLambda} models if it is found that a very large proportion of training set deviance is explained by the model in question.

}

\value{
 An object of type \code{clogitL1} with the following fields:

\item{beta}{a (\code{numLambda} + 1)-by-p matrix of estimated coefficients. First row has all 0s}
\item{lambda}{a vector of length \code{numLambda} + 1 containing the value of the regularisation parameter at which we obtained the fits.}
\item{nz_beta}{a vector of length \code{numLambda} + 1 containg the number of nonzero parameter estimates for the fit at the corresponding regularisation parameter.}
\item{ss_beta}{a vector of length \code{numLambda} + 1 containing the number of predictors considered by the sequential strong rule at that iteration.}
\item{dev_perc}{a vector of length \code{numLambda} + 1 containing the percentage of null deviance explained by the model represented by that row in the matrix.}
\item{y_c}{reordered vector of responses. Grouped by stratum with cases coming first.}
\item{X_c}{reordered matrix of predictors. See above.}
\item{strata_c}{reordered stratum vector. See above.}
\item{nVec}{vector of length the number of unique strata in \code{strata} containing the number of observations encountered in each stratum.}
\item{mVec}{vector containing the number of cases in each stratum.}
\item{alpha}{penalty trade off parameter.}
}

\seealso{
 \code{\link{plot.clogitL1}}
}

\examples{

set.seed(142)
# data parameters
K = 10 # number of strata
n = 5 # number in strata
m = 2 # cases per stratum
p = 20 # predictors

# generate data
y = rep(c(rep(1, m), rep(0, n-m)), K)
X = matrix (rnorm(K*n*p, 0, 1), ncol = p) # pure noise
strata = sort(rep(1:K, n))

par(mfrow = c(1,2))
# fit the conditional logistic model
clObj = clogitL1(y, X, strata)
plot(clObj, logX=TRUE)

# cross validation
clcvObj = cv.clogitL1(clObj)
plot(clcvObj)
}