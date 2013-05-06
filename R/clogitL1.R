clogitL1 = function(y, X, strata, numLambda = 100, minLambdaRatio = 0.000001, switch = floor(0.9*numLambda), alpha = 1){
	
	# twiddle parameters for stability
	minLambdaRatio = max(c(minLambdaRatio, 0.000001))
	minLambdaRatio = min(c(minLambdaRatio, 1))

	alpha = max(c(alpha, 0.000001))
	alpha = min(c(alpha, 1))
	
	# make sure inputs are appropriate
	if (length(y) != length(strata)) stop("Please specify one stratum per observation")
	if (length(y) != nrow(X)) stop("Please ensure that each observation has predictors and response")
	if (any(y != 1 & y != 0)) stop("Response vector should contain 1 for cases and 0 for controls")

	# first put y and X into form conducive to call to C function
	# group strata together, putting cases first
	yC = NULL
	XC = NULL
	nVec = NULL
	mVec = NULL
	for (i in unique(strata)){
		currentX = X[strata==i, ]
		currentY = y[strata==i]

		yC = c(yC, currentY[currentY == 1], currentY[currentY==0])
		XC = rbind(XC, currentX[currentY == 1, ], currentX[currentY == 0, ])
		nVec = c(nVec, length(currentY))
		mVec = c(mVec, sum(currentY))
	}
	XC = as.matrix(XC)
	
	# fit the model - clogitL1_c returns a matrix
	# 	first ncol(x) columns: beta estimates
	#	column ncol(X)+1: lambda values, then non zero beta and strong set beta
	esti.beta = clogitl1_c (nVec, mVec, X, y, switch, numLambda, minLambdaRatio, alpha=alpha)

	out.beta = esti.beta[,1:ncol(X)] # maybe we stopped before we got to numLambda iterations
	empty.beta = apply (esti.beta, 1, function(x){all(x==0)})
	out.beta = out.beta[!empty.beta,]

	out = list(beta=out.beta, lambda=esti.beta[!empty.beta,ncol(X)+1], nz_beta=esti.beta[!empty.beta,ncol(X)+2], ss_beta=esti.beta[!empty.beta,ncol(X)+3], dev_perc=esti.beta[!empty.beta, ncol(X)+5], y_c=yC, X_c=XC, nVec=nVec, mVec=mVec, alpha=alpha)
	class(out) = "clogitL1"
	out
}
