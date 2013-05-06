plot.cv.clogitL1 = function(x, ...){
	# Function for plotting a cv.clogitL1 object - plots the mean cv deviance and standard error bars

	plot (x=c(x$lambda, x$lambda), y = c(x$mean_cv + x$se_cv, x$mean_cv - x$se_cv), type = "n", xlab = "log(lambda)", ylab = "Conditional likelihood deviance", ...)
	points (x=x$lambda, y=x$mean_cv, col = "red") # mean cv deviance
	axis(side=3, at = x$lambda, labels=x$nz_beta, tick=FALSE) # number of non-zero predictors on top axis
	for (i in 1:length(x$lambda)){ # standard error bounds
		lines (x = rep(x$lambda[i], 2), y = x$mean_cv[i] + c(x$se_cv[i], -x$se_cv[i]), col = "gray") # verical lines
		lines (x = x$lambda[i] + c(-0.03, 0.03), y = rep (x$mean_cv[i] + x$se_cv[i], 2), col = "gray") # top horizontal line
		lines (x = x$lambda[i] + c(-0.03, 0.03), y = rep (x$mean_cv[i] - x$se_cv[i], 2), col = "gray") # bottom horizontal line
	}
	abline (v = x$minCV_lambda, lty = 2) # minimum CV lambda
	abline (v = x$minCV1se_lambda, lty=2) # 1SE-rule lambda
}