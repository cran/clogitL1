plot.clogitL1 = function(x, logX=T, ...){
	# Plots the parameter profiles against (log) regularisation parameter
	# If 'logX' is TRUE, then we plot it against the log regularisation parameter
	
	if (logX) horiz = log(x$lambda)
	else horiz = x$lambda
	matplot (x=horiz, y=x$beta, type = "l", xlab = "Regularisation parameter", ylab = "Parameter estimate", ...)
}