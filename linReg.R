##
## Linear Regression for Binary Classification
## EdX - CaltechX CS1156x Learning From Data
## Author: Kai He
##

# Need getLine(), getLineData(), getClass(), PlotSim(), eval() from PLA.R code
source("F:/Google Drive/Study Note/edX-Machine Learning/hw/CS1156xHW1/PLA.R")

# define linear regression
linReg <- function(trainX, trainY){
	trainX = cbind(1, trainX)

	# through matrix inverse
	w <- solve(t(trainX) %*% trainX) %*% t(trainX) %*% trainY 
	w
}

# set up a simulation.
testLinReg <- function(N, is.in=T, plot=F){
	# initialize data and f line
	sampleX <- matrix(runif(2*N, -1, 1), N, 2)
	f <- getLine()
	sampleY <- getClass(sampleX, f)

	# get linear regression result, g
	g <- linReg(sampleX, sampleY)
	
	# predict
	predY <- getClass(sampleX, g)

	if(is.in){
		# evaluate in-sample error
		error <- mean(sampleY!=predY)
	}
	else{
		# evaluate out-of-sample error using Monte-Carlo method
		error <- eval(g, f)
	}
	
	if(plot){
		plotSim(sampleX, sampleY, g)
	}
	return(error)
}

iterLinReg <- function(N, iter, is.in=T){
	errorSet <- vector()
	for( i in 1:iter ){
		error <- testLinReg(N, is.in)
		errorSet <- c(error, errorSet)
	}
	
	mean(errorSet)
}

# combine LinReg with PLA by setting the former result as the initial guess
# in the latter

iter.LinReg.PLA <- function(N, iter){
	errSet = vector()
	countSet = vector()

	for(i in 1:iter){
		sampleX <- matrix(runif(2*N, -1, 1), N, 2)
		f <- getLine()
		sampleY <- getClass(sampleX, f)
		
		# run linear regression
		g <- linReg(sampleX, sampleY)
		
		# serve g as the initial guess in PLA
		res <- pla(sampleX, sampleY, g)

		err <- eval(res[[1]], f)
		errSet <- c(errSet, err)
		countSet <- c(countSet, res[[2]])
	}	
	#colMeans(data.frame(errSet, countSet))
	data.frame(errSet, countSet)
}
	