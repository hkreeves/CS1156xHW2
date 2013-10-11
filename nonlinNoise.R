##
## Linear Regression for Binary Classification 
## in Non-linear Case with Noise
##
## EdX - CaltechX CS1156x Learning From Data
## Author: Kai He
##

source("linReg.R")

# set boundary decision
dec <- function(x){
	x[1]^2+x[2]^2-0.6
}

# given a boundary decision f, where f:=f(X), assign labels accordingly
getClass2 <- function(X, f){
	# convert to a 1*2 matrix when X is a vector (one point only)
	if(is.vector(X)) X <- matrix(X, nrow=1)	

	apply(X, 1, function(x) sign(f(x)))
}

# evaluate out-of-sample (test set) error rate
eval2 <- function(hypo, real, noiseR, plot=FALSE, nl.trans=F, size=1000){
	hypo.form <- hypo[[1]]
	hypo.clf <- hypo[[2]]
	real.form <- real[[1]]
	real.clf <- real[[2]]
	
	# monte-carlo test data
	testX <- matrix(runif(2*size, -1, 1), size, 2)
	testY <- real.clf(testX, real.form)	
	# inject noise
	noi <- sample(1:size, as.integer(size*noiseR))
	testY[noi] <- -testY[noi]

	if(nl.trans)
		testX.new <- cbind(testX, testX[,1]*testX[,2], testX^2)
	else
		testX.new <- testX
	predY <- hypo.clf(testX.new, hypo.form)
	
	# only in the absence of transform does the plot option be available
	if(plot & !nl.trans) plotSim(testX, testY, hypo.form)
	mean(predY!=testY)
}
	

# set up a simulation with noise
testNoise <- function(N, noiseR=0.1, is.in=T, plot=F){
	sampleX <- matrix(runif(N*2, -1, 1), nrow=N)
	sampleY <- getClass2(sampleX, dec)

	# inject noise
	noi <- sample(1:N, as.integer(N*noiseR))
	sampleY[noi] <- -sampleY[noi]
	
	# perform linear regression
	g <- linReg(sampleX, sampleY)

	# predict
	predY <- getClass(sampleX, g)

	if(is.in){
		# evaluate in-sample error
		error <- mean(sampleY!=predY)
		
		if(plot) plotSim(sampleX, sampleY, g)
	}
	else{
		# evaluate out-of-sample error using Monte-Carlo method
		error <- eval2(list(g, getClass), list(dec, getClass2),
				noiseR=noiseR, plot=plot)
	}
	
	error
}

iterNoise <- function(N, iter, noiseR=0.1, is.in=T){
	errorSet <- vector()
	for( i in 1:iter ){
		error <- testNoise(N, noiseR, is.in)
		errorSet <- c(error, errorSet)
	}
	
	mean(errorSet)
}

# set a simulation with non-linear transformation of X
# (x1,x2) -> (x1, x2, x1*x2, x1^2, x2^2) 
testNonLinTrans <- function(N, noiseR=0.1, is.in=T, plot=F)
{
	sampleX <- matrix(runif(N*2, -1, 1), nrow=N)
	sampleY <- getClass2(sampleX, dec)
	# inject noise
	noi <- sample(1:N, as.integer(N*noiseR))
	sampleY[noi] <- -sampleY[noi]

	# transform sampleX nonlinearly
	transfX <- cbind(sampleX, sampleX[,1]*sampleX[,2], sampleX^2)
	
	# perform linear regression
	g <- linReg(transfX, sampleY)
	# gg <- lm(sampleY ~ sampleX[,1] + sampleX[,2] + sampleX[,1]:sampleX[,2])
	# print(g)
	# print(summary(gg))

	# predict
	predY <- getClass(transfX, g)

	if(is.in) 
		error <- mean(sampleY!=predY)
	else
		error <- eval2(list(g, getClass), list(dec, getClass2),
				noiseR=noiseR, plot=plot, nl.trans=T)
	error		
}
	