##
## Coin simulation: 1000 coins, 10 flips each round, 100,000 rounds.
##

rnd <- 10000
c1 <- 0
crand <- 0
cmin <- 0 
for(iter in 1:rnd){
	coins <- matrix(rbinom(10*1000, 1, 0.5), 1000, 10)
	frac <- rowSums(coins)/10
	c1 <- c1 + frac[1]
	crand <- crand + frac[sample(1000, 1)]
	cmin <- cmin + min(frac)
}
c1 <- c1 / rnd
crand <- crand / rnd
cmin <- cmin/ rnd
print(c(c1, crand, cmin))