SIMULATION EXERCISE



set.seed(2015)
lambda <- 0.2 # the rate parameter lambda as instructed
n <- 40 # number of exponentials
sim <- 1000 # a thousand simulations

# the exponential distribution
plot(rexp(10000,lambda), pch=20, cex=0.6, main="The exponential distribution with rate 0.2 and 10.000 observations")


# generate the collection of means for 1000 simulations of the exponential distribution
means <- NULL
for (i in 1 : sim) means <- c(means, mean(rexp(n,lambda)))
hist(means, col="blue", main="rexp mean distribution", breaks=40)
rug(means)



SAMPLE MEAN VS THEORETICAL MEAN


hist(means, col="darkblue", main="Theoretical vs actual mean for rexp()", breaks=20)
abline(v=mean(means), lwd="4", col="red")
text(3.6, 90, paste("Actual mean = ", round(mean(means),2), "\n Theoretical mean = 5" ), col="red")

SAMPLE VARIANCE VS THEORETICAL VARIANCE


# theoretical standard deviation vs practical standard deviation
print (paste("Theoretical standard deviation: ", round( (1/lambda)/sqrt(n) ,4), ", Practical standard deviation", round(sd(means) ,4) ) )

# the variance should be:
print (paste("Theoretical variance: ", (1/lambda)^2/n, ", Practical variance", round(var(means) ,4) ) )

## [1] "Theoretical standard deviation:  0.7906 , Practical standard deviation 0.7847"
## [1] "Theoretical variance:  0.625 , Practical variance 0.6158"
## THEREFORE THE NUMBERS MATCH


IS THE DISTRIBUTION OF MEANS NORMAL

hist(means, prob=TRUE, col="lightblue", main="mean distribution for rexp()", breaks=20)
lines(density(means), lwd=3, col="blue")

