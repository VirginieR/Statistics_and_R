
# When the histogram of a list of numbers approximates the normal distribution, 
# we can use a convenient mathematical formula to
# approximate the proportion of values or outcomes in any given interval: given the integral.

# Average or mean and standard deviation define the integral we need to know about the distribution.

# The mean : sum of all the values divided by the total number of values
# The standard deviation: is like the distance between each value and the mean. (Average distance from the mean).

# Standardized units:
# How many standard deviations away from the mean?

# zi = xi - mu /Sx --> (This is used to define z-scores)

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

# make averages5
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}

print ('Averages5')
print (mean(averages5))

# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}

print ('Averages50')
print (mean(averages50))

hist(averages5)
hist(averages50)

# They both look roughly normal, but with a sample size of 50 the spread is smaller.

# What proportion are between 23 and 25?

below_25 <- pnorm(25,mean(averages50),sd(averages50)) 
below_23 <- pnorm(23,mean(averages50),sd(averages50)) 

print (below_25)
print (below_23)
print (below_25 - below_23)

# What is the proportion of observations between 23 and 25 in a normal 
# distribution with average 23.9 and standard deviation 0.43?

below_25 <- pnorm(25,23.9,0.43) 
below_23 <- pnorm(23,23.9,0.43) 

print (below_25)
print (below_23)
print (below_25 - below_23)
