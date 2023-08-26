# Code is avaialbe at :
# http://genomicsclass.github.io/book/pages/random_variables.html

# As scientists we need to be skeptics. 
# How do we know that this obsdiff is due to the diet? 
# What happens if we give all 24 mice the same diet? Will we see a difference this big? 
# Statisticians refer to this scenario as the null hypothesis.

library(downloader)
set.seed(1)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- "femaleControlsPopulation.csv"
if (!file.exists(filename)) download(url,destfile=filename)

population <- read.csv(filename)
population <- unlist(population) # turn it into a numeric

##12 control mice
control <- sample(population,12)
##another 12 control mice that we act as if they were not
treatment <- sample(population,12)
print(mean(treatment) - mean(control))

n <- 10000
null <- vector("numeric",n)
for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  null[i] <- mean(treatment) - mean(control)
}
# The values in null form what we call the null distribution. 
# So what percent of the 10,000 are bigger than obsdiff?
mean(null >= obsdiff) # And that gives us the proportion of times.

hist(null)
# Only a small percent of the 10,000 simulations. 
# As skeptics what do we conclude? 
# When there is no diet effect, we see a difference as big as the one we observed only 1.5% of the time. 
# This is what is known as a p-value.
# The p-value is the answer to the question, what is the probability that an outcome from the null distribution
# is bigger than what we observed when the null hypothesis is true.
# So here, we got a 2.6.
# Typically, in the scientific literature, that's
# considered small enough for us to report the result. However,
# we should keep in mind that you can just report it as a number.
# It doesn't have to be and either/or situation,
# where you say it either is significant or not, although that's
# how many scientific journals report their results.
# What we can say for sure that the p-value here is 0.0263.
# ------------------------------------------------------------
  
# Exercise

set.seed(1)
n <- 10000
sampling <- vector("numeric",n)
for (i in 1:n) {
  control <- sample(population,5)
  sampling[i] <-  mean(control)
}

hist(sampling) ##take a look
mean( abs( sampling - mean(population) ) > 1)



