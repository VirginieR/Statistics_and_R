# T- test exercises
library(downloader)
library(dplyr)
library(rafalib)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist


# We can look for the true population difference in means between smoking and non-smoking birth weights

mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

# We are interested in testing whether the birth weights of babies born to non-smoking mothers 
# are significantly different from the birth weights of babies born to smoking mothers.

set.seed(1)

dat.ns <- sample(bwt.nonsmoke,25)
dat.s <- sample(bwt.smoke,25)

# Compute the t-statistic (call it tval) between dat.ns and dat.s
tval <- t.test(dat.ns, dat.s)$statistic
print (tval)

# Recall that we summarize our data using a t-statistics because we know that in situations 
# where the null hypothesis is true (what we mean when we say "under the null") and the sample size 
# is relatively large, this t-value will have an approximate standard normal distribution. 
# Because we know the distribution of the t-value under the null, we can quantitatively determine how 
# unusual the observed t-value would be if the null hypothesis were true.

# The standard procedure is to examine the probability a t-statistic that actually does follow 
# the null hypothesis would have larger absolute value than the absolute value of the t-value we 
# just observed -- this is called a two-sided test.

# We have computed these by taking one minus the area under the standard normal curve 
# between -abs(tval) and abs(tval). In R, we can do this by using the pnorm() function, 
# which computes the area under a normal curve from negative infinity up to the value given 
# as its first argument:

pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))
print (pval)

# By reporting only p-values, many scientific publications provide an incomplete story 
# of their findings. As we have mentioned, with very large sample sizes, scientifically 
# insignificant differences between two groups can lead to small p-values. 
# Confidence intervals are more informative as they include the estimate itself. 
# We will learn how to compute these in the next module.




