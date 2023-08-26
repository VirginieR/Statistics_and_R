# Info page class:
# http://genomicsclass.github.io/book/pages/t-tests_in_practice.html

# How to obtain a p-value in practice

library(dplyr)
dir <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/"
filename <- "femaleMiceWeights.csv"
url <- paste0(dir, filename)
dat <- read.csv(url)

control <- filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(dat,Diet=="hf") %>% select(Bodyweight) %>% unlist

diff <- mean(treatment) - mean(control)
print(diff)

# We learned that diff, referred to as the observed effect size, is a random variable. 
# We also learned that under the null hypothesis, the mean of the distribution of diff is 0. 
# What about the standard error? We also learned that the standard error of this random variable 
# is the population standard deviation divided by the square root of the sample size:

# --------------------
# Standard Deviation:
#  The standard deviation measures the average distance of data points from the mean 
# (average) of a dataset. It provides a sense of how spread out the data points are around the mean. 
# A higher standard deviation indicates greater variability, while a lower standard deviation 
# indicates less variability. 

# Standard Error:
#  The standard error is a measure of how much the sample mean varies from the true population mean. 
# It's particularly useful in estimating how much the sample mean might vary from the true mean in 
# repeated sampling. The standard error is calculated as the standard deviation of a sample divided 
# by the square root of the sample size.
# --------------------

# This is the SE of the sample average, but we actually want the SE of diff.
sd(control)/sqrt(length(control))

# We saw how statistical theory tells us that the variance of the difference of two random 
# variables is the sum of its variances, so we compute the variance and take the square root:
  
se <- sqrt( 
  var(treatment)/length(treatment) + 
    var(control)/length(control) 
) 

# Statistical theory tells us that if we divide a random variable by its SE, 
# we get a new random variable with an SE of 1.
 
tstat <- diff/se 

# This ratio is what we call the t-statistic. It’s the ratio of two random variables 
# and thus a random variable. Once we know the distribution of this random variable, 
# we can then easily compute a p-value.

# So now to calculate a p-value all we need to do is ask: how often does a normally 
# distributed random variable exceed diff? R has a built-in function, pnorm, to answer 
# this specific question. pnorm(a) returns the probability that a random variable following 
# the standard normal distribution falls below a. 

# To obtain the probability that it is larger than a, we simply use 1-pnorm(a). 
# We want to know the probability of seeing something as extreme as diff: either smaller (more negative) 
# than -abs(diff) or larger than abs(diff). We call these two regions “tails” and calculate their size:
  
righttail <- 1 - pnorm(abs(tstat)) 
lefttail <- pnorm(-abs(tstat))
pval <- lefttail + righttail
print(pval) 

# In this case, the p-value is smaller than 0.05 and using the conventional cutoff of 0.05, 
# we would call the difference statistically significant.

# As described earlier, statistical theory offers another useful result. 
# If the distribution of the population is normal, then we can work out the exact 
# distribution of the t-statistic without the need for the CLT. This is a big “if” 
# given that, with small samples, it is hard to check if the population is normal. 
# Furthermore, we can look at a qq-plot for the samples. 
# This shows that the approximation is at least close:

library(rafalib)
mypar(1,2)

qqnorm(treatment)
qqline(treatment,col=2)

qqnorm(control)
qqline(control,col=2)

# If we use this approximation, then statistical theory tells us that the distribution 
# of the random variable tstat follows a t-distribution. This is a much more complicated 
# distribution than the normal. The t-distribution has a location parameter like the normal 
# and another parameter called degrees of freedom. R has a nice function that actually 
# computes everything for us.

result <- t.test(treatment, control)
result$p.value

# The p-value is slightly bigger now. This is to be expected because our CLT approximation 
# considered the denominator of tstat practically fixed (with large samples it practically is), 
# while the t-distribution approximation takes into account that the denominator 
# (the standard error of the difference) is a random variable. The smaller the sample size, 
# the more the denominator varies.

# It may be confusing that one approximation gave us one p-value and another gave us another, 
# because we expect there to be just one answer. However, this is not uncommon in data analysis. 
# We used different assumptions, different approximations, and therefore we obtained different results.


library(dplyr)
dat <- read.csv("mice_pheno.csv")
control <- filter(dat,Diet=="chow") %>% select(Bodyweight) 
treatment <- filter(dat,Diet=="hf") %>% select(Bodyweight) 
t.test(treatment,control)$p.value



  
