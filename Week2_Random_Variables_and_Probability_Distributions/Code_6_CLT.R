# Information Here:
# http://genomicsclass.github.io/book/pages/clt_and_t-distribution.html

# The CLT is one of the most frequently used mathematical results in science. It tells us that when the sample size is large, 
#the average Y¯ of a random sample follows a normal distribution centered at the population average μY
# and with standard deviation equal to the population standard deviation σY, divided by the square root of the sample size N. 
# We refer to the standard deviation of the distribution of a random variable as the random variable’s standard error.

# Please note that if we subtract a constant from a random variable, the mean of the new random variable shifts by that constant. 
# To see how intuitive this is, imagine that we subtract 10 grams from each of the mice weights. 
# The average weight should also drop by that much. 

library(downloader) 
library(rafalib)
library(dplyr)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )

# If a list of numbers has a distribution that is well approximated by the normal distribution, 
# what proportion of these numbers are within one standard deviation away from the list's average?
pnorm(1)-pnorm(-1)

# What proportion of these numbers are within two standard deviations away from the list's average?
pnorm(2)-pnorm(-2)

# What proportion of these numbers are within three standard deviations away from the list's average?
pnorm(3)-pnorm(-3)

y <- filter(dat,Sex == "M" & Diet == "chow") %>%  select(Bodyweight) %>% unlist
sd <- popsd(y)

# Standardized units:
# How many standard deviations away from the mean?
# What proportion of the mice are within one standard deviation away from the average weight?

z = (y - mean(y)) / sd 
mean( abs(z) <=1 )


# What proportion of the mice are within two standard deviation away from the average weight?
z = (y - mean(y)) / sd 
mean( abs(z) <=2 )

# What proportion of the mice are within three standard deviation away from the average weight?
z = (y - mean(y)) / sd 
mean( abs(z) <=3 )

# Note that the numbers for the normal distribution and our weights are relatively close. 
# Also, notice that we are indirectly comparing quantiles of the normal distribution to quantiles 
# of the mouse weight distribution. We can actually compare all quantiles using a qqplot.

qqnorm(z)
abline(0,1)

# As predicted by the CLT, the distribution of the random variable is very well 
# approximated by the normal distribution.
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs)
popsd(avgs)
