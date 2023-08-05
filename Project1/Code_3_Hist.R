library(UsingR)

# Heights of indivous
x=father.son$fheight

#Height of 20 individuals
round(sample(x,20),1)

#----------------------------------------------
# To describe the data we use the hist
bins <- seq(floor(min(x)),ceiling(max(x)))
hist(x, breaks=bins, xlab="Height",main="Adult men heights")

#----------------------------------------------
# Another way is to use the Empiral Cumulative Density Function (ECDF)

# For any number x the empirical CDF reports the proportion of numbers in our list smaller or equal to x.

myCDF <- ecdf(x) 
# We will evaluate the function at these values:
xs<-seq(floor(min(x)),ceiling(max(x)),1) 

# and then plot them:
plot(xs,myCDF(xs),type="l",xlab="x=Height",ylab="F(x)")

#----------------------------------------------
# How to approximate to the normal distribution

mu <- mean(x)
popsd <- function(x) sqrt(mean((x-mean(x))^2)) 
popsd(x)

# Proportion of individuals that are taller than 70, so we can use the normal distribution
mean(x>70)
1-pnorm(70,mean(x),popsd(x))

#----------------------------------------------
# To continue the approximation we can do the QQ plot
# To corroborate that the normal distribution is in fact a good approximation we can use quantile-quantile plots (QQ-plots). 

# Percentiles from 1% to 99%
ps <- seq(0.01,0.99,0.01)
# Compute the percentiles for our data
qs <- quantile(x,ps)

# Doing the same for the normal distribution
normalqs <- qnorm(ps,mean(x),popsd(x))
plot(normalqs,qs,xlab="Normal percentiles",ylab="Height percentiles")
abline(0,1) ##identity line

#----------------------------------------------
# To continue the approximation we can do the QQ plot
# This makes the above automatically
qqnorm(x)
qqline(x)

#----------------------------------------------
# Non-normally distributed data looks
# On t distribution
dfs <- c(3,6,12,30)
mypar2(2,2)
for(df in dfs){
  x <- rt(1000,df)
  qqnorm(x,xlab="t quantiles",main=paste0("d.f=",df),ylim=c(-6,6))
  qqline(x)
}
