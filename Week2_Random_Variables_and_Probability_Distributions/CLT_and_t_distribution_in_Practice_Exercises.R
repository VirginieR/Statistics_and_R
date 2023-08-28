
library(downloader)
library(rafalib)
library(dplyr)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)

# We want to roll n dice 10,000 times and keep these proportions. 
# This random variable (proportion of 6s) has mean p=1/6 and variance p*(1-p)/n. 
# So according to the CLT, z = (mean(x==6) - p) / sqrt(p*(1-p)/n) should be normal with mean 0 and SD 1.

set.seed(1)
n=100
sides <- 6
p <- 1/sides
zs <- replicate(10000,{
  x <- sample(1:sides,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
})

qqnorm(zs)
abline(0,1) 
mean(abs(zs) > 2)

# 2.
# In the example used in exercise 1, the original data is binary (either 6 or not). 
# In this case, the success probability also affects the appropriateness of the CLT. 
# With very low probabilities, we need larger sample sizes for the CLT to "kick in".

# Run the simulation from exercise 1, but for different values of p and n. 
# For which of the following is the normal approximation best?

Ns <- c(5, 30, 30, 100)
Ps <- c(0.5, 0.5, 0.1, 0.01)

parameter_matrix <- cbind(Ps, Ns)
print(parameter_matrix)

res <- sapply(1:nrow(parameter_matrix), function(i) {
  replicate(10000, {
    p <- parameter_matrix[i, 1]  # Accessing the P column (p values)
    n <- parameter_matrix[i, 2]  # Accessing the N column (n values)
    sides <- 1/p
    x <- sample(1:sides, n, replace = TRUE)
    (mean(x == 1) - p) / sqrt(p * (1 - p) / n)
  })
})

# Now we can use qq-plots to see how well CLT approximations works for these. 
mypar(2,2)
for (i in seq(along=Ns)) {
  titleavg <- signif(mean(res[,i]),3)
  titlesd <- signif(popsd(res[,i]),3)
  title <- paste0("N=",Ns[i]," Avg=",titleavg," SD=",titlesd)
  qqnorm(res[,i],main=title)
  qqline(res[,i],col=2)
}

# Res: p=0.5 and n=30

# 3.

X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

mean(X)
sd(X)

2 * ( 1-pnorm(2/sd(X) * sqrt(12) ) )
se <- sqrt((sd(Y)^2/12)+(sd(X)^2/12))

tstat <- (mean(Y)-mean(X))/se 

# In particular, you will need to remember that the t-distribution is centered at
# 0 and has one parameter: the degrees of freedom, that control the size of the tails. 
# You will notice that if X follows a t-distribution the probability that X is smaller 
# than an extreme value such as 3 SDs away from the mean grows with the degrees of freedom. 
# For example, notice the difference between:

1 - pt(3,df=3)
1 - pt(3,df=15)
1 - pt(3,df=30)
1 - pnorm(3)

pval <- 2 * (1-pnorm(tstat))
print(pval) 

result <- t.test(X, Y)
result$p.value


  
