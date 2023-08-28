RNGkind("Mersenne-Twister", "Inversion", "Rejection")

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )


# 1. 
# What proportion of these 1,000 averages are more than 1 gram away from the average of x ?
set.seed(1)

n <- 1000
sampling <- vector("numeric",n)
for (i in 1:n) {
  random_x <- sample(x,50)
  sampling[i] <-  mean(random_x)
}

hist(sampling) ##take a look
mean( abs( sampling - mean(x) ) > 1)

# 2.
library(gapminder)
library(dplyr)
data(gapminder)
head(gapminder)

countries_1952 <- filter(gapminder,year == 1952) %>%  select(lifeExp) %>% unlist

# What is the proportion of countries in 1952 that have a life expectancy between 40 and 60 years?
# Hint: this is the proportion that have a life expectancy less than or equal to 60 years, minus 
# the proportion that have a life expectancy less than or equal to 40 years.
print (mean(countries_1952 <= 60) - mean(countries_1952 <= 40))

# 3.
# When we examined mouse weights, we found that our sample estimates for females were closer 
# to the population difference than with males. What is a possible explanation for this?
  
# Answer: The population variance of the females is smaller than that of the males; thus, 
# the sample variable has less variability.

# 4.
# Create qq-plots for the four populations: male/females on each of the two diets. 
# What is the best explanation for all these mouse weights being well approximated by 
# the normal distribution?

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- "mice_pheno.csv"
download(url,destfile=filename)
dat <- read.csv(filename)
dat <- na.omit( dat )

mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% dplyr::select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% dplyr::select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% dplyr::select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% dplyr::select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)

# This just happens to be how nature behaves in this particular case. 
# Perhaps the result of many biological factors averaging out.
