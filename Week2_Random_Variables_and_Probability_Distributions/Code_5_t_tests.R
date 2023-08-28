# Statistical Inference
# Not measuring all the population but a sample, so we infer the value for the whole population
# Information Here: http://genomicsclass.github.io/book/pages/populations_and_samples.html

library(downloader)
library(dplyr)
library(rafalib)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- "mice_pheno.csv"
download(url,destfile=filename)
dat <- read.csv(filename)
dat <- na.omit( dat )

print ("x")
x <- filter(dat,Sex == "F" & Diet == "chow") %>%  select(Bodyweight) %>% unlist
length(x)
mean(x)
popsd(x)

print("y")
y <- filter(dat,Sex == "F" & Diet == "hf") %>% select(Bodyweight) %>% unlist
length(y)
mean(y)
popsd(y)


set.seed(2)
X <- sample(x,25)
mean(X)
set.seed(2)
Y <- sample(y,25)
mean(Y)

abs((mean(y)-mean(x))-(mean(Y)-mean(X)))
# We want to know how close is the sample average to the population average, to answer we use the
# central limit theorem.
