library(dplyr)
# Code is avaialbe at :
# http://genomicsclass.github.io/book/pages/random_variables.html


dir <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/"
filename <- "femaleMiceWeights.csv"
url <- paste0(dir, filename)
dat <- read.csv(url)

control <- filter(dat,Diet=="chow") %>%   dplyr::select(Bodyweight) %>% unlist
treatment <- filter(dat,Diet=="hf") %>% dplyr::select(Bodyweight) %>% unlist

# Many scientific statements like a high-fat diet makes mice heavier, the comparison is made on the average.

print( mean(treatment) )
print( mean(control) )

# Many scientific statements like a high-fat diet makes mice heavier, the comparison is made on the average.
obsdiff <- mean(treatment) - mean(control)
print(obsdiff)

# Random Variables 

# Imagine that we actually have the weight of all control female mice and can upload them to R. 
# In Statistics, we refer to this as the population. 
# These are all the control mice available from which we sampled 24. 
# Note that in practice we do not have access to the population. 
# We have a special data set that we are using here to illustrate concepts.

library(downloader)
set.seed(1)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- "femaleControlsPopulation.csv"
if (!file.exists(filename)) download(url,destfile=filename)

population <- read.csv(filename)
population <- unlist(population) # turn it into a numeric

# Let’s sample 12 mice three times and see how the average changes.

control <- sample(population,12)
mean(control)

# Doing this several times the average varies with the new set of random controls.
# So this is the random variable 
# We can continue to do this repeatedly and start learning something about the distribution of this random variable.

population_mean <- mean(population)
mean_5_sample <- mean(sample(population,5))
  
abs(population_mean-mean_5_sample)

set.seed(5)
population_mean <- mean(population)
mean_5_sample <- mean(sample(population,5))

abs(population_mean-mean_5_sample)

