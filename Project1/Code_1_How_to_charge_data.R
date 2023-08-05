library(downloader) 
library(dplyr)

# Use of downloader library
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv" 
download(url, destfile=filename)

dat <- read.csv("femaleMiceWeights.csv")
hf_table <- dat[dat$Diet == 'hf', ]
mean_hf <- mean(hf_table$Bodyweight)

# Using dplyr we can do the same as above to isolate from the dataset dat new data
controls <- filter(dat, Diet=="chow")
controls<- select(controls, Bodyweight)
unlist(controls)

# The above can be done in one line (using %>% pipe)
controls <- filter(dat, Diet=='chow') %>% select(Bodyweight) %>% unlist
