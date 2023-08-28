library(downloader)
library(dplyr)

url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)

data <- read.csv("msleep_ggplot2.csv")

# Class of the data
print (class(data))

# How many animals in the table are primates
print (sapply(data, unique)) # This shows that the col order have the value primates so we can filter by the primates in this row

primates <- filter(data, order =='Primates')
print (nrow(primates))
print (class(primates))

# Extracting the sleep (total) for the primates

sleep_total <- filter(data, order =='Primates') %>% select(sleep_total)
print (class(sleep_total))

# Computing the mean of the sleep (total) for the primates

sleep_total_mean <- mean(filter(data, order =='Primates') %>% select(sleep_total) %>% unlist)
print (sleep_total_mean)


# Computing the mean of the sleep (total) for the primates using summarize

primates %>% summarise(mean_sleep=mean(sleep_total))









